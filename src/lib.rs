#![cfg(target_os = "illumos")]
#![allow(unused)]

use anyhow::{bail, Result};
use libc::c_void;
use libc::time_t;
use num_enum::TryFromPrimitive;
use std::collections::BTreeMap;
use std::convert::TryFrom;
use std::ffi::{CStr, CString};
use std::iter::Iterator;
use std::os::raw::{c_char, c_int, c_uint};
use std::os::unix::ffi::OsStrExt;
use std::path::Path;
use std::slice::from_raw_parts;
use std::slice::from_raw_parts_mut;
use std::time::{Duration, SystemTime, UNIX_EPOCH};

enum DiNode {}
enum DiProp {}

const DI_NODE_NIL: *mut DiNode = std::ptr::null_mut();
const DI_PROP_NIL: *mut DiProp = std::ptr::null_mut();

const DIIOC: c_uint = 0xDF << 8;
const DINFOSUBTREE: c_uint = DIIOC | 0x01; /* include subtree */
const DINFOMINOR: c_uint = DIIOC | 0x02; /* include minor data */
const DINFOPROP: c_uint = DIIOC | 0x04; /* include properties */
const DINFOPATH: c_uint = DIIOC | 0x08; /* include multipath node data */
const DINFOLYR: c_uint = DIIOC | 0x40; /* include device layering data */
const DINFOHP: c_uint = DIIOC | 0x400000; /* include hotplug info (?) */

const DINFOCPYONE: c_uint = DIIOC; /* just a single node */
const DINFOCPYALL: c_uint = DINFOSUBTREE | DINFOPROP | DINFOMINOR;

const DI_PROP_TYPE_STRING: c_int = 2;

#[derive(Debug, Eq, PartialEq, TryFromPrimitive)]
#[repr(i32)]
pub enum PropType {
    Boolean = 0,
    Int32 = 1,
    String = 2,
    Byte = 3,
    Unknown = 4,
    Undefined = 5,
    Int64 = 6,
}

#[link(name = "devinfo")]
extern "C" {
    fn di_init(phys_path: *const c_char, flag: c_uint) -> *mut DiNode;
    fn di_fini(root: *mut DiNode);

    fn di_drv_first_node(drv_name: *const c_char, root: *mut DiNode) -> *mut DiNode;
    fn di_drv_next_node(node: *mut DiNode) -> *mut DiNode;

    fn di_parent_node(node: *mut DiNode) -> *mut DiNode;
    fn di_sibling_node(node: *mut DiNode) -> *mut DiNode;
    fn di_child_node(node: *mut DiNode) -> *mut DiNode;

    fn di_node_name(node: *mut DiNode) -> *const c_char;
    fn di_driver_name(node: *mut DiNode) -> *const c_char;
    fn di_instance(node: *mut DiNode) -> c_int;

    fn di_prop_next(node: *mut DiNode, prop: *mut DiProp) -> *mut DiProp;

    fn di_prop_name(prop: *mut DiProp) -> *const c_char;
    fn di_prop_type(prop: *mut DiProp) -> c_int;

    fn di_prop_strings(prop: *mut DiProp, data: *const *const c_char) -> c_int;
    fn di_prop_ints(prop: *mut DiProp, data: *const *const c_int) -> c_int;
    fn di_prop_int64(prop: *mut DiProp, data: *const *const i64) -> c_int;

    fn di_devfs_path(node: *mut DiNode) -> *mut c_char;
    fn di_devfs_path_free(path_buf: *mut c_char);
}

pub struct DevInfo {
    root: *mut DiNode,
}

impl Drop for DevInfo {
    fn drop(&mut self) {
        unsafe { di_fini(self.root) };
    }
}

fn string_props(node: *mut DiNode) -> BTreeMap<String, String> {
    let mut out = BTreeMap::new();
    let mut prop = DI_PROP_NIL;
    loop {
        prop = unsafe { di_prop_next(node, prop) };
        if prop == DI_PROP_NIL {
            break;
        }

        if unsafe { di_prop_type(prop) } != DI_PROP_TYPE_STRING {
            continue;
        }

        let mut data = std::ptr::null();
        let vals = unsafe { di_prop_strings(prop, &mut data) };
        if vals != 1 {
            continue;
        }

        let name = unsafe { CStr::from_ptr(di_prop_name(prop)) }.to_string_lossy();
        let val = unsafe { CStr::from_ptr(data) }.to_string_lossy();

        out.insert(name.to_string(), val.to_string());
    }
    out
}

impl DevInfo {
    pub fn new_path<P: AsRef<Path>>(p: P) -> Result<Self> {
        let path = CString::new(p.as_ref().as_os_str().as_bytes()).unwrap();

        let root = unsafe { di_init(path.as_ptr(), DINFOCPYALL) };
        if root == DI_NODE_NIL {
            let e = std::io::Error::last_os_error();
            bail!("di_init: {}", e);
        }

        Ok(DevInfo { root })
    }

    pub fn new() -> Result<Self> {
        Self::new_path("/")
    }

    pub fn walk_driver(&mut self, name: &str) -> DriverWalk {
        DriverWalk {
            parent: self,
            driver: name.to_string(),
            node: DI_NODE_NIL,
            fin: false,
        }
    }

    pub fn walk_node(&mut self) -> NodeWalk {
        NodeWalk {
            parent: self,
            node: DI_NODE_NIL,
            fin: false,
            skip_children: false,
        }
    }
}

pub struct NodeWalk<'w> {
    parent: &'w DevInfo,
    node: *mut DiNode,
    fin: bool,
    skip_children: bool,
}

impl<'a> NodeWalk<'a> {
    pub fn skip_children(&mut self) {
        self.skip_children = true;
    }
}

impl<'a> Iterator for NodeWalk<'a> {
    type Item = Result<Node<'a>>;

    fn next(&mut self) -> Option<Result<Node<'a>>> {
        if self.fin {
            return None;
        }

        if self.node == DI_NODE_NIL {
            /*
             * Visit the root node first.
             */
            self.node = self.parent.root;
            return Some(Ok(Node {
                parent: self.parent,
                node: self.node,
            }));
        }

        if self.skip_children {
            /*
             * We have been asked to prune children of the most recent node from
             * the walk.
             */
            self.skip_children = false;
        } else {
            /*
             * Does this node have children?
             */
            let child = unsafe { di_child_node(self.node) };
            if child != DI_NODE_NIL {
                /*
                 * Yes.  Visit the first child.
                 */
                self.node = child;
                return Some(Ok(Node {
                    parent: self.parent,
                    node: self.node,
                }));
            }
        }

        /*
         * No children of this node.  Try the next sibling.
         */
        let sib = unsafe { di_sibling_node(self.node) };
        if sib != DI_NODE_NIL {
            /*
             * Visit this sibling.
             */
            self.node = sib;
            return Some(Ok(Node {
                parent: self.parent,
                node: self.node,
            }));
        }

        /*
         * No siblings at this level.  Walk up until we find a sibling or the
         * root.
         */
        loop {
            self.node = unsafe { di_parent_node(self.node) };
            if self.node == DI_NODE_NIL {
                self.fin = true;
                return None;
            }

            let sib = unsafe { di_sibling_node(self.node) };
            if sib != DI_NODE_NIL {
                /*
                 * Visit this node.
                 */
                self.node = sib;
                return Some(Ok(Node {
                    parent: self.parent,
                    node: self.node,
                }));
            }
        }
    }
}

pub struct DriverWalk<'w> {
    parent: &'w DevInfo,
    driver: String,
    node: *mut DiNode,
    fin: bool,
}

pub struct Node<'n> {
    parent: &'n DevInfo,
    node: *mut DiNode,
}

impl<'a> Iterator for DriverWalk<'a> {
    type Item = Result<Node<'a>>;

    fn next(&mut self) -> Option<Result<Node<'a>>> {
        if self.fin {
            return None;
        }

        self.node = if self.node == DI_NODE_NIL {
            let driver = CString::new(self.driver.as_bytes()).unwrap();
            unsafe { di_drv_first_node(driver.as_ptr(), self.parent.root) }
        } else {
            unsafe { di_drv_next_node(self.node) }
        };

        if self.node == DI_NODE_NIL {
            self.fin = true;
            return None;
        }

        Some(Ok(Node {
            parent: self.parent,
            node: self.node,
        }))
    }
}

impl<'a> Node<'a> {
    pub fn parent(&self) -> Option<Node<'a>> {
        let node = unsafe { di_parent_node(self.node) };
        if node == DI_NODE_NIL {
            return None;
        }

        Some(Node {
            parent: self.parent,
            node,
        })
    }

    pub fn node_name(&self) -> String {
        unsafe { CStr::from_ptr(di_node_name(self.node)) }
            .to_string_lossy()
            .to_string()
    }

    pub fn driver_name(&self) -> Option<String> {
        let v = unsafe { di_driver_name(self.node) };
        if v.is_null() {
            None
        } else {
            Some(unsafe { CStr::from_ptr(v) }.to_string_lossy().to_string())
        }
    }

    pub fn instance(&self) -> Option<i32> {
        let v = unsafe { di_instance(self.node) };
        if v == -1 {
            None
        } else {
            Some(v)
        }
    }

    pub fn devfs_path(&self) -> Result<String> {
        let p = unsafe { di_devfs_path(self.node) };
        if p.is_null() {
            let e = std::io::Error::last_os_error();
            bail!("di_devfs_path failed: {}", e);
        }

        let cs = unsafe { CStr::from_ptr(p) };
        let s = cs.to_str().unwrap().to_string();
        unsafe { di_devfs_path_free(p) };
        Ok(s)
    }

    pub fn props(&self) -> PropertyWalk {
        PropertyWalk {
            parent: self.parent,
            node: self.node,
            prop: DI_PROP_NIL,
            fin: false,
        }
    }

    pub fn string_props(&self) -> BTreeMap<String, String> {
        string_props(self.node)
    }

    pub fn depth(&self) -> u32 {
        let mut d = 0;
        let mut n = self.node;
        loop {
            if n == DI_NODE_NIL {
                break;
            }
            n = unsafe { di_parent_node(n) };
            d += 1;
        }
        d
    }
}

pub struct PropertyWalk<'p> {
    parent: &'p DevInfo,
    node: *mut DiNode,
    prop: *mut DiProp,
    fin: bool,
}

impl<'a> Iterator for PropertyWalk<'a> {
    type Item = Result<Property<'a>>;

    fn next(&mut self) -> Option<Result<Property<'a>>> {
        if self.fin {
            return None;
        }

        self.prop = unsafe { di_prop_next(self.node, self.prop) };
        if self.prop == DI_PROP_NIL {
            self.fin = true;
            return None;
        }

        Some(Ok(Property {
            parent: self.parent,
            prop: self.prop,
        }))
    }
}

pub struct Property<'p> {
    parent: &'p DevInfo,
    prop: *mut DiProp,
}

impl Property<'_> {
    pub fn name(&self) -> String {
        unsafe { CStr::from_ptr(di_prop_name(self.prop)) }
            .to_string_lossy()
            .to_string()
    }

    pub fn value_type(&self) -> PropType {
        PropType::try_from(unsafe { di_prop_type(self.prop) }).unwrap()
    }

    pub fn as_i64(&self) -> Option<i64> {
        match self.value_type() {
            PropType::Int64 => {
                let mut data: *const i64 = std::ptr::null();
                let n = unsafe { di_prop_int64(self.prop, &mut data) };
                if n >= 1 {
                    Some(unsafe { *data })
                } else {
                    None
                }
            }
            PropType::Int32 => self.as_i32().map(|n| n as i64),
            _ => None,
        }
    }

    pub fn as_i32(&self) -> Option<i32> {
        match self.value_type() {
            PropType::Int32 => {
                let mut data: *const c_int = std::ptr::null();
                let n = unsafe { di_prop_ints(self.prop, &mut data) };
                if n >= 1 {
                    Some(unsafe { *data })
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    pub fn to_str(&self) -> Option<String> {
        self.as_cstr()?.to_str().ok().map(|s| s.to_string())
    }

    pub fn as_cstr(&self) -> Option<&CStr> {
        match self.value_type() {
            PropType::String => {
                let mut data: *const c_char = std::ptr::null();
                let n = unsafe { di_prop_strings(self.prop, &mut data) };
                if n >= 1 {
                    Some(unsafe { CStr::from_ptr(data) })
                } else {
                    None
                }
            }
            _ => None,
        }
    }
}

impl std::fmt::Display for Property<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.value_type() {
            PropType::Int32 => write!(f, "{}", self.as_i32().unwrap()),
            PropType::Int64 => write!(f, "{}", self.as_i64().unwrap()),
            PropType::String => {
                write!(f, "{}", self.as_cstr().unwrap().to_str().unwrap())
            }
            _ => write!(f, "<?Property>"),
        }
    }
}
