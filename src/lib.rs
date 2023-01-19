#![cfg(target_os = "illumos")]
#![allow(unused_imports)]
#![allow(unused)]

use anyhow::{bail, Result};
use libc::___errno;
use libc::c_void;
use libc::time_t;
use libc::ENXIO;
use num_enum::TryFromPrimitive;
use std::collections::BTreeMap;
use std::convert::TryFrom;
use std::ffi::OsStr;
use std::ffi::{CStr, CString};
use std::iter::Iterator;
use std::os::raw::{c_char, c_int, c_uchar, c_uint};
use std::os::unix::ffi::OsStrExt;
use std::path::{Path, PathBuf};
use std::slice::from_raw_parts;
use std::slice::from_raw_parts_mut;
use std::time::{Duration, SystemTime, UNIX_EPOCH};

enum DiNode {}
enum DiProp {}
enum DiMinor {}
enum DiDevlinkHandle {}
enum DiDevlink {}

const DI_NODE_NIL: *mut DiNode = std::ptr::null_mut();
const DI_PROP_NIL: *mut DiProp = std::ptr::null_mut();
const DI_MINOR_NIL: *mut DiMinor = std::ptr::null_mut();
const DI_LINK_NIL: *mut DiDevlinkHandle = std::ptr::null_mut();

const DIIOC: c_uint = 0xDF << 8;
const DINFOSUBTREE: c_uint = DIIOC | 0x01; /* include subtree */
const DINFOMINOR: c_uint = DIIOC | 0x02; /* include minor data */
const DINFOPROP: c_uint = DIIOC | 0x04; /* include properties */
const DINFOPATH: c_uint = DIIOC | 0x08; /* include multipath node data */
const DINFOLYR: c_uint = DIIOC | 0x40; /* include device layering data */
const DINFOHP: c_uint = DIIOC | 0x400000; /* include hotplug info (?) */

const DINFOCPYONE: c_uint = DIIOC; /* just a single node */
const DINFOCPYALL: c_uint = DINFOSUBTREE | DINFOPROP | DINFOMINOR;

/*
 * These flags are Private:
 */
const DINFOPRIVDATA: c_uint = DIIOC | 0x10; /* include private data */
const DINFOFORCE: c_uint = DIIOC | 0x20; /* force load all drivers */
const DINFOCACHE: c_uint = DIIOC | 0x100000; /* use cached data  */
const DINFOCLEANUP: c_uint = DIIOC | 0x200000; /* cleanup /etc/devices files */

const DI_PROP_TYPE_STRING: c_int = 2;

const DI_MAKE_LINK: c_uint = 0x01;

const DI_PRIMARY_LINK: c_uint = 0x01;
const DI_SECONDARY_LINK: c_uint = 0x02;
const DI_LINK_TYPES: c_uint = 0x03;

const DI_WALK_CONTINUE: c_int = 0;
const DI_WALK_PRUNESIB: c_int = -1;
const DI_WALK_PRUNECHILD: c_int = -2;
const DI_WALK_TERMINATE: c_int = -3;

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
    fn di_prop_bytes(prop: *mut DiProp, data: *const *const c_uchar) -> c_int;
    fn di_prop_ints(prop: *mut DiProp, data: *const *const c_int) -> c_int;
    fn di_prop_int64(prop: *mut DiProp, data: *const *const i64) -> c_int;

    fn di_devfs_path(node: *mut DiNode) -> *mut c_char;
    fn di_devfs_minor_path(minor: *mut DiMinor) -> *mut c_char;
    fn di_devfs_path_free(path_buf: *mut c_char);

    fn di_minor_next(node: *mut DiNode, minor: *mut DiMinor) -> *mut DiMinor;
    fn di_minor_name(minor: *mut DiMinor) -> *const c_char;
    fn di_minor_nodetype(minor: *mut DiMinor) -> *const c_char;
    fn di_minor_spectype(minor: *mut DiMinor) -> c_int;

    fn di_devlink_init(name: *const c_char, flags: c_uint) -> *mut DiDevlinkHandle;
    fn di_devlink_fini(hdlp: *mut DiDevlinkHandle);

    fn di_devlink_walk(
        hdl: *mut DiDevlinkHandle,
        re: *const c_char,
        mpath: *const c_char,
        flags: c_uint,
        arg: *mut c_void,
        devlink_callback: unsafe extern "C" fn(*const DiDevlink, *mut c_void) -> c_int,
    ) -> c_int;

    fn di_devlink_path(devlink: *const DiDevlink) -> *const c_char;
    fn di_devlink_content(devlink: *const DiDevlink) -> *const c_char;
    fn di_devlink_type(devlink: *const DiDevlink) -> c_int;
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
    fn new_common<P: AsRef<Path>>(p: P, force_load: bool) -> Result<Self> {
        let path = CString::new(p.as_ref().as_os_str().as_bytes()).unwrap();
        let mut flags = DINFOCPYALL;
        if force_load {
            flags |= DINFOFORCE;
        }

        let root = unsafe { di_init(path.as_ptr(), flags) };
        if root == DI_NODE_NIL {
            let e = std::io::Error::last_os_error();
            bail!("di_init: {}", e);
        }

        Ok(DevInfo { root })
    }

    pub fn new_path<P: AsRef<Path>>(p: P) -> Result<Self> {
        Self::new_common(p, false)
    }

    pub fn new() -> Result<Self> {
        Self::new_common("/", false)
    }

    pub fn new_force_load() -> Result<Self> {
        Self::new_common("/", true)
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

#[derive(Clone)]
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

    pub fn minors(&self) -> MinorWalk {
        MinorWalk {
            parent: self.parent,
            node: self.node,
            minor: DI_MINOR_NIL,
            fin: false,
        }
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

    pub fn parent(&self) -> Result<Option<Node<'a>>> {
        let n = unsafe { di_parent_node(self.node) };
        if n == DI_NODE_NIL {
            if unsafe { *___errno() } == ENXIO {
                Ok(None)
            } else {
                bail!("{}", std::io::Error::last_os_error());
            }
        } else {
            Ok(Some(Node {
                parent: self.parent,
                node: n,
            }))
        }
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

    pub fn as_bytes(&self) -> Option<&[u8]> {
        match self.value_type() {
            PropType::Byte => {
                let mut data: *const c_uchar = std::ptr::null();
                let n = unsafe { di_prop_bytes(self.prop, &mut data) };
                if n >= 0 {
                    Some(unsafe { std::slice::from_raw_parts(data, n.try_into().unwrap()) })
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

pub struct MinorWalk<'p> {
    parent: &'p DevInfo,
    node: *mut DiNode,
    minor: *mut DiMinor,
    fin: bool,
}

impl<'a> Iterator for MinorWalk<'a> {
    type Item = Result<Minor<'a>>;

    fn next(&mut self) -> Option<Result<Minor<'a>>> {
        if self.fin {
            return None;
        }

        self.minor = unsafe { di_minor_next(self.node, self.minor) };
        if self.minor == DI_MINOR_NIL {
            self.fin = true;
            return None;
        }

        Some(Ok(Minor {
            parent: self.parent,
            minor: self.minor,
        }))
    }
}

pub struct Minor<'p> {
    parent: &'p DevInfo,
    minor: *mut DiMinor,
}

#[derive(Debug, PartialEq, Eq)]
pub enum SpecType {
    Char,
    Block,
}

impl<'a> Minor<'a> {
    pub fn name(&self) -> String {
        unsafe { CStr::from_ptr(di_minor_name(self.minor)) }
            .to_string_lossy()
            .to_string()
    }

    pub fn node_type(&self) -> String {
        unsafe { CStr::from_ptr(di_minor_nodetype(self.minor)) }
            .to_string_lossy()
            .to_string()
    }

    pub fn spec_type(&self) -> SpecType {
        match unsafe { di_minor_spectype(self.minor) as libc::mode_t } {
            libc::S_IFCHR => SpecType::Char,
            libc::S_IFBLK => SpecType::Block,
            other => panic!("unknown spectype 0x{:x}", other),
        }
    }

    pub fn devfs_path(&self) -> Result<String> {
        let p = unsafe { di_devfs_minor_path(self.minor) };
        if p.is_null() {
            let e = std::io::Error::last_os_error();
            bail!("di_devfs_minor_path failed: {}", e);
        }

        let cs = unsafe { CStr::from_ptr(p) };
        let s = cs.to_str().unwrap().to_string();
        unsafe { di_devfs_path_free(p) };
        Ok(s)
    }
}

pub struct DevLinks {
    handle: *mut DiDevlinkHandle,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DevLinkType {
    Primary,
    Secondary,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DevLink {
    path: PathBuf,
    content: PathBuf,
    linktype: DevLinkType,
}

impl DevLink {
    pub fn path(&self) -> &Path {
        &self.path
    }

    pub fn target(&self) -> &Path {
        &self.content
    }

    pub fn linktype(&self) -> DevLinkType {
        self.linktype
    }
}

extern "C" fn devlink_accumulate(link: *const DiDevlink, arg: *mut c_void) -> c_int {
    let path = unsafe { di_devlink_path(link) };
    let content = unsafe { di_devlink_content(link) };
    let ltype = unsafe { di_devlink_type(link) } as u32;
    if path.is_null()
        || content.is_null()
        || (ltype != DI_PRIMARY_LINK && ltype != DI_SECONDARY_LINK)
    {
        /*
         * XXX Report an error, probably?
         */
        return DI_WALK_CONTINUE;
    }

    let mut out = unsafe { Box::from_raw(arg as *mut Vec<DevLink>) };

    out.push(DevLink {
        path: PathBuf::from(OsStr::from_bytes(
            unsafe { CStr::from_ptr(path) }.to_bytes(),
        )),
        content: PathBuf::from(OsStr::from_bytes(
            unsafe { CStr::from_ptr(content) }.to_bytes(),
        )),
        linktype: match ltype {
            DI_PRIMARY_LINK => DevLinkType::Primary,
            DI_SECONDARY_LINK => DevLinkType::Secondary,
            other => panic!("what is link type 0x{:x}?", other),
        },
    });
    assert_eq!(Box::into_raw(out) as *mut c_void, arg);

    DI_WALK_CONTINUE
}

impl DevLinks {
    fn new_common(make_link: bool) -> Result<DevLinks> {
        let mut flags = 0;
        if make_link {
            flags |= DI_MAKE_LINK;
        }

        let handle = unsafe { di_devlink_init(std::ptr::null(), flags) };
        if handle == DI_LINK_NIL {
            let e = std::io::Error::last_os_error();
            bail!("di_devlink_init: {}", e);
        }

        Ok(DevLinks { handle })
    }

    pub fn new(make_link: bool) -> Result<Self> {
        Self::new_common(make_link)
    }

    pub fn links_for_path<P: AsRef<Path>>(&self, p: P) -> Result<Vec<DevLink>> {
        let mut out: Box<Vec<DevLink>> = Default::default();
        let arg = Box::into_raw(out);
        let mpath = CString::new(p.as_ref().as_os_str().as_bytes()).unwrap();

        let r = unsafe {
            di_devlink_walk(
                self.handle,
                std::ptr::null(),
                mpath.as_ptr(),
                0,
                arg as *mut c_void,
                devlink_accumulate,
            )
        };

        /*
         * Make sure we get our boxed argument back so that it will be freed:
         */
        let mut out = unsafe { Box::from_raw(arg) };

        if r != 0 {
            let e = std::io::Error::last_os_error();
            bail!("di_devlink_walk: {}", e);
        }

        Ok(out.to_vec())
    }
}

impl Drop for DevLinks {
    fn drop(&mut self) {
        unsafe { di_devlink_fini(self.handle) };
    }
}
