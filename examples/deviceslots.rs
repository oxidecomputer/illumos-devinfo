use anyhow::{anyhow, Result};
use clap::{Parser, ValueEnum};
use illumos_devinfo::*;
use std::collections::BTreeMap;
use std::path::PathBuf;

fn slot_to_device(slot: i64) -> Option<&'static str> {
    match slot {
        0x00 => Some("u.2"),
        0x01 => Some("u.2"),
        0x02 => Some("u.2"),
        0x03 => Some("u.2"),
        0x04 => Some("u.2"),
        0x05 => Some("u.2"),
        0x06 => Some("u.2"),
        0x07 => Some("u.2"),
        0x08 => Some("u.2"),
        0x09 => Some("u.2"),
        0x10 => Some("nic"),
        0x11 => Some("m.2"),
        0x12 => Some("m.2"),
        0x13 => Some("sidecar"),
        _ => None,
    }
}

fn get_dev_path(node: &Node<'_>) -> Result<Option<PathBuf>> {
    let mut wm = node.minors();
    while let Some(m) = wm.next().transpose()? {
        if m.name() != "wd" {
            continue;
        }
        let links = {
            match DevLinks::new(true) {
                Ok(links) => links,
                Err(_) => DevLinks::new(false)?,
            }
        };
        let devfs_path = m.devfs_path()?;

        let paths = links
            .links_for_path(&devfs_path)?
            .into_iter()
            .filter(|l| {
                l.linktype() == DevLinkType::Primary
                    && l.path()
                        .file_name()
                        .map(|f| f.to_string_lossy().ends_with("d0"))
                        .unwrap_or(false)
            })
            .collect::<Vec<_>>();

        if paths.is_empty() {
            return Err(anyhow!("no links for {}", devfs_path));
        }

        if paths.len() != 1 {
            return Err(anyhow!("got weird paths {:?} for {}", paths, devfs_path));
        }
        return Ok(Some(paths[0].path().to_path_buf()));
    }
    Ok(None)
}

#[derive(Copy, Clone, PartialEq, Eq, Debug, ValueEnum)]
enum PathFlavor {
    /// Emit the path within "/devices"
    Devfs,
    /// Emit the path within "/dev"
    Dev,
}

/// Progam to identify disks, their types, and their paths.
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// The type of path to emit
    #[arg(short, long, value_enum, default_value_t = PathFlavor::Devfs)]
    flavor: PathFlavor,
}

fn main() -> Result<()> {
    let args = Args::parse();

    let mut device_info = {
        match DevInfo::new_force_load() {
            Ok(di) => di,
            Err(_) => DevInfo::new()?,
        }
    };
    let mut node_walker = device_info.walk_driver("blkdev");

    let mut device_descriptions = BTreeMap::new();

    while let Some(mut node) = node_walker.next().transpose()? {
        if let Some(driver_name) = node.driver_name() {
            if driver_name == "blkdev" {
                let dev_path = get_dev_path(&node)?;
                let devfs_path = PathBuf::from(format!("/devices{}", node.devfs_path()?));
                while let Ok(Some(parent)) = node.parent() {
                    node = parent;
                    if let Some(Ok(slot_prop)) = node.props().into_iter().find(|prop| {
                        if let Ok(prop) = prop {
                            prop.name() == "physical-slot#".to_string()
                        } else {
                            false
                        }
                    }) {
                        let slot = slot_prop.as_i64().expect("Expected i64");
                        let device = slot_to_device(slot).unwrap_or("Not found");

                        let path = match args.flavor {
                            PathFlavor::Devfs => devfs_path.to_string_lossy(),
                            PathFlavor::Dev => dev_path
                                .map(|p| String::from(p.to_string_lossy()))
                                .unwrap_or(String::from("none"))
                                .into(),
                        };

                        device_descriptions.insert(slot, format!("{slot:>4}\t{device:>6}\t{path}"));
                        break;
                    }
                }
            }
        }
    }

    println!("Slot\tDevice\tPath");
    for device in device_descriptions.values() {
        println!("{device}");
    }

    Ok(())
}
