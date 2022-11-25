use anyhow::Result;
use illumos_devinfo::*;
use std::collections::BTreeMap;

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

fn main() -> Result<()> {
    let mut device_info = DevInfo::new()?;
    let mut node_walker = device_info.walk_node();

    let mut device_descriptions = BTreeMap::new();

    while let Some(mut node) = node_walker.next().transpose()? {
        if let Some(driver_name) = node.driver_name() {
            if driver_name == "blkdev" {
                let devfs_path = node.devfs_path()?;
                while let Some(parent) = node.parent() {
                    node = parent;
                    if let Some(Ok(slot_prop)) = node.props().into_iter().find(|prop| {
                        if let Ok(prop) = prop {
                            prop.name() == "physical-slot#".to_string()
                        } else { false }
                    }) {
                        let slot = slot_prop.as_i64().expect("Expected i64");
                        let device = slot_to_device(slot).unwrap_or("Not found");

                        device_descriptions.insert(
                            slot,
                            format!("{slot:>4}\t{device:>6}\t{devfs_path}")
                        );
                        break;
                    }
                }
            }
        }
    }

    println!("Slot\tDevice\tDevfs Path");
    for device in device_descriptions.values() {
        println!("{device}");
    }

    Ok(())
}
