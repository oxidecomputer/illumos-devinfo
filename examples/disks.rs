use anyhow::Result;
use illumos_devinfo::*;

fn main() -> Result<()> {
    let mut di = DevInfo::new()?;

    let mut w = di.walk_node();
    while let Some(n) = w.next().transpose()? {
        let mut wm = n.minors();
        while let Some(m) = wm.next().transpose()? {
            /*
             * Disks will either have the DDI_NT_BLOCK node type, or one of the
             * more specific DDI_NT_BLOCK* subtypes (with a suffix after the
             * colon):
             */
            if m.node_type() != "ddi_block" && !m.node_type().starts_with("ddi_block:") {
                continue;
            }

            /*
             * Just look for raw (not block) disk devices.
             */
            if m.spec_type() != SpecType::Char {
                continue;
            }

            println!("{}: {}", m.node_type(), m.devfs_path()?);

            let links = DevLinks::new(false)?;
            for l in links.links_for_path(m.devfs_path()?)? {
                println!("    {:?}", l.path());
            }
        }
    }

    Ok(())
}
