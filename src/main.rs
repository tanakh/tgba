use std::path::PathBuf;

#[argopt::cmd]
fn main(bios: PathBuf, rom: PathBuf) -> anyhow::Result<()> {
    tgba::run(&bios, &rom)
}
