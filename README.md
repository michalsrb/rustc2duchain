# rustc2duchain

This is a tool that uses rustc's internal libraries to parse and analyze crate and then output information about it in a format that [kdev-rust](https://github.com/michalsrb/kdev-rust) can easily turn into internal [KDevelop](https://www.kdevelop.org/) representation.

This tool's linking to rustc libraries is hackish and is only temporary solution. In future this tool will be replaced with [Rust Language Server](https://github.com/rust-lang/rfcs/blob/master/text/1317-ide.md).

# Disclaimer

  * This is early experminent.
  * I am new to Rust.
  * Rustc internal libraries are unstable and are not meant to be used by external tools. This tool may stop buildling any time you update your rust compiler.

# Prerequisites

You need to have working cargo and nightly rustc installation.

# Build and installation

**Warning**: You must rebuild `rustc2duchain` every time you update your rust compiler!

Build `rustc2duchain` like any other binary crate:

```
git clone https://github.com/michalsrb/rustc2duchain.git
cd rustc2duchain
cargo build --release
```

This will build the `rustc2duchain` binary and place it at `./target/release/rustc2duchain` path relative to the repository. You can keep it in there and configure the `kdev-rust` plugin to use it from there. In that case you will also need to set path to the rustc libraries in your `kdev-rust` configuration.

Alternatively, if you use rustup, you can copy `rustc2duchain` in the appropriate bin directory of nightly toolchain. For example like this:

```
cp ./target/release/rustc2duchain $HOME/.multirust/toolchains/nightly-x86_64-unknown-linux-gnu/bin
```

In that case `kdev-rust` will locate `rustc2duchain` automatically and should work out of the box.

# Usage

This tool is meant to be used with the `kdev-rust` KDevelop plugin. See its [README](https://github.com/michalsrb/kdev-rust/blob/master/README.md) for details.

You can also test it by running it manually:

```
rustup run nightly rustc2duchain --debug </path/to/some/crate/main.rs>
```
