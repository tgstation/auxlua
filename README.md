# Auxlua

Auxlua is a library for integrating lua scripting into BYOND projects. Internally, it uses [Roblox's Luau](https://luau-lang.org/) to provide easy and reliable sandboxing to scripts.

## Dependencies

The [Rust] compiler:

1. Install the Rust compiler's dependencies (primarily the system linker):

   - Ubuntu: `sudo apt-get install gcc-multilib`
   - Windows (MSVC): [Build Tools for Visual Studio 2017][msvc]
   - Windows (GNU): No action required

1. Use [the Rust installer](https://rustup.rs/), or another Rust installation method,
   or run the following:

   ```sh
   curl https://sh.rustup.rs -sSfo rustup-init.sh
   chmod +x rustup-init.sh
   ./rustup-init.sh
   ```

1. Set the default compiler to **32-bit**:

   ```sh
   # Clone the `auxlua` repository to a directory of your choice
   git clone https://github.com/tgstation/auxlua.git
   # in the `auxlua` directory...
   cd auxlua
   # Linux
   rustup target add i686-unknown-linux-gnu
   # Windows
   rustup target add i686-pc-windows-msvc
   ```

## Compiling

The [Cargo] tool handles compilation, as well as automatically downloading and
compiling all Rust dependencies. The default configuration is suitable for
use with any BYOND project. To compile in release mode (recommended for
speed):

Linux:

```sh
export PKG_CONFIG_ALLOW_CROSS=1
cargo build --release --target i686-unknown-linux-gnu
# output: target/i686-unknown-linux-gnu/release/libauxlua.so
```

Windows:

```sh
cargo build --release --target i686-pc-windows-msvc
# output: target/i686-pc-windows-msvc/release/auxlua.dll
```

## Installing

The auxlua binary (`auxlua.dll` or `libauxlua.so`) should be placed in the root
of your repository next to your `.dmb`. There are alternative installation
locations, but this one is best supported.

Included in the folder `dmsrc` is the file `_hooks.dm`. To use auxlua, copy-paste this file into your project.

## Troubleshooting

You must build a 32-bit version of the library for it to be compatible with BYOND.

## License

Auxlua is licensed under [GNU AGPL v3](https://www.gnu.org/licenses/agpl-3.0.html).
