# rasch
Chess engine in Rust

Since I'm learning Rust I though I could re-implement my Frege chess engine https://github.com/Ingo60/frech in Rust.

This was a good idea, because I learned a lot. The result is a program that I couldn't beat myself anymore. Especially when pondering is on.

### Usage

You need a Chess frontend that speaks Chess Engine Communication Protocol, such as xboard (https://www.gnu.org/software/xboard/engine-intf.html).

Then, for example:

```
cargo build --release
xboard -fSAN -fcp "target/release/rasch negamax"
```
