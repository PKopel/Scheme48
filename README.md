# Scheme48
Scheme interpreter based on ["Write Yourself a Scheme in 48 Hours"](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours), with changes
including:
* use of RIO instead of standard prelude
* use of Text instead of String
* addition of quasiquoter 

## Execute  

* Run `stack exec s48` to run repl
* Run `stack install ` to install `s48` executable on your machine

To exit the repl type `:q` or `:quit`.

### Options
* `--version`: show version of repl
* `--help`: show options with short descriptions
* `-v | --verbose`: run repl with verbose output
* `-l | --load FILE`: on start execute contents of FILE  

## Dependencies
* **[RIO](https://hackage.haskell.org/package/rio)**
* **[attoparsec](https://hackage.haskell.org/package/attoparsec)**
* **[Haskeline](https://hackage.haskell.org/package/haskeline)**
* **[pretty-terminal](https://github.com/loganmac/pretty-terminal)**
* **[hspec](https://hspec.github.io)**
* **[QuickCheck](https://hackage.haskell.org/package/QuickCheck)**
* **[SYB](https://hackage.haskell.org/package/syb)**

## Author

* **[Paweł Kopel](https://github.com/PKopel)**