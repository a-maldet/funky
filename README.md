
# funky

<!-- badges: start -->

[![GitHub last
commit](https://img.shields.io/github/last-commit/a-maldet/funky.svg?logo=github)](https://github.com/a-maldet/funky/commits/master)
[![GitHub code size in
bytes](https://img.shields.io/github/languages/code-size/a-maldet/funky.svg?logo=github)](https://github.com/a-maldet/funky)
<!-- badges: end -->

This **R package** makes **functional programming** easy and helps
avoiding **memory leaks** when using **closures** in R.

> Functional Programming In R Should Be FunKy

## Installation

``` r
# Install development version from GitHub
devtools::install_github('a-maldet/funky', build_opts = NULL)
```

## Usage

``` r
library(funky)
```

## Available functions

  - `restrict_fn_env()`: Restrict the environment of a function to the
    **optimal scope** and reduce its ancestry tree to the minimum. This
    is very important, because otherwise the functions created inside of
    a **closure** reference the **entire environment ancestry tree**.
    This may cause **memory leaks** and slow down your code.
  - `eval_closure()`: A wrapper for `restrict_fn_env()`, which allows
    you to evaluate an expression inside of a closure, which has the
    **optimal scope**. This is important to **avoid memory leaks** when
    using **closures** in R.
  - `get_call_args()`: Returns a named list holding all argument values
    of the current function call (including non-overwritten default
    values).
  - `set_fn_defaults()`: Set the defaults for a given function. Can be
    useful for customizing various functions.
  - `get_defaults()`: Return all function arguments including their
    default values for a given function.

## Remark:

If you like this package, please give me a star on github:
<https://github.com/a-maldet/funky>

## License

GPL-3
