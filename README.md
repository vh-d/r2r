# r2r

## Installation

```
devtools::install_github("vh-d/r2r")
```


## Examples

First, start a server R session and run

``` r
library(r2r)
r2r::server()
```

Next, start a client R session and run:

```
library(r2r)
r2r::eval_remote(1+1)
r2r::do.call_remote("print", "Hello world!")
r2r::do.call_remote(print, "Hello world!", quote = TRUE)

r2r::eval_remote(a+b, data = list(a = 1, b = 2))
r2r::eval_remote(a <- 1, global = TRUE)
r2r::eval_remote(a+b, data = list(b = 20))
```

