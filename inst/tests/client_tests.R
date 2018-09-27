library(r2r)
r2r::connect_global()

# basic tests -------------------------------------------------------------
r2r::eval_remote(1+1)
r2r::do.call_remote("print", "Hello world!")

r2r::eval_remote(a+b, data = list(a = 1, b = 2))
r2r::eval_remote(a <- 1, global = TRUE)
r2r::eval_remote(a+b, data = list(b = 20))


#eval_remote(expr = library(package="data.table", logical.return = TRUE, verbose = TRUE), global = TRUE, socket = .r2r_socket)
