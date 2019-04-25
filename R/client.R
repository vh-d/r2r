#' Connect to a remote server
#'
#' @param address host address (defaults to 'localhost')
#' @param port port
#'
#' @return
#' @export
connect <- function(
  address = "tcp://localhost",
  port    = 5555, 
  verbose = TRUE
){

  stopifnot(requireNamespace("pbdZMQ", quietly = TRUE))

  if (isTRUE(verbose)) message("Connecting to ", paste0(address, ":", port, "..."), appendLF = FALSE)

  context = pbdZMQ::init.context()
  socket  = pbdZMQ::init.socket(context, "ZMQ_REQ")

  pbdZMQ::connect.socket(
    socket = socket,
    address = paste0(address, ":", port)
  )

  if (test_remote(socket = socket)) message("ok.") else warning("Testing connection failed.")

  return(invisible(socket))
}


#' Destroy socket connection
#'
#' @param socket socket connection
#'
#' @export
disconnect <- function(socket = .r2r_socket) {
  pbdZMQ::zmq.close(socket)
}


#' @export
#' @rdname connect
save_socket <- function(socket){
    .GlobalEnv$.r2r_socket <- socket
}


#' @param ... \code{connect_global} passes args to \code{connect} 
#'
#' @export
#' @rdname connect
connect_global <- function(...){
  save_socket(
    connect(...)
  )
}

#' Remote query execution
#'
#' @param command function to be called
#' @param args_remote list of arguments that are passed as non-evaluated expression over the connection and evaluated on the server
#' @param args_local list of arguments that are passed evaluated (e.g. data)
#' @param data named list with data to be send to remote session for execution
#' @param quote should be set TRUE when command is a character value
#' @param socket connection
#'
#' @return whatever command call returns
#' @export
do.call_remote <- function(
  what,
  args_remote = NULL,
  args_local  = NULL,
  data        = list(),
  quote       = FALSE,
  socket      = .r2r_socket
) {

  msg_push <-
    list(
      command     = "call",
      what        = if (quote) substitute(what) else as.character(what)[1],
      args_local  = args_local,
      args_remote = substitute(args_remote),
      data        = data,
      quoted      = quote
    )

  # send message with data
  pbdZMQ::send.socket(
    socket = socket,
    data   = msg_push
  )

  # receive answer
  msg_pull <- pbdZMQ::receive.socket(socket = socket)

  if (!is.null(msg_pull$error))   stop(msg_pull$error)
  if (!is.null(msg_pull$warning)) warning(msg_pull$warning)
  if (!is.null(msg_pull$message)) message(msg_pull$message)

  return(msg_pull$result)

}


#' Evaluate expression on remote R session
#'
#' @param expr    expression to be evaluated
#' @param data    optional list with data
#' @param socket
#'
#' @return
#' @export
#'
#' @examples
#' r_eval(1+1)
eval_remote <- function(
  expr,
  data   = NULL,
  global = FALSE,
  socket = .r2r_socket
) {

  msg_push <-
    list(
      command     = "eval",
      expr        = substitute(expr),
      data        = data,
      global      = global
    )

  # send message with data
  pbdZMQ::send.socket(
    socket = socket,
    data   = msg_push
  )

  # receive answer
  msg_pull <- pbdZMQ::receive.socket(socket = socket)

  if (!is.null(msg_pull$error))   stop(msg_pull$error)
  if (!is.null(msg_pull$warning)) warning(msg_pull$warning)
  if (!is.null(msg_pull$message)) message(msg_pull$message)

  return(msg_pull$result)

}


test_remote <- function(socket = .r2r_socket) {
  pbdZMQ::send.socket(data = list(command = "test"), socket = socket)
  msg_pull <- pbdZMQ::receive.socket(socket = socket)

  return(is.character(msg_pull) && msg_pull == "ok")
}

#' Stop server
#'
#' @param socket
#'
#' @return
#' @rdname connect
#' @export
stop_remote <- function(
  socket = .r2r_socket
) {
  pbdZMQ::send.socket(data = list(command = "stop"), socket = socket)
  msg_pull <- pbdZMQ::receive.socket(socket = socket)

  if (is.character(msg_pull) && msg_pull == "ok") {
    pbdZMQ::zmq.close(socket = socket)
    return(TRUE)
  } else {
    return(FALSE)
  }
}


#' Break server loop
#'
#' @param socket
#'
#' @return
# @export
break_remote <- function(socket = .r2r_socket) {
  pbdZMQ::send.socket(data = list(command = "break"), socket = socket)

  msg_pull <- pbdZMQ::receive.socket(socket = socket)
  return(is.character(msg_pull) && msg_pull == "ok")
}


#' Start a new R session with a running r2r server
#'
#' @param address host address
#' @param port    port
#' @param Rscript path to Rscript binary
#' @param args    further arguments to Rscript command (e.g. "--vanilla")
#' @param stdout  see \code{\link{system2}}
#' @param invisible logical; visibility of the new session window
#'
#' @return socket connection (invisible)
#' @export
#' @rdname connect
#' @examples
#' #TBA
start_server_locally <- function(
  address   = "tcp://localhost",
  port      = pbdZMQ::random_open_port(),
  Rscript   = NULL,
  args      = NULL, # --vanilla
  stdout    = NULL,
  invisible = FALSE,
  wait      = FALSE,
  global    = TRUE,
  debug     = FALSE
) {
  
  if (!length(Rscript) || is.na(Rscript)) {
    Rscript <- file.path(R.home("bin"), "Rscript")
    if (.Platform$OS.type == "windows") Rscript <- paste0(Rscript, ".exe")
  }
  
  if (isTRUE(debug)) cat("Starting new process: ", Rscript, "\n")
  
  stopifnot(file.exists(Rscript))
  
  cmd <-
    sprintf(
      '"library(r2r);r2r::server(port = %i, debug = %s)"', as.integer(port), as.character(debug)
    )

  if (.Platform$OS.type == "windows") {
    system2(
      command   = Rscript,
      args      = c(args, "-e", cmd),
      stdout    = stdout,
      invisible = invisible,
      wait      = wait
    )
  } else {
    system2(
      command   = Rscript,
      args      = c(args, "-e", cmd),
      stdout    = stdout,
      wait      = wait
    )
  }
  
  socket <-
    r2r::connect(
      address = address,
      port    = as.integer(port)
    )

  if (global) r2r::save_socket(socket)

  return(invisible(socket))
}


#' @return logical
#' @rdname connect
#' @export
stop_server <- stop_remote




