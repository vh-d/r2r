.do.call_remote <- function(msg, socket, debug) {
  # insert odbc connection into arguments
  if (debug) {
    message("\twhat",   as.character(msg$what))
    message("\tdata",   str(msg$data))
  }

  args_list <-
    c(
      list(),
      eval(msg$args_remote,
           envir = msg$data),
      msg$args_local
    )

  # template for outgoing message
  msg_push <-
    list(
      result  = NULL,
      error   = NULL,
      msg     = NULL
    )

  # main call
  tryCatch(
    msg_push$result <-
      do.call(
        what = if (msg$quoted) eval(msg$what) else msg$what,
        args = args_list
      ),

    error   = function(m){msg_push$error   <<- m;return(NULL)},
    message = function(m){msg_push$message <<- m;return(NULL)},
    warning = function(m){msg_push$warning <<- m;return(NULL)}
  )

  if (debug) {
    message("Sending:")
    message("\tresult:",  head(msg_push$result))
    message("\terror:",   head(msg_push$error))
    message("\twarning:", head(msg_push$warning))
    message("\tmessage:", head(msg_push$message))
  }

  if (debug) print(head(msg_push$result))

  if (debug) message("Sending results...", appendLF = FALSE)

  # send data to client
  pbdZMQ::send.socket(
    socket = socket,
    data   = msg_push
  )

  if (debug) message("ok.")

  return(NULL)
}


.eval_remote <- function(msg, socket, debug) {

  if (debug) {
    message("\texpr",   msg$expr)
    message("\tglobal", msg$global)
    message("\tdata",   str(msg$data))
  }

  # prepare answer
  msg_push <-
    list(
      result  = NULL,
      error   = NULL,
      msg     = NULL
    )

  # main call
  tryCatch(
    msg_push$result <-
      eval(
        expr   = msg$expr,
        envir  = if (msg$global) globalenv() else msg$data,
        enclos = if (msg$global) globalenv() else parent.frame(n = 2)
      ),

    error   = function(m){msg_push$error   <<- m;return(NULL)},
    message = function(m){msg_push$message <<- m;return(NULL)},
    warning = function(m){msg_push$warning <<- m;return(NULL)}
  )

  if (debug) {
    message("Sending:")
    message("\tresult:",  head(msg_push$result))
    message("\terror:",   head(msg_push$error))
    message("\twarning:", head(msg_push$warning))
    message("\tmessage:", head(msg_push$message))
  }

  # send response with data to the client
  pbdZMQ::send.socket(
    socket = socket,
    data   = msg_push
  )

  if (debug) message("ok.\n")

  return(NULL)
}


#' Starts server
#'
#' @param address address where the server listens to
#' @param port port
#' @param debug TRUE/FALSE debugging option (print messages etc...)
#' @export
server <- function(
  address = "tcp://*",
  port    = 5555,
  debug   = TRUE){

  stopifnot(requireNamespace("pbdZMQ", quietly = !debug))

  if (isTRUE(debug)) message("Listening on ", address, ":", port, " ...", appendLF = FALSE)

  context = pbdZMQ::init.context()
  socket  = pbdZMQ::init.socket(context, "ZMQ_REP")

  # waith for socket connection from a client
  if (pbdZMQ::bind.socket(socket, paste0(address, ":", port)) && isTRUE(debug)) message("ok.")

  # handle exit behaviour
  on.exit(
    {
      if (isTRUE(debug)) message("Closing! \n")
      pbdZMQ::disconnect.socket(
        socket  = socket,
        address = address
      )
    }
  )


  #  main loop --------------------------------------------------------------

  while(TRUE){

    if (isTRUE(debug)) message("Waiting for an incomming message...")

    # listen to ingoing message
    msg_pull <- pbdZMQ::receive.socket(socket = socket)

    # if (is.character(msg_pull) && msg_pull == "b") break
    # if (is.character(msg_pull) && msg_pull == "q") quit(save = "no", status = 0)

    if (isTRUE(debug)) {
      message("Message received:")
      message("\tcommand:", msg_pull$command, "\n")
    }
    
    if (is.list(msg_pull)) {
      switch(
        msg_pull$command,

        "stop" = {
          pbdZMQ::send.socket(socket = socket, data = "ok")
          quit(save = "no", status = 0)
        },

        "break" = {
          pbdZMQ::send.socket(socket = socket, data = "ok")
          break
        },

        "test" = {
          pbdZMQ::send.socket(socket = socket, data = "ok")
        },

        "call" = .do.call_remote(msg_pull, socket = socket, debug = debug),

        "eval" = .eval_remote(msg_pull, socket = socket, debug = debug),

        # default case:
        {
          warning("Unkown command:", msg_pull$command)
        }
      )
    } else {
      warning("Unkown type of message:", msg_pull)
    }
  }

  return(invisible(NULL))
}
