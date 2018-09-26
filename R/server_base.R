server_base <- function(address = "localhost", port = 6011, ..., debug = FALSE){

  message("Listening on ", address, ":", port, " ...")

  # waith for socket connection from a client
  con <- socketConnection(
    host     = address,
    port     = port,
    blocking = TRUE,
    server   = TRUE,
    open     = "w+b",
    ...)

  on.exit(
    {
      message("Closing! \n")
      close(con)
    }
  )


  #  main loop --------------------------------------------------------------

  while(TRUE){

    # listen to ingoing message
    message_recieved <- readRDS(file = con)

    if (is.character(message_recieved) && message_recieved == "b") break
    if (is.character(message_recieved) && message_recieved == "q") quit(save = "no", status = 0)

    message("Message received:", message_recieved, "\n")

    # insert odbc connection into arguments
    call_string <- as.character(message_recieved$command)
    args_list <- c(list(), eval(message_recieved$args_remote), message_recieved$args_local)

    # template for outgoing message
    message_put <- list(
      result  = NULL,
      error   = NULL,
      message = NULL)

    # main call
    tryCatch(
      message_put$result <-
        do.call(
          what = call_string,
          args = args_list),

      error   = function(m){message_put$error   <- m;return(NULL)},
      message = function(m){message_put$message <- m;return(NULL)},
      warning = function(m){message_put$warning <- m;return(NULL)}
    )

    if (debug) {
      if (is.character(message_put$result) & is.null(message_put$error)) {
        message_put$error <- message_put$result
        message_put$result <- NULL
      }

      # print result
      print(head(message_put$result))
    }

    if (debug) message("Sending results...", appendLF = FALSE)

    # send data to client
    saveRDS(object = message_put,
            file   = con)

    if (debug) message("ok.")
  }

  message("Stopping...")
  return(invisible(NULL))
}
