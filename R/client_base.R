#' Connect to remote server
#'
#' @param address host address (defaults to 'localhost')
#' @param port port
#'
#' @return
# @export
connect_base <- function(address = "localhost", port = 6011){

  cat("Listening...")

  remote_con <- socketConnection(
    host     = address,
    port     = port,
    blocking = TRUE,
    server   = FALSE,
    open     = "w+b")

  return(remote_con)
}

#' Remote query execution
#'
#' @param command function to be called
#' @param args_remote list of arguments that are passed quoted over the connection and evaluated on the server
#' @param args_local list of arguments that are passed evaluated (e.g. data)
#' @param remote_con connection
#'
#' @return whatever command call returns
# @export
rcall_base <- function(command, args_remote = NULL, args_local = NULL, remote_con) {

  msg_push <- list(
    command = command,
    args_remote = substitute(args_remote),
    args_local  = args_local
  )

  saveRDS(object = msg_push, file = remote_con)

  msg_pull <- readRDS(file = remote_con)

  if (!is.null(msg_pull$error)) stop(msg_pull$error)
  if (!is.null(msg_pull$warning)) warning(msg_pull$warning)
  if (!is.null(msg_pull$message)) message(msg_pull$message)

  return(msg_pull$result)

}


#' Title
#'
#' @param con
#'
#' @return
# @export
rstop_base <- function(con) {
  saveRDS(object = "q", file = con)
}



#' Title
#'
#' @param con
#'
#' @return
# @export
rbreak_base <- function(con) {
  saveRDS(object = "b", file = con)
}
