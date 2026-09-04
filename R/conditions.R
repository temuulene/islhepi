# Conditions
#
# Forward the caller's environment so cli can interpolate local values in
# messages raised by internal validation helpers.

.islh_abort <- function(message, call = parent.frame(), .envir = parent.frame()) {
  cli::cli_abort(message, call = call, .envir = .envir)
}

