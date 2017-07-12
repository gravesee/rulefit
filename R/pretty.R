#' @export
sas <- function(r) UseMethod("sas")

#' @export
sas.statement_numeric <- function(l) {
  fmt <- if (l$dir == -1) "(.z < %s < %s)" else "(%s >= %s)"
  sprintf(fmt, l$name, l$value)
}

#' @export
sas.statement_factor <- function(l) {
  sprintf("(%s in (\"%s\"))", l$name, paste(l$value, collapse="\",\""))
}

#' @export
sas.statement_ordered <- function(l) sas.node_factor(l)

#' @export
sas.statement_missing <- function(l) sprintf("(missing(%s))", l$name)

#' @export
print.statement_factor <- function(x, ...) {
 v <- sprintf("[\"%s\"]", paste0(x$value, collapse = "\",\""))
 cat(sprintf("%s IN %s", x$name, v), sep = "\n")
}

#' @export
print.statement_ordered <- function(x, ...) {
  print.statement_factor(x, ...)
}

#' @export
print.statement_missing <- function(x, ...) {
  cat(sprintf("%s IS NULL", x$name), sep = "\n")
}

#' @export
print.statement_numeric <- function(x, ...) {
  d <- if (x$dir == -1) "<" else if (x$dir == 1) ">=" else "WHAT?"
  cat(sprintf("%s %s %2.5f", x$name, d, x$value), sep = "\n")
}

#' @export
print.rule <- function(x, ...) {
  lapply(x, print)
}