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
toString.statement_factor <- function(x, ...) {
  v <- sprintf("[\"%s\"]", paste0(x$value, collapse = "\",\""))
  sprintf("%s IN %s", x$name, v)
}

#' @export
toString.statement_ordered <- function(x, ...) {
  toString.statement_factor(x, ...)
}

#' @export
toString.statement_missing <- function(x, ...) {
  sprintf("%s IS NULL", x$name)
}

#' @export
toString.statement_numeric <- function(x, ...) {
  d <- if (x$dir == -1) "<" else if (x$dir == 1) ">=" else "WHAT?"
  sprintf("%s %s %2.5f", x$name, d, x$value)
}

#' @export
toString.rule <- function(x, ...) {
  paste0(lapply(x, toString), collapse = " AND ")
}