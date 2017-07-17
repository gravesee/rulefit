#' @export
sas <- function(x) UseMethod("sas")

#' @export
sas.statement_numeric <- function(x) {
  fmt <- if (x$dir == -1) "(.z < %s < %s)" else "(%s >= %s)"
  sprintf(fmt, x$name, x$value)
}

#' @export
sas.statement_factor <- function(x) {
  sprintf("(%s in (\"%s\"))", x$name, paste(x$value, collapse="\",\""))
}

#' @export
sas.statement_ordered <- function(x) sas.node_factor(x)

#' @export
sas.statement_missing <- function(x) sprintf("(missing(%s))", x$name)

#' @expot
sas.rule <- function(x) {
  paste0(lapply(x, sas), collapse = " AND ")
}

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

#' @export
print.rule <- function(x, ...) {
  print(toString(x))
}