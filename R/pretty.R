#' @export
sas <- function(x, ...) UseMethod("sas")

#' @export
sas.default <- function(x, ...) NULL

#' @export
sas.statement_numeric <- function(x, ...) {
  fmt <- if (x$dir == -1) "(.z < %s < %s)" else "(%s >= %s)"
  sprintf(fmt, x$name, x$value)
}

#' @export
sas.statement_factor <- function(x, ...) {
  sprintf("(%s in (\"%s\"))", x$name, paste(x$value, collapse="\",\""))
}

#' @export
sas.statement_ordered <- function(x, ...) sas.node_factor(x, ...)

#' @export
sas.statement_missing <- function(x, ...) sprintf("(missing(%s))", x$name)

#' @export
sas.rule <- function(x, ...) {
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

#' @export
rlang <- function(x) UseMethod("rlang")

#' @export
rlang.statement_numeric <- function(x) {
  fmt <- if (x$dir == -1) "(%s < %s & !is.na(%s))" else "(%s >= %s & !is.na(%s))"
  sprintf(fmt, x$name, x$value, x$name)
}

#' @export
rlang.statement_factor <- function(x) {
  sprintf("(%s %%in%% c(\"%s\"))", x$name, paste(x$value, collapse="\",\""))
}

#' @export
rlang.statement_ordered <- function(x) rlang.node_factor(x)

#' @export
rlang.statement_missing <- function(x) sprintf("(is.na(%s))", x$name)

#' @export
rlang.default <- function(x) NULL

#' @export
rlang.rule <- function(x) {
  paste0(lapply(x, rlang), collapse = " & ")
}

### SAS model
#' @export
sas.rulefitFit <- function(x, s=c("lambda.1se", "lambda.min"), pfx="rf", ...) {
  s <- match.arg(s)
  cf <- coef(x$fit, s)[,1]
  rules <- x$rules[which(cf[-1] != 0)]

  nm <- sprintf("%s_rule%03d", pfx, seq_along(rules))

  code <- c(
    c("/* Rule Definitions */"),
    sprintf("%s = %s;", nm, sapply(rules, sas)),
    c("\n/* Model Equation */"),
    sprintf("%s_rulefit_mod = %3.6f", pfx, cf[1]),
    sprintf("  + % 3.6f * %s", cf[cf != 0][-1], nm), ";")

  code
}

### SQL support

#' @export
sql <- function(x, ...) UseMethod("sql")

#' @export
sql.default <- function(x, ...) NULL

#' @export
sql.statement_numeric <- function(x, ...) {
  fmt <- if (x$dir == -1) "(%s < %s)" else "(%s >= %s)"
  sprintf(fmt, x$name, x$value)
}

#' @export
sql.statement_factor <- function(x, ...) {
  sprintf("(%s in (\'%s\'))", x$name, paste(x$value, collapse="\',\'"))
}

#' @export
sql.statement_ordered <- function(x, ...) sql.node_factor(x, ...)

#' @export
sql.statement_missing <- function(x, ...) sprintf("(%s is NULL)", x$name)

#' @export
sql.rule <- function(x, ...) {
  paste0(lapply(x, sql), collapse = " AND ")
}

#' @export
sql.rulefitFit <- function(x, s=c("lambda.1se", "lambda.min"), pfx="rf", ...) {
  s <- match.arg(s)
  cf <- coef(x$fit, s)[,1]
  rules <- x$rules[which(cf[-1] != 0)]
  
  nm <- sprintf("%s_rule%03d", pfx, seq_along(rules))
  len <- length(nm)
  
  code <- c(
    c("/* Rule Definitions */"),
    sprintf("CASE WHEN %s THEN 1 ELSE 0 END as %s,", sapply(rules[-len], sql), nm[-len]),
    sprintf("CASE WHEN %s THEN 1 ELSE 0 END as %s", sapply(rules[len], sql), nm[len]),
    c("/* Model Equation */"),
    sprintf("    % 3.6f", cf[1]),
    sprintf("  + % 3.6f * %s", cf[cf != 0][-1], nm), 
    sprintf("  as %s_rulefit_mod", pfx))
  
  code
}
