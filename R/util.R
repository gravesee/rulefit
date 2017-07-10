## a little class to encapsulate node info
node_generator <- function(mod, tree, p, c, d, dir, ...) {
  var_id <- tree[[1]][p] + 1 # variable position (+1 because its zero indexed)
  var_levels <- mod$var.levels[[var_id]]

  type <-
    if (dir == 0) "missing" else
      if (mod$var.type[var_id] > 0) "factor" else
        if (is.character(mod$var.levels[[var_id]])) "ordered" else "numeric"

  value <- switch(type,
    "missing" = NULL,
    "factor"  = var_levels[mod$c.splits[[tree[[2]][p] + 1]] == dir],
    "ordered" = if (dir == -1) var_levels[var_levels <= tree[[2]][p] + 1] else
      var_levels[var_levels >  tree[[2]][p] + 1],
    "numeric" = tree[[2]][p])

  structure(
    list(
      name   = mod$var.names[var_id],
      value  = value,
      depth  = d,
      dir    = dir,
      update = if (tree[[8]][c] != 0 && tree[[1]][c] == -1) tree[[8]][c] else NA),
    class = paste0("node_", type))
}


#' Rule Logic Generator
#' @param l A logical statement produced by \code{\link{node_generator}}
rule_fit <- function(l) UseMethod("rule_fit")

rule_fit.node_numeric <- function(l) {
  fmt <- if (l$dir == -1) "(.z < %s < %s)" else "(%s >= %s)"
  sprintf(fmt, l$name, l$value)
}

rule_fit.node_factor <- function(l) {
  sprintf("(%s in (\"%s\"))", l$name, paste(l$value, collapse="\",\""))
}

rule_fit.node_ordered <- function(l) rule_fit.node_factor(l)
rule_fit.node_missing <- function(l) sprintf("(missing(%s))", l$name)
