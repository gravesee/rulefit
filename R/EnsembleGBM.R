#' @include Ensemble.R

EnsembleGBM <- setRefClass("EnsembleGBM", contains="Ensemble",
  methods = list(
    initialize = function(mod) {
      stopifnot(inherits(mod, "gbm"))
      .self$mod <- mod
      .self$node_map <- make_node_map()
    })
)


EnsembleGBM$methods(predict_sparse_nodes = function(newdata) {
  mod.copy <- mod

  ## overwrite GBM tree data to predict the node id
  for (i in seq.int(node_map$terminal_nodes)) {
    mod.copy$trees[[i]][[2]][node_map$terminal_nodes[[i]]] <- node_map$tree_to_node[[i]]
  }

  node_ids <- gbm::predict.gbm(mod.copy, newdata, seq.int(mod$n.trees), single.tree = TRUE)

  sp <- as(matrix(), "dgTMatrix")
  sp@i <- as.integer(rep(seq.int(nrow(node_ids)), ncol(node_ids)) - 1)
  sp@j <- as.integer(node_ids - 1)
  sp@Dim <- as.integer(c(nrow(node_ids), max(node_map$tree_to_node[[mod$n.trees]])))
  sp@x <- rep(1, length(sp@i))

  sp
})


#' Make Node Map
#'
#' @name EnsembleGBM-make_node_map
#' @param mod model object from gbm package
#' Creates look up lists that map unique terminal node IDs to gbm trees and
#' local node ids
NULL
EnsembleGBM$methods(make_node_map = function() {
  terminal_nodes <- lapply(seq.int(mod$n.trees), function(i) {
    which(mod$trees[[i]][[1]] == -1)
  })

  tree_to_node <- relist(seq_along(unlist(terminal_nodes)), terminal_nodes)
  node_to_tree <- rep(seq_along(tree_to_node), lengths(tree_to_node))

  list(
    "terminal_nodes" = terminal_nodes,
    "tree_to_node" = tree_to_node,
    "node_to_tree" = node_to_tree)
})


EnsembleGBM$methods(get_tree_position = function(node_id) {
  l <- mapply(c, node_map$node_to_tree, unlist(node_map$terminal_nodes), SIMPLIFY = F)
  l[[node_id]]
})


#' Terminal Node Lineage
#'
#' Recursively build a rule starting from a terminal node and working backwards
#' to the root node. Logical statements are accumulated along the way.
#'
#' @name EnsembleGBM-get_lineage
#' @param mod GBM model object
#' @param tree Tree number from the gbm model
#' @param child Local child position within the tree
#' @return Returns a list of logical conditions than can be combined to create a
#' single rule.
NULL
EnsembleGBM$methods(get_lineage=function(tid, cid) {

  if (cid == 1) return()

  ## tree / term node position
  tree <- mod$trees[[tid]] # tree
  k <- do.call(cbind, tree[3:5])

  # parent and direction
  p <- row(k)[which(k == (cid - 1))]
  d <- col(k)[which(k == (cid - 1))]

  dir <- c(-1, 1, 0)

  ## recurse
  structure(
    rev(c(list(make_statement(tree, p, cid, dir[d])), get_lineage(tid, p))),
    class = "rule")
})


EnsembleGBM$methods(make_statement = function(tree, p, c, dir, ...) {
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
      dir    = dir),
    class = c(paste0("statement_", type), "statement"))
})
