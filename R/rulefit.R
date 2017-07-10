## ' @title GBM Nodes to Sparse Matrix
## ' @description Predict gbm terminal node membership in a sparse matrix format
## ' @param mod GBM model
## ' @param newdata Dataframe for predicting node membership
## ' @param n.trees Number of trees to use for model prediction
#' @export
gbm_to_sparse_nodes <- function(mod, newdata, n.trees) {

  m <- make_node_map(mod)
  mod.copy <- mod

  ## overwrite GBM tree data to predict the node id
  for (i in seq.int(m$terminal_nodes)) {
    mod.copy$trees[[i]][[2]][m$terminal_nodes[[i]]] <- m$tree_to_node[[i]]
  }

  node_ids <- gbm::predict.gbm(mod.copy, newdata, seq.int(n.trees), single.tree = TRUE)

  sp <- as(matrix(), "dgTMatrix")
  sp@i <- as.integer(rep(seq.int(nrow(node_ids)), ncol(node_ids)) - 1)
  sp@j <- as.integer(node_ids - 1)
  sp@Dim <- as.integer(c(nrow(node_ids), max(unlist(m$tree_to_node))))
  sp@x <- rep(1, length(sp@i))

  sp
}

#' Make Node Map
#' @param mod model object from gbm package
#' Creates look up lists that map unique terminal node IDs to gbm trees and
#' local node ids
make_node_map <- function(mod) {

  terminal_nodes <- lapply(seq.int(mod$n.trees), function(i) {
    which(mod$trees[[i]][[1]] == -1)
  })

  tree_to_node <- relist(seq_along(unlist(terminal_nodes)), terminal_nodes)
  node_to_tree <- rep(seq_along(tree_to_node), lengths(tree_to_node))

  list(
    "terminal_nodes" = terminal_nodes,
    "tree_to_node" = tree_to_node,
    "node_to_tree" = node_to_tree)
}

#' Get Tree Position
#' @param node_id Unique terminal node ID in the entire forest ensemble
#' @param node_map Node-to-tree lookup lists produced by
#' \code{\link{make_node_map}}
#' @value returns a tuple containing the tree number and local node position of
#' the requested \code{node_id}
get_tree_position <- function(node_id, node_map) {
  l <- mapply(c, node_map$node_to_tree, unlist(node_map$terminal_nodes), SIMPLIFY = F)
  l[[node_id]]
}


#' Terminal Node Lineage
#'
#' Recursively build a rule starting from a terminal node and working backwards
#' to the root node. Logical statements are accumulated along the way.
#'
#' @param mod GBM model object
#' @param tree Tree number from the gbm model
#' @param child Local child position within the tree
#' @value Returns a list of logical conditions than can be combined to create a
#' single rule.
get_lineage <- function(mod, tree, child) {

  if (child == 1) return()

  ## tree / term node position
  t <- mod$trees[[tree]] # tree
  k <- do.call(cbind, t[3:5])

  # parent and direction
  p <- row(k)[which(k == (child - 1))]
  d <- col(k)[which(k == (child - 1))]
  dir <- c(-1, 1, 0)

  ## recurse
  c(list(node_generator(mod, t, p, child, dir[d])),
    get_lineage(mod, tree, p))
}


#' Train Rule Fit Model
#'
#' Train a Rule Fit model
#'
#' @export
RuleFit <- function(mod, newdata, n.trees, y, nfolds=5, alpha=1, family="binomial", ...) UseMethod("RuleFit")


#' Train Rule Fit Model
#'
#' Train a Rule Fit model from an existing GBM model
#'
#' @param mod A gbm model object
#' @param newdata Optional dataset to train the RuleFit model
#' @param n.trees Number of trees to consider from the gbm model. Defaults to
#' the optimal number of trees based on training method.
#' @param y Dependent variable for training the model. Defaults to target used
#' to build the gbm model.
#' @param nfolds Number of cross validation folds to use for selecting the
#' optimal glmnet lambda. Defaults to 5.
#' @param alpha Regularization mixture term. Defaults to 1 for LASSO regression.
#' Can pass 0 for Ridge or (0, 1) for Elastic Net.
#' @param family Defaults to distribution used to train GBM model.
#' @value A \code{rule_fit_modeL} object
#' @export
RuleFit.gbm <- function(mod, newdata, n.trees=gbm.perf(mod, plot.it = F),
  y=mod$data$y, nfolds=5, alpha=1, family=NULL, ...) {

  if (missing(newdata)) stop("must provide newdata", call. = F)

  if (is.null(family)) {
    family <- switch(mod$distribution$name,
      "bernoulli" = "binomial",
      "default" = "gaussian")
  }

  nodes <- gbm_to_sparse_nodes(mod, newdata, n.trees)

  fit <- cv.glmnet(nodes, y, nfolds=nfolds, family=family, ...)

  structure(
    list(
      gbm_model = mod,
      glmnet_model = fit,
      n.trees = n.trees),
    class="rule_fit_model")
}

#' Print RuleFit Model
#' @export
print.rule_fit_model <- function(x, ...) {

  ## summarize GBM model
  cat("RuleFit model")
  invisible()

}

#' Predict RuleFit Model
#' @param object A rule_fit_model object
#' @param newdata Optional dataset for calculating predictions. Default uses
#' training data stored with model.
#' @param ... parameters passed on to other functions.
#' @export
predict.rule_fit_model <- function(object, newdata, ...) {

  if(missing(newdata)) stop("must provide newdata", call. = F)

  nodes <- gbm_to_sparse_nodes(object$gbm_model, newdata, object$n.trees)

  predict(object$glmnet_model, nodes, ...)[,1]
}

#' Convert RuleFit Model to SAS Code
#' @param rf A rule_fit_model object
#' @param ... parameters passed on to other functions.
#' @value Returns a character vector of SAS code that produces the RuleFit
#' model predictions.
#' @export
rulefit_to_sas <- function(rf, pfx, ...) {

  m <- make_node_map(rf$gbm_model)
  coefs <- coef(rf$glmnet_model, ...)
  node_ids <- which(coefs[-1] != 0)

  K <- lapply(node_ids, get_tree_position, m)

  rules <- lapply(K, function(k) get_lineage(rf$gbm_model, k[1], k[2]))

  ## Logic to dedup rules...

  ### MAKE THIS OPTIONAL ?
  hashes <- sapply(rules, function(x) digest::sha1(as.character(x)))
  coefs <- c(coefs[1], sapply(split(coefs[-1][coefs[-1] != 0], hashes), sum))

  rules <- sapply(split(rules, hashes), head, 1)


  node_code <- lapply(seq_along(rules), function(i) {

    rule_code <- lapply(rev(rules[[i]]), rule_fit)

    paste(
      sprintf("%s_rule_%03d  = ", pfx, i),
      do.call(paste, c(rule_code, sep=" and ")), ";")
  })

  code <- c("/*** Rule Definitions ***/\n", unlist(node_code))
  code <- c(code, "\n/*** Model Equation ***/\n")
  code <- c(code, c(sprintf("%s_rule_fit_model = % 03.10f +", pfx, coefs[1])))
  code <- c(code, paste(sprintf("  %s_rule_%03d * % 03.10f", pfx, seq_along(rules),
    coefs[-1]), collapse=" +\n"), ";")

  code
}


