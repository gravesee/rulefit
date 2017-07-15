## work back from node map to get all nodes passed through
node_path <- function(cid, tree) {

  if (cid == 1) return(1)

  k <- do.call(cbind, tree[3:5])
  p <- row(k)[which(k == (cid - 1))]

  return(c(cid, node_path(p, tree)))
}

make_node_map <- function(mod, n.trees) {
  ## Number of internal nodes
  nt <- seq.int(n.trees)
  l <- lapply(mod$trees[nt], function(t) seq_along(t[[1]]))

  ## Don't need the path of the root node
  paths <- lapply(nt, function(i) {
    lapply(l[[i]], node_path, mod$trees[[i]])
  })

  nodes_per_tree <- lengths(l)
  nodes_per_tree[1] <- 0

  i <- cumsum(nodes_per_tree)

  res <- mapply(function(p, n) relist(unlist(p) + n, p), paths, i, SIMPLIFY = F)

  term <- lapply(mod$trees[nt], function(t) which(t[[1]] == -1))

  #nodes <- relist(seq_along(unlist(term)), term)
  nodes <- mapply('[', relist(seq_along(unlist(l)), l), term, SIMPLIFY = F)

  ## unlist one level
  list(rules = unlist(res, recursive = F), nodes = nodes, termn = term)
}

## predict node membership sparse matrix
predict_sparse_nodes = function(rf, newx) {
  ## check variables
  v <- rf$base_model$var.names
  stopifnot(all(v %in% names(newx)))

  nm <- rf$node_map
  nt <- seq.int(rf$n.trees)

  ## overwrite GBM tree data to predict the node id
  for (i in nt) {
    rf$base_model$trees[[i]][[2]][nm$termn[[i]]] <- nm$nodes[[i]]
  }

  ## reorder newx to be in same order model was trained
  node_ids <- predict(rf$base_model, newx[v], nt, single.tree = TRUE)

  ### rule values to put in sparse matrix
  v <- sapply(t(node_ids), function(i) nm$rules[i])

  ## create row index
  rows <- split(v, rep(seq.int(nrow(node_ids)), each=rf$n.trees))
  rows <- rep(seq_along(rows), sapply(rows, function(r) length(unlist(r))))

  ## create column index
  cols <- unlist(v)

  Matrix::sparseMatrix(
    i = as.integer(rows),
    j = as.integer(cols),
    dims = as.integer(c(nrow(node_ids), max(unlist(nm$rules))))
  )
}

## helper class to bundle split info
make_statement <- function(mod, tree, p, c, dir) {
  v <- tree[[1]][p] + 1 # variable position (+1 because its zero indexed)
  lvls <- mod$var.levels[[v]]

  type <-
    if (dir == 0) "missing" else
      if (mod$var.type[v] > 0) "factor" else
        if (is.character(lvls)) "ordered" else "numeric"

  sv <- tree[[2]][p] # split value

  value <- switch(type,
    "missing" = NULL,
    "factor"  = lvls[mod$c.splits[[sv + 1]] == dir],
    "ordered" = if (dir == -1) lvls[lvls <= sv + 1] else lvls[lvls > sv + 1],
    "numeric" = sv)

  structure(
    list(
      name   = mod$var.names[v],
      value  = value,
      dir    = dir),
    class = c(paste0("statement_", type), "statement"))
}

### create list of statements working back from a terminal node
make_rule <- function(mod, tree, n) {

  if (n == 1) return()

  ## tree / term node position
  k <- do.call(cbind, tree[3:5])

  # parent and direction
  p <- row(k)[which(k == (n - 1))]
  d <- col(k)[which(k == (n - 1))]

  dir <- c(-1, 1, 0)

  ## recurse
  structure(
    c(list(make_statement(mod, tree, p, n, dir[d])), make_rule(mod, tree, p)),
    class = "rule")
}

## generate all of the decision tree rules from a given tree ensemble
generate_rules <- function(mod, nm) {

  nt <- seq_along(nm$nodes)

  ## Loop over trees
  ## loop over terminal node positions
  ## extract recursive rules
  rules <- lapply(mod$trees[nt], function(t) {
    n <- seq_along(t[[2]])
    lapply(n, function(i) make_rule(mod, t, i))
  })

  unlist(rules, recursive = FALSE)
}


#' @export
rulefit <- function(mod, n.trees) UseMethod("rulefit")

## wrap a tree ensemble in a class and generate the rules
#' @export
rulefit.gbm <- function(mod, n.trees) {

  nm <- make_node_map(mod, n.trees)
  rules <- generate_rules(mod, nm)

  structure(
    list(
      base_model  = mod,
      n.trees     = n.trees,
      rules       = rules,
      node_map    = nm,
      fit         = NULL,
      support     = numeric(0)),
    class="rulefit")
}

#' @export
rulefit.GBMFit <- function(mod, n.trees) {

  ## convert the model to gbm 2.1
  old <- gbm3::to_old_gbm(mod)

  ## check distribution is in right format
  if(is.null(old$distribution$name)) {
    old$distribution <- list(name=old$distribution)
  }

  nm <- make_node_map(old, n.trees)
  rules <- generate_rules(old, nm)

  structure(
    list(
      base_model  = old,
      n.trees     = n.trees,
      rules       = rules,
      node_map    = nm,
      fit         = NULL,
      support     = numeric(0)),
    class="rulefit")
}

## print method for rulefit class
#' @export
print.rulefit <- function(object) {
  cat(sprintf("RuleFit object with %d rules\n", length(object$rules)))
  cat(sprintf("Rules generated from %s model show below\n", class(object$base_model)))
  cat(paste0(rep("-", 80), collapse = ""), sep = "\n")
  show(object$base_model)
  invisible()
}


## train generic
#' @export
train <- function(rf, x, y, ...) UseMethod("train")

## train method for rulefit class
#' @export
train.rulefit <- function(rf, x, y, bags = NULL, ...) {
  nodes <- predict_sparse_nodes(rf, x)
  rf$fit <- glmnet::cv.glmnet(nodes, y, ...)
  if(!is.null(bags)){
    rf$fit$glmnet.fit$beta <- nodes %>%
      as.matrix %>%
      as.data.frame %>%
      purrr::map_df(~ as.numeric(.)) %>%
      dplyr::bind_cols(data.frame(y = y), .) %>%
      broom::bootstrap(bags) %>%
      dplyr::do(glmnet::glmnet(.[, -1] %>%
                                 as.data.frame %>%
                                 as.matrix, 
                               .[, 1] %>%
                                 as.data.frame %>%
                                 as.matrix %>%
                                 as.numeric,
                               lambda = rf$fit$glmnet.fit$lambda)$beta %>%
                  as.matrix %>%
                  as.data.frame) %>%
      dplyr::mutate(tree = 1:ncol(nodes)) %>%
      dplyr::group_by(tree) %>%
      dplyr::summarise_each(funs(mean)) %>%
      dplyr::select(-replicate, -tree) %>%
      as.data.frame %>%
      as.matrix %>%
      as('dgCMatrix')
  } 
  rf$support <- Matrix::colSums(nodes)/nrow(nodes)
  rf
}


## predict method for rulefit class
#' @export
predict.rulefit <- function(object, newx, s=c("lambda.1se", "lambda.min"), nodes=FALSE, ...) {
  s <- match.arg(s)
  X <- predict_sparse_nodes(object, newx)

  if (nodes) {
    cf <- coef(object$fit, s=s)[-1]
    return(X[,which(cf != 0)])
  }

  predict(object$fit, X, s=s)
}

## summary method for rulefit class
#' @export
summary.rulefit <- function(object, s=c("lambda.1se", "lambda.min"), dedup=TRUE, ...) {
  s <- match.arg(s)
  if (is.null(object$fit)) return(invisible())

  cf <- coef(object$fit, s=s)[-1]

  res <- data.frame(
    rule = sapply(object$rules[cf != 0], toString),
    support = object$support[cf != 0],
    coefficient = cf[cf != 0],
    number = which(cf != 0))

  if (dedup) {

    sums <- aggregate(.~rule, res, sum)
    maxs <- aggregate(.~rule, res, max)
    nums <- aggregate(.~rule, res, c)

    res <- sums
    res$support <- maxs$support
    res$number <- nums$number

  }

  res <- res[order(-res$support),]
  row.names(res) <- NULL

  return(res)
}
