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
  
  rf <- structure(
    list(
      base_model  = mod,
      n.trees     = n.trees,
      rules       = rules,
      node_map    = nm,
      fit         = NULL,
      support     = numeric(0)),
    class="rulefit")
  
  predict_sparse_nodes(rf, gbm::reconstructGBMdata(mod)) %>%
    as.matrix %>%
    caret::findLinearCombos(.) %>%
    '$'(., 'remove') -> singular_nodes_index
  
  rf$nodes_index <- c(1, singular_nodes_index)
  
  return(rf)
}

#' @export
rulefit.GBMFit <- function(mod, n.trees) {

  ## convert the model to gbm 2.1
  old <- gbm3::to_old_gbm(mod)

  ## check distribution is in right format
  if(is.character(old$distribution)) {
    old$distribution <- list(name=old$distribution)
  }

  nm <- make_node_map(old, n.trees)
  rules <- generate_rules(old, nm)

  rf <- structure(
    list(
      base_model  = old,
      n.trees     = n.trees,
      rules       = rules,
      node_map    = nm,
      fit         = NULL,
      support     = numeric(0)),
    class="rulefit")
  
  predict_sparse_nodes(rf, gbm::reconstructGBMdata(old)) %>%
    as.matrix %>%
    caret::findLinearCombos(.) %>%
    '$'(., 'remove') -> singular_nodes_index
  
  rf$nodes_index <- c(1, singular_nodes_index)
  
  return(rf)
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

## functions for decoding rules 
name_extract <- function(rulelist) purrr::map_chr(rulelist, ~ '$'(., 'name'))

names_list <- function(rulelist) purrr::map(rulelist, ~ if(!is.null(.)) name_extract(.))

unique_names <- function(rulelist) names_list(rulelist) %>% unlist %>% unique

## missing values to zeros for linear components
missing_to_zero <- function(x){
  Hmisc::impute(x, 0) %>%
    unclass
}

# winsorization for continuous variables
winsorize <- function(x, beta){
  quant <- quantile(x, probs = c(beta, 1.0 - beta), na.rm = T)
  pmax(min(quant), pmin(max(quant), x)) # min and max so we can take any beta between 0 and 1
}

## train generic
#' @export
train <- function(rf, x, y, ...) UseMethod("train")

## train method for rulefit class
#' @export
train.rulefit <- function(rf, x, y, 
                          linear_components = NULL, 
                          interact = NULL, 
                          bags = NULL, 
                          alpha = 1, 
                          winsor = .025, 
                          foldid = NULL, 
                          parallel = TRUE, 
                          keep, 
                          ...) {

  # make node matrix
  nodes <- predict_sparse_nodes(rf, x)[, -rf$nodes_index]
  
  # record rule support
  rf$support <- Matrix::colSums(nodes) / nrow(nodes)
  
  # add linear effects
  if(!is.null(linear_components)){
    nodes <- x %>%
      select_(paste0('c(', paste(linear_components, collapse = ', '), ')')) %>%
      mutate_all(funs(winsorize(., winsor))) %>%
      as.data.frame %>%
      as.matrix %>%
      cbind(nodes)
    
    # expand on interactions
    if(!is.null(interact)){
      nodes <- purrr::map(linear_components, ~ '['(nodes,,.)) %>% 
        purrr::map(~ . * nodes[, -(1:length(linear_components))]) %>% 
        purrr::reduce(cbind) %>% 
        cbind(nodes, .)
      
      colnames(nodes) <- NULL
    }
  }
  
  # centering and scaling
  nodes <- nodes %>%
    scale
  
  # extract centers
  centers <- nodes %>%
    attr('scaled:center')
  
  scales <- nodes %>%
    attr('scaled:scale') %>%
    ifelse(. == 0, 1, .)
  
  nodes <- nodes %>%
    as.data.frame %>%
    purrr::map_df(~ Hmisc::impute(., 0)) %>%
    as.matrix %>%
    as(., 'dgCMatrix')
  
  rf$fit <- glmnet::cv.glmnet(nodes, 
                              y, 
                              standardize = F, 
                              alpha = alpha,
                              keep = keep,
                              foldid = foldid,
                              ...)
  
  if (!is.null(bags)) {
    lambda <- rf$fit$glmnet.fit$lambda
    
    if (parallel) {
      betas <- foreach(i = seq.int(bags), .combine = `+`) %dopar% {
        n <- sample(seq.int(nrow(nodes)), nrow(nodes), replace = TRUE)
        fit <- glmnet(nodes[n,], y[n], lambda=lambda, alpha = alpha, standardize = F, ...)
        fit$beta
      }
    } else {
      betas <- Reduce(`+`, lapply(seq.int(bags), function(i) {
        n <- sample(seq.int(nrow(nodes)), nrow(nodes), replace = TRUE)
        fit <- glmnet(nodes[n,], y[n], lambda=lambda, alpha = alpha, standardize = F, ...)
        fit$beta}))
    }
    betas <- betas / bags
    rf$fit$glmnet.fit$beta <- betas
  }
  
  rf$fit$glmnet.fit$beta <- rf$fit$glmnet.fit$beta / scales # rescale betas
  rf$fit$glmnet.fit$a0 <- centers %*% rf$fit$glmnet.fit$beta %>% # add rescaled centers to the intercept
    as.numeric %>% 
    '-'(rf$fit$glmnet.fit$a0, .)
  rf$linear_components <- linear_components
  rf$interact <- interact
  rf
}

## predict method for rulefit class
#' @export
predict.rulefit <- function(object, newx, s=c("lambda.1se", "lambda.min"), nodes=FALSE, ...) {
  s <- match.arg(s)
  X <- predict_sparse_nodes(object, newx)
  X <- X[, -object$nodes_index]
  
  if(!is.null(object$linear_components)){
    X <- newx %>%
      select_(paste0('c(', paste(object$linear_components, collapse = ', '), ')')) %>%
      mutate_all(funs(missing_to_zero)) %>%
      as.data.frame %>%
      as.matrix %>%
      cbind(X)
  }
  
  if(!is.null(object$interact)){
    X <- purrr::map(object$linear_components, ~ '['(X,,.)) %>% 
      purrr::map(~ . * X[, -(1:length(object$linear_components))]) %>% 
      purrr::reduce(cbind) %>% 
      cbind(X, .)
  }

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
    rule = sapply(object$rules[-object$nodes_index][cf != 0], toString),
    support = object$support[-object$nodes_index][cf != 0],
    coefficient = cf[-object$nodes_index][cf != 0],
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
