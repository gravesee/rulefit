#' @include Ensemble.R

#' @title RuleFit Class
#' @name RuleFit-class
#' @description RuleFit class that wraps a tree ensemble object and provides
#' methods for creating rules and training models with them.
#' @field ensemble Wrapper class that provides access to the base ensemble model
#' @field fit glmnet model combining rules from ensemble
#' @field rules list of rules generated from the ensemble
#' @field rids rule ids. The integer identifier of the node from the ensemble.
#' @field coefs fitted model coefficients for the rule ensemble.
#' @field intercept model intercep from fitted model.
#' @export RuleFit
#' @exportClass RuleFit
RuleFit <- setRefClass("RuleFit",
  fields = c(ensemble="Ensemble", fit="ANY", rules="list", rids="numeric",
    coefs="numeric", intercept="numeric"))


#' Train Rule Fit Model
#'
#' Train a Rule Fit model from an existing ensemble model
#'
#' @name RuleFit-train
#' @param x Optional dataset to train the RuleFit model
#' @param y Dependent variable for training the model. Defaults to target used
#' to build the gbm model.
#' @param nfolds Number of cross validation folds to use for selecting the
#' optimal glmnet lambda. Defaults to 5.
#' @param alpha Regularization mixture term. Defaults to 1 for LASSO regression.
#' Can pass 0 for Ridge or (0, 1) for Elastic Net.
#' @return A \code{rule_fit_modeL} object
NULL
RuleFit$methods(train = function(x, y, nfolds=5, alpha=1, ...) {

  nodes <- ensemble$predict_sparse_nodes(x)

  fit <<- cv.glmnet(x=nodes, y=y, nfolds=nfolds, alpha=alpha, ...)

  generate_rules()

})


#' Generate Rules
#'
#' Generate human readable rules from a rulefit model
#' @name RuleFit-generate_rules
NULL
RuleFit$methods(generate_rules = function() {

  coefs_ <- coef(fit, s="lambda.min")

  intercept <<- coefs_[1]
  rids <<- which(coefs_[-1] != 0)
  coefs <<- coefs_[-1][rids]

  posns <- lapply(rids, ensemble$get_tree_position)

  rules <<- lapply(posns, function(pos) ensemble$get_lineage(pos[1], pos[2]))

})


#' Predict Rule Fit Model
#'
#' @name RuleFit-predict
#' @param x Dataset on which to make RuleFit model predictions.
#' @param kfold TRUE/FALSE for whether to return the out-of-fold predictions
#' from the trained glmnet model. A close estimate of how the model will
#' generalize.
#' @return A numeric vector of predicted values.
NULL
RuleFit$methods(predict = function(x, kfold=FALSE, ...) {

  if (kfold) {
    if (is.null(fit$fit.preval)) {
      stop("kfold predictions not saved during training. Re-train with 'keep=TRUE'.", call. = F)
    }

    p <- rf$fit$fit.preval[,which.min(fit$cvm)]
    return(p)
  }

  nodes <- ensemble$predict_sparse_nodes(x)
  p <- glmnet::predict.cv.glmnet(fit, newx=nodes, s="lambda.min", ...)
  as.numeric(p)

})

#' Generate Rule Fit Model SAS Code
#'
#' @name RuleFit-generate_sas_code
#' @param file Destination to print the SAS code
#' @param pfx Character prefix to prepend to rule code. No underscore needed.
#' @param dedup TRUE/FALSE for whether to combine rules that are exactly the
#' same. See details for more information
#' @details Depending on the tree ensemble used, it is possible to train a model
#' with duplicate rules. This can happen using GBM with a low shrinkage as trees
#' are more likely to be correlated under such conditions. The dedup parameter
#' will combine rules that are exactly the same using a hash function and sum
#' their coefficients to create single, aggregated rule.
NULL
RuleFit$methods(generate_sas_code = function(file, pfx, dedup=FALSE) {

  coefs_ <- coefs
  rules_ <- rules

  if (dedup) {

    hashes <- sapply(rules, function(x) digest::sha1(as.character(x)))

    coefs_ <- sapply(split(coefs, hashes), sum)
    rules_ <- sapply(split(rules, hashes), head, 1)

  }

  node_code <- lapply(seq_along(rules_), function(i) {

    x <- lapply(rules_[[i]], sas)

    paste(
      sprintf("%s_rule_%03d  = ", pfx, i),
      do.call(paste, c(x, sep=" and ")), ";")
  })

  code <- c("/*** Rule Definitions ***/\n", unlist(node_code))
  code <- c(code, "\n/*** Model Equation ***/\n")
  code <- c(code, c(sprintf("%s_rule_fit_model = % 03.10f +", pfx, intercept)))
  code <- c(code, paste(sprintf("  %s_rule_%03d * % 03.10f", pfx,
    seq_along(rules_), coefs_), collapse=" +\n"), ";")

  writeLines(code, file, sep="\n")

})


#' Create a Rule Fit Model
#'
#' Create a Rule Fit Model using an tree ensemble as its foundation.
#'
#' @param x a tree ensemble model
#' @return  A RuleFit model object
#' @rdname rulefit-generators
#'
#' @export rulefit
rulefit <- function(x) UseMethod("rulefit")


#' Create a Rule Fit Model
#'
#' Create a Rule Fit Model from a GBM model
#'
#' @param x a GBM model
#' @return A RuleFit model object
#' @rdname rulefit-generators
#'
#' @export rulefit.gbm
rulefit.gbm <- function(x) {
  RuleFit(ensemble=EnsembleGBM(x))
}
