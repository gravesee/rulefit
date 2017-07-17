

## Variable level importance
#' @export
importance <- function(x, ...) UseMethod("importance")

#' Variable Importance
#' @param x rulefitFit object as output by the \code{\link{train}} function
#' @param newx development dataset usede to train the rulefit model
#' @param s lambda penalty term used to retrieve coefficients
#' @export
importance.rulefitFit <- function(x, newx, s=c("lambda.1se", "lambda.min"), ...) {
  s <- match.arg(s)

  su <- summary(x, newx, s=s, dedup=FALSE)
  su <- su[order(su$node),]

  vars_per_rule <- lapply(x$rules[su$node], function(r) sapply(r, '[[', "name"))

  rules_per_var <- list()
  for (i in seq_along(vars_per_rule)) {
    if (length(vars_per_rule[[i]]) > 0) {
      idx <- vars_per_rule[[i]]
      rules_per_var[idx] <- lapply(rules_per_var[idx], function(x) c(x, i))
    }
  }

  nodes <- predict(x, newx, s=s, nodes=TRUE)

  ## importance at X
  m <- as.matrix(nodes)
  Ik = sweep(abs(t(t(m) - su$support)), MARGIN = 2, abs(su$coefficient), FUN = `*`)

  ## divide each rule Ik by the number of vars in each rule
  imp <- colSums(Ik)/lengths(vars_per_rule)
  imp <- sapply(rules_per_var, function(r) sum(imp[r]))

  structure(sort(imp/max(imp), decreasing = TRUE), class="rulefitVarImp")
}

#' @export
plot.rulefitVarImp <- function(x, y, ...) {
  f <- colorRampPalette(c("lightblue", "blue"))
  barplot(rev(x), horiz=TRUE, ylab = "Variable", xlab = "Relative Importance",
    col = f(length(x)))
  title("Variable Importance")
}