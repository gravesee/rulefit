Installation
------------

    devtools::install_git("https://GravesEE@gitlab.ins.risk.regn.net/minneapolis-r-packages/rulefit.git")

Usage
-----

### Creating a RuleFit Model

A RuleFit model uses a tree ensemble to generate its rules. As such, a
tree ensemble model must be provided to the rulefit function. This
funciton returns a RuleFit object which can be used to mine rules and
train rule ensembles.

    mod <- gbm.fit(titanic[-1], titanic$Survived, distribution="bernoulli",
      interaction.depth=3, shrinkage=0.1, verbose = FALSE)

    rf <- rulefit(mod)

    class(rf)

    ## [1] "RuleFit"
    ## attr(,"package")
    ## [1] "rulefit"

### Training

Training a RuleFit model is as easy as calling the train method. This is
a Reference Class object like those in `binnr` which means functions are
"attached" to the object itself. The train method uses the `cv.glmnet`
function from the `glmnet` package and accepts all of the same
arguments.

##### Common Arguments

<table style="width:11%;">
<colgroup>
<col width="5%" />
<col width="5%" />
</colgroup>
<thead>
<tr class="header">
<th align="left">Argument</th>
<th align="left">Purpose</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">x</td>
<td align="left">Dataset of predictors that should match what was used for training the ensemble.</td>
</tr>
<tr class="even">
<td align="left">y</td>
<td align="left">Target variable to train against.</td>
</tr>
<tr class="odd">
<td align="left">family</td>
<td align="left">What is the distribution of the target? Binomial for 0/1 variables.</td>
</tr>
<tr class="even">
<td align="left">keep</td>
<td align="left">TRUE/FALSE for whether to keep the out-of-fold predictions. Useful for validation.</td>
</tr>
<tr class="odd">
<td align="left">alpha</td>
<td align="left">Penatly mixing parameter. LASSO regression uses the default of 0.</td>
</tr>
<tr class="even">
<td align="left">nfolds</td>
<td align="left">How many k-folds to train the model with. Defaults to 5.</td>
</tr>
<tr class="odd">
<td align="left">dfmax</td>
<td align="left">How many variables should the final model have?</td>
</tr>
</tbody>
</table>

    rf$train(x=titanic[-1], y = titanic$Survived, family="binomial", keep=TRUE, nfolds=10)

The train method first creates hundreds of rules from the tree ensemble.
Each rule is a path through a single decision tree and represents the
conjunction of logical statments. Here are a few of the candidate rules
printed in pseudo- code.

    head(rf$rules, 1)

    ## [[1]]
    ## Pclass IN ["3"]
    ## Sex IN ["female"]
    ## Fare >= 20.80000

    tail(rf$rules, 1)

    ## [[1]]
    ## Age IS NULL
    ## Fare >= 7.76250
    ## Pclass IN ["1","2"]

### Predicting

Once a RuleFit model is trained. Predictions can be produced by calling
the predict method.

    p <- rf$predict(x = titanic[-1]) ## All Development Records
    k <- rf$predict(kfold = TRUE) ## Validation
    p_gbm <- predict(mod, titanic[-1], n.trees = gbm.perf(mod, plot.it = F))

    ## Using OOB method...

    roc_dev <- pROC::roc(titanic$Survived, -p)
    roc_val <- pROC::roc(titanic$Survived, -k)
    roc_gbm <- pROC::roc(titanic$Survived, -p_gbm)

    plot(roc_dev)
    par(new=TRUE)
    plot(roc_val, col="red")
    par(new=TRUE)
    plot(roc_gbm, col="blue")

![](README_files/figure-markdown_strict/predict-1.png)

### Predicting on New Data

    v <- rf$ensemble$mod$var.names
    p_sample <- rf$predict(x=head(titanic)[v])
    p_sample

    ## [1] -2.0797191  3.6025955  0.9215585  4.4289147 -1.6492812 -2.3815767

### Exporting SAS Code

    f <- tempfile(fileext = "sas")
    rf$generate_sas_code(file=f, pfx="mod1")

Code sample of first and last 6 lines:

    ## /*** Rule Definitions ***/
    ## 
    ## mod1_rule_001  =  (Pclass in ("3")) and (Sex in ("female")) and (Fare >= 20.8) ;
    ## mod1_rule_002  =  (Sex in ("female")) and (Pclass in ("1","2")) ;
    ## mod1_rule_003  =  (Sex in ("female")) and (Pclass in ("1","2")) ;
    ## mod1_rule_004  =  (Sex in ("female")) and (Pclass in ("1","2")) ;
    ## 
    ## ...
    ## 
    ##   mod1_rule_081 *  0.1230950646 +
    ##   mod1_rule_082 *  0.7217724358 +
    ##   mod1_rule_083 *  0.4237396467 +
    ##   mod1_rule_084 *  0.0385974410 +
    ##   mod1_rule_085 *  0.0060821114
    ## ;

### Using RuleFit with Binnr

    rules <- rf$ensemble$predict_sparse_nodes(titanic[-1])
    rules <- as.matrix(rules[,rf$rids])

    ### create binnr model
    library(binnr)

    ## For binnr cheat sheet run: vignette("binnr-cheat-sheet")

    ## For binnr quick start run: vignette("binnr-quick-start-guide")

    x <- cbind(titanic, rules)
    b <- bin(x, titanic$Survived)

    cl <- b$cluster()
    to_drop <- b$prune_clusters(cl, corr = 0.90, 1)
    b$drop(to_drop)

    b$fit("model1", "model with RuleFit rules", overwrite = TRUE)

    to_drop <- setdiff(names(x), names(titanic))
    b$drop(to_drop)
    b$fit("model2", "model with no rules")

### Top Rules adding to binnr model

    b$select("model1")
    b$sort()
    toprules <- as.integer(row.names(head(b$summary(), 10)))

    ## model1 
    ## Out-of-Fold KS:  0.6792733

    ## Warning: NAs introduced by coercion

    rf$rules[toprules[!is.na(toprules)]]

    ## [[1]]
    ## Sex IN ["female"]
    ## Pclass IN ["1","2"]
    ## 
    ## [[2]]
    ## Sex IN ["male"]
    ## Embarked IN ["Q","S"]
    ## 
    ## [[3]]
    ## Age >= 2.50000
    ## Embarked IN ["C","S"]
    ## Sex IN ["female"]
    ## 
    ## [[4]]
    ## Sex IN ["female"]
    ## Age >= 14.75000
    ## 
    ## [[5]]
    ## Sex IN ["female"]
    ## Fare >= 15.37290
    ## 
    ## [[6]]
    ## Age >= 11.00000
    ## Sex IN ["female"]
    ## Age < 38.50000
    ## 
    ## [[7]]
    ## Sex IN ["female"]
    ## Embarked IN ["C","Q"]
    ## 
    ## [[8]]
    ## SibSp < 2.50000
    ## Pclass IN ["2","3"]
    ## Age < 7.50000
    ## 
    ## [[9]]
    ## SibSp < 2.50000
    ## Fare < 127.81665
    ## Age < 10.00000

### RuleFit and Binnr Lift

RuleFit models can be used to extract nodes and use them alongside
traditional modeling methods such as logistic regression. Here,
supplementing the original dataset with RuleFit rules adds considerable
lift.

    ## Extract the unbiased, K-Fold predictions from the binnr models
    fit1 <- b$models$model1@fit
    p_binnr_rules <- fit1$fit.preval[,which.min(fit1$cvm)]

    fit2 <- b$models$model2@fit
    p_binnr_alone <- fit2$fit.preval[,which.min(fit2$cvm)]

    roc_binnr_rules <- pROC::roc(titanic$Survived, -p_binnr_rules)
    roc_binnr_alone <- pROC::roc(titanic$Survived, -p_binnr_alone)

    plot(roc_binnr_alone)
    par(new=TRUE)
    plot(roc_binnr_rules, col="red")

![](README_files/figure-markdown_strict/compare-methods-1.png)

    par(new=TRUE)
