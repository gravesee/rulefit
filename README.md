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
    ## Sex IN ["female"]
    ## Pclass IN ["1","2"]

    tail(rf$rules, 1)

    ## [[1]]
    ## Age >= 24.75000
    ## Parch >= 0.50000

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

    ## [1] -2.4830345  3.6040292  0.5309737  4.7334587 -1.7660612 -2.4831693

### Exporting SAS Code

    f <- tempfile(fileext = "sas")
    rf$generate_sas_code(file=f, pfx="mod1")

Code sample of first and last 6 lines:

    ## /*** Rule Definitions ***/
    ## 
    ## mod1_rule_001  =  (Sex in ("female")) and (Pclass in ("1","2")) ;
    ## mod1_rule_002  =  (Sex in ("male")) ;
    ## mod1_rule_003  =  (Pclass in ("3")) and (Sex in ("female")) and (Fare >= 23.35) ;
    ## mod1_rule_004  =  (Sex in ("female")) and (Pclass in ("1","2")) ;
    ## 
    ## ...
    ## 
    ##   mod1_rule_085 * -0.0468967245 +
    ##   mod1_rule_086 *  0.0018188550 +
    ##   mod1_rule_087 * -0.2504449966 +
    ##   mod1_rule_088 *  0.1020695954 +
    ##   mod1_rule_089 * -0.0703940300
    ## ;
