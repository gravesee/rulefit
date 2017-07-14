
Usage
-----

### Creating a RuleFit Model

A RuleFit model uses a tree ensemble to generate its rules. As such, a
tree ensemble model must be provided to the rulefit function. This
funciton returns a RuleFit object which can be used to mine rules and
train rule ensembles.

    mod <- gbm.fit(titanic[-1], titanic$Survived, distribution="bernoulli",
      interaction.depth=3, shrinkage=0.1, verbose = FALSE)

    rf <- rulefit(mod, n.trees=100)

    print(rf)

    ## RuleFit object with 1000 rules
    ## Rules generated from gbm model show below
    ## --------------------------------------------------------------------------------
    ## A gradient boosted model with bernoulli loss function.
    ## 100 iterations were performed.
    ## There were 7 predictors of which 7 had non-zero influence.

the `rulefit` function wraps a gbm model in a class that manages rule
construction and model fitting. The rules are generated immediately but
the model is not fit until the `train` function is called.

    head(lapply(rf$rules, toString))

    ## [[1]]
    ## [1] ""
    ## 
    ## [[2]]
    ## [1] "Sex IN [\"male\"]"
    ## 
    ## [[3]]
    ## [1] "Sex IN [\"female\"]"
    ## 
    ## [[4]]
    ## [1] "Pclass IN [\"3\"] AND Sex IN [\"female\"]"
    ## 
    ## [[5]]
    ## [1] "Fare < 22.90415 AND Pclass IN [\"3\"] AND Sex IN [\"female\"]"
    ## 
    ## [[6]]
    ## [1] "Fare >= 22.90415 AND Pclass IN [\"3\"] AND Sex IN [\"female\"]"

For ease of programming *every* internal node is generated -- even the
root node. That is why the first rule listed above is empty. Root nodes
are not splits. This was a design decision and does not affect how the
package is used in practice.

### Training

Training a RuleFit model is as easy as calling the train method. The
train method uses the `cv.glmnet` function from the `glmnet` package and
accepts all of the same arguments.

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
<tr class="even">
<td align="left">parallel</td>
<td align="left">TRUE/FALSE to build kfold models in parallel. Require a backend.</td>
</tr>
</tbody>
</table>

    rf <- train(rf, titanic[-1], y = titanic$Survived, family="binomial", keep=T)

### Predicting

Once a RuleFit model is trained. Predictions can be produced by calling
the predict method. As with the train function, `predict` also takes
arguments accepted by `predict.cv.glmnet`. The most important of which
is the lambda parameter, `s`. The default is to use `s="lambda.min"`
which minimizes the out-of-fold error.

Both a score as well as a sparse matrix of rules can be predicted.

    p_rf <- predict(rf, newx = titanic[-1], s="lambda.1se")
    nodes <- predict(rf, newx = titanic[-1], s="lambda.1se", nodes=TRUE)

    head(p_rf)

    ##               1
    ## [1,] -1.9345827
    ## [2,]  2.4238655
    ## [3,] -0.2156901
    ## [4,]  2.9697037
    ## [5,] -1.7711434
    ## [6,] -1.8137063

    head(nodes)

    ## 6 x 40 sparse Matrix of class "ngCMatrix"
    ##                                                                           
    ## [1,] . . . . | . | . . . | . | . . . . . . . . . | . . . | . . . . . . . .
    ## [2,] | | | | . | . | . | . | . | . | . | | . . | | . . . . . . . . | . . .
    ## [3,] . . . . . . . | . . . . . . . . . . . . . . | . . . . . . . | . . . .
    ## [4,] | | | | . | . | . | . . . | . | | | | . . | | . . . . | | . . | . . .
    ## [5,] . . . . | . | . . . | . | . . . . . . . . . | . . . | | . . . . . . .
    ## [6,] . . . . | . | . . . | . | . . . . . . . . . | . . . | . . . . . . . .
    ##               
    ## [1,] | . . . .
    ## [2,] . . . . .
    ## [3,] . . . . .
    ## [4,] . . . . .
    ## [5,] . . . . .
    ## [6,] . . . . .

The out-of-fold predictions can also be extracted if the model was
trained with `keep=TRUE`. Again, this is working with the `cv.glmnet`
API. There is nothing magical going on here:

    p_val <- rf$fit$fit.preval[,match(rf$fit$lambda.1se, rf$fit$lambda)]

#### Comparing RuleFit dev & val to GBM

    p_gbm <- predict(mod, titanic[-1], n.trees = gbm.perf(mod, plot.it = F))

    ## Using OOB method...

    roc_rf <- pROC::roc(titanic$Survived, -p_rf)
    roc_val <- pROC::roc(titanic$Survived, -p_val)
    roc_gbm <- pROC::roc(titanic$Survived, -p_gbm)

    plot(roc_rf)
    par(new=TRUE)
    plot(roc_val, col="blue")
    par(new=TRUE)
    plot(roc_gbm, col="red")

![](README_files/figure-markdown_strict/predict-1.png)

### Rule Summary

RuleFit also provides a summary method to inspect and measure the
coverage of fitted rules.

    s <- summary(rf, s="lambda.1se", dedup=TRUE)
    s

    ##                                                             rule
    ## 1                            SibSp < 1.50000 AND Fare >= 7.13335
    ## 2                            Fare < 52.27710 AND Sex IN ["male"]
    ## 3                            Fare < 52.55000 AND Sex IN ["male"]
    ## 4                      Embarked IN ["Q","S"] AND Sex IN ["male"]
    ## 5                        Pclass IN ["2","3"] AND Sex IN ["male"]
    ## 6                           SibSp < 2.50000 AND Fare >= 15.17290
    ## 7                          Parch < 1.50000 AND Sex IN ["female"]
    ## 8         Sex IN ["male"] AND Fare < 7.91040 AND Parch < 2.50000
    ## 9  Pclass IN ["1","2"] AND Fare >= 7.98750 AND Sex IN ["female"]
    ## 10                     Pclass IN ["1","2"] AND Sex IN ["female"]
    ## 11                            Age < 53.50000 AND Pclass IN ["1"]
    ## 12                           Fare < 29.85000 AND Age >= 36.50000
    ## 13                   Embarked IN ["C","Q"] AND Sex IN ["female"]
    ## 14                            Age < 36.25000 AND Age >= 30.75000
    ## 15        Parch < 0.50000 AND Fare >= 8.17500 AND Age < 25.50000
    ## 16                            Pclass IN ["1"] AND Age < 36.25000
    ## 17                          Fare < 7.98750 AND Sex IN ["female"]
    ## 18                             Age < 5.50000 AND Fare < 84.98750
    ## 19      Fare < 7.88750 AND Pclass IN ["3"] AND Sex IN ["female"]
    ## 20                            Age < 10.00000 AND SibSp < 2.50000
    ## 21                            Age >= 29.50000 AND Fare < 7.84170
    ## 22    Fare >= 23.35000 AND Pclass IN ["3"] AND Sex IN ["female"]
    ## 23                          Fare < 26.77500 AND Fare >= 26.12500
    ## 24                          Fare < 57.48960 AND Fare >= 52.27710
    ## 25       Sex IN ["female"] AND Embarked IN ["Q"] AND Age IS NULL
    ## 26        Sex IN ["male"] AND SibSp < 2.50000 AND Age < 15.50000
    ## 27        Fare < 27.13540 AND Age < 53.50000 AND Pclass IN ["1"]
    ## 28                            Age < 27.50000 AND Age >= 26.50000
    ## 29    Sex IN ["female"] AND Fare >= 14.25415 AND Fare < 15.62085
    ## 30          Age IS NULL AND Fare < 13.98540 AND Fare >= 10.82500
    ##        support   coefficient                               number
    ## 1  0.869809203  0.0004645232                                  264
    ## 2  0.573512907 -0.1818575426                                   53
    ## 3  0.573512907 -0.0006445563                                  103
    ## 4  0.540965208 -0.3221666015                                  493
    ## 5  0.510662177 -0.6250436207                               43, 83
    ## 6  0.432098765  0.1672426599                                  144
    ## 7  0.285072952  0.4016745394                                   67
    ## 8  0.202020202 -0.1208764631                                  734
    ## 9  0.190796857  0.0002289093                                  587
    ## 10 0.190796857  2.0519103753 8, 18, 28, 38, 48, 78, 108, 218, 258
    ## 11 0.173961841  0.0399817043                                  244
    ## 12 0.129068462 -0.1455668253                                  484
    ## 13 0.124579125  0.4468212856                                   98
    ## 14 0.121212121  0.0425628527                                  534
    ## 15 0.105499439 -0.0066801191                                  605
    ## 16 0.102132435  0.1202773843                                  554
    ## 17 0.053872054  0.0666293325                                  584
    ## 18 0.047138047  0.0863324131                                  693
    ## 19 0.047138047  0.1948373068                                   75
    ## 20 0.044893378  0.8103011370                                  383
    ## 21 0.041526375 -0.3032423332                                  784
    ## 22 0.030303030 -0.5591165044                             116, 256
    ## 23 0.029180696  0.1386313554                                  567
    ## 24 0.026936027  0.8298192561                                  177
    ## 25 0.026936027  0.2863394343                                  668
    ## 26 0.024691358  1.8272287990                                  405
    ## 27 0.021324355  0.6636036688                                  245
    ## 28 0.020202020  0.2770159330                                  827
    ## 29 0.019079686 -0.0550543112                                  835
    ## 30 0.003367003  0.7794430962                                  927
