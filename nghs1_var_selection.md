NGHS1 Variable Selection
================
Daniel O'Leary
October 29, 2018

Setup
=====

``` r
if (!require("pacman")) install.packages("pacman")
```

    ## Loading required package: pacman

``` r
pacman::p_load(tidyverse, haven, broom, glmnet, caret, randomForest)
```

``` r
lassoTrainingData <- 
  read_csv("G:/My Drive/research/projects/phsr1/nghs1/nghs1_data/nghs1_lasso_training_data.csv") %>% 
  as_data_frame()
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double()
    ## )

    ## See spec(...) for full column specifications.

``` r
set.seed(1987)
```

``` r
# Get best result function
getBestResult = function(caret_fit) {
  best = which(rownames(caret_fit$results) == rownames(caret_fit$bestTune))
  best_result = caret_fit$results[best, ]
  rownames(best_result) = NULL
  best_result
}


plotVarSelection <- function(model, data) {
  num_rows <- nrow(summary(coef(model$finalModel, model$bestTune$lambda)))
  
  coef_sum <- summary(coef(model$finalModel, model$bestTune$lambda))[2:num_rows, "i"]
  
  coefs <- 
    data.frame(
      names(data)[coef_sum - 1], 
      summary(coef(model$finalModel, model$bestTune$lambda))[2:num_rows,3]
    )
  
  names(coefs) <- c("var", "coef")
  
  coefs %>% 
    ggplot(aes(fct_reorder(var, coef), coef)) +
    geom_point() + 
    coord_flip()
}
```

``` r
# set up parameters
fitControl <-
  trainControl(
    ## 5-fold CV
    method = "repeatedcv",
    number = 5,
    ## repeated ten times
    repeats = 10
  )

fitControl2 <- 
  trainControl(
    ## 5-fold CV
    method = "repeatedcv",
    number = 5,
    ## repeated ten times
    repeats = 10
  )
  
trainGrid <-
  expand.grid(
    alpha = 1,
    lambda = 10^seq(10, -2, length = 100)
  )
```

Variable selection
==================

Selection 1: Negative Emotions Predicting Change in BMI
-------------------------------------------------------

Let's start with the two scales in the dataset that measure emotions, the **Manifest Anxiety Scale** and the **Perceived Stress Scale**. We'll only look at variables up to year 5 because we want to be able to predict the eating disorders inventory at year 5 with these variables.

``` r
# Select data
psychTrain1 <-
  lassoTrainingData %>% 
  select(
    y10_QI,
    y2_STRESS,
    y4_STRESS,
    y3_ANXR,
    y5_ANXR
  ) %>% 
  filter_all(all_vars(!is.na(.)))

psychTrain1X <- 
  psychTrain1 %>% 
  select(-y10_QI)

psychTrain1Y <-
  psychTrain1 %>% 
  select(y10_QI)
```

``` r
lassoFitPsych1 <- 
  train(
    psychTrain1X, 
    psychTrain1Y$y10_QI, 
    method = "glmnet", 
    trControl = fitControl2, 
    preProc = c('scale','center'), 
    tuneGrid = trainGrid
  )

elnetFitPsych1 <-
  train(
    psychTrain1X, 
    psychTrain1Y$y10_QI, 
    method = "glmnet", 
    trControl = fitControl2, 
    preProc = c('scale','center') 
  )

models <- list(lasso = lassoFitPsych1, elastic = elnetFitPsych1)
resamples(models) %>% summary()
```

    ## 
    ## Call:
    ## summary.resamples(object = .)
    ## 
    ## Models: lasso, elastic 
    ## Number of resamples: 50 
    ## 
    ## MAE 
    ##             Min.  1st Qu.   Median     Mean  3rd Qu.     Max. NA's
    ## lasso   4.611327 4.988858 5.133399 5.118739 5.243533 5.494381    0
    ## elastic 4.640500 4.981273 5.123511 5.118386 5.279325 5.431407    0
    ## 
    ## RMSE 
    ##             Min.  1st Qu.   Median     Mean  3rd Qu.     Max. NA's
    ## lasso   5.886386 6.422192 6.712256 6.667679 6.867972 7.292393    0
    ## elastic 5.752687 6.388774 6.716784 6.664006 6.946779 7.322064    0
    ## 
    ## Rsquared 
    ##                 Min.    1st Qu.    Median       Mean    3rd Qu.       Max.
    ## lasso   0.0043422308 0.01553104 0.0336188 0.03524935 0.04642245 0.08325144
    ## elastic 0.0001648749 0.01716780 0.0359231 0.03857624 0.06007053 0.09091933
    ##         NA's
    ## lasso      0
    ## elastic    0

``` r
getBestResult(lassoFitPsych1)
```

    ##   alpha    lambda     RMSE   Rsquared      MAE    RMSESD RsquaredSD
    ## 1     1 0.2154435 6.667679 0.03524935 5.118739 0.3218946 0.02250594
    ##       MAESD
    ## 1 0.1904052

``` r
getBestResult(elnetFitPsych1)
```

    ##   alpha    lambda     RMSE   Rsquared      MAE    RMSESD RsquaredSD
    ## 1     1 0.2497193 6.664006 0.03857624 5.118386 0.3893367 0.02607068
    ##       MAESD
    ## 1 0.2012004

Elnet selected alpha = 1 which is equivalent to LASSO.

``` r
coef(elnetFitPsych1$finalModel, elnetFitPsych1$bestTune$lambda)
```

    ## 5 x 1 sparse Matrix of class "dgCMatrix"
    ##                      1
    ## (Intercept) 25.6899613
    ## y2_STRESS    .        
    ## y4_STRESS    .        
    ## y3_ANXR      0.9995076
    ## y5_ANXR      .

``` r
plotVarSelection(elnetFitPsych1, psychTrain1X)
```

![](nghs1_var_selection_files/figure-markdown_github/unnamed-chunk-3-1.png)

Year 3 anxiety is the only variable selected by the model. We will compare this to year 2 stress for theoretical reasons.

Selection 2: Income Predicting Negative Emotions
------------------------------------------------

Let's look at which of these emotion variables are predicted by income.

``` r
# Select data
psychTrain2 <-
  lassoTrainingData %>% 
  select(
    y0_CATINC,
    y2_STRESS,
    y4_STRESS,
    y3_ANXR,
    y5_ANXR
  ) %>% 
  filter_all(all_vars(!is.na(.)))

psychTrain2X <- 
  psychTrain2 %>% 
  select(-y0_CATINC)

psychTrain2Y <-
  psychTrain2 %>% 
  select(y0_CATINC)
```

``` r
lassoFitPsych2 <- 
  train(
    psychTrain2X, 
    psychTrain2Y$y0_CATINC, 
    method = "glmnet", 
    trControl = fitControl2, 
    preProc = c('scale','center'), 
    tuneGrid = trainGrid
  )

elnetFitPsych2 <-
  train(
    psychTrain2X, 
    psychTrain2Y$y0_CATINC, 
    method = "glmnet", 
    trControl = fitControl2, 
    preProc = c('scale','center') 
  )

models <- list(lasso = lassoFitPsych2, elastic = elnetFitPsych2)
resamples(models) %>% summary()
```

    ## 
    ## Call:
    ## summary.resamples(object = .)
    ## 
    ## Models: lasso, elastic 
    ## Number of resamples: 50 
    ## 
    ## MAE 
    ##              Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
    ## lasso   0.8958362 0.9269862 0.9374084 0.9400723 0.9530132 0.9813451    0
    ## elastic 0.8710831 0.9219501 0.9420361 0.9394480 0.9541520 0.9902400    0
    ## 
    ## RMSE 
    ##             Min.  1st Qu.   Median     Mean  3rd Qu.     Max. NA's
    ## lasso   1.070984 1.103264 1.117154 1.119159 1.134122 1.168208    0
    ## elastic 1.032397 1.107166 1.121471 1.118533 1.131975 1.175961    0
    ## 
    ## Rsquared 
    ##                 Min.     1st Qu.     Median       Mean    3rd Qu.
    ## lasso   1.098969e-04 0.003782882 0.01084215 0.01771001 0.02982806
    ## elastic 9.956058e-05 0.004540029 0.01055438 0.01656640 0.02215142
    ##               Max. NA's
    ## lasso   0.05618823    0
    ## elastic 0.05898367    0

``` r
getBestResult(lassoFitPsych2)
```

    ##   alpha     lambda     RMSE   Rsquared       MAE     RMSESD RsquaredSD
    ## 1     1 0.03053856 1.119159 0.01771001 0.9400723 0.02283717 0.01731245
    ##       MAESD
    ## 1 0.0191511

``` r
getBestResult(elnetFitPsych2)
```

    ##   alpha     lambda     RMSE  Rsquared      MAE     RMSESD RsquaredSD
    ## 1     1 0.02713804 1.118533 0.0165664 0.939448 0.02644431 0.01604246
    ##        MAESD
    ## 1 0.02177303

Elnet selected alpha = 1 which is equivalent to LASSO.

``` r
coef(lassoFitPsych2$finalModel, lassoFitPsych2$bestTune$lambda)
```

    ## 5 x 1 sparse Matrix of class "dgCMatrix"
    ##                       1
    ## (Intercept)  2.85498108
    ## y2_STRESS   -0.03629176
    ## y4_STRESS    .         
    ## y3_ANXR     -0.09056794
    ## y5_ANXR      .

``` r
plotVarSelection(lassoFitPsych2, psychTrain2X)
```

![](nghs1_var_selection_files/figure-markdown_github/unnamed-chunk-6-1.png)

Higher income is associated with lower year 3 anxiety and lower year 2 stress.

Selection 3: Eating Surveys Predicting BMI
------------------------------------------

Next, we'll look at the psychological variables that we're most interested in - ones that are somewhat related to eating. For this step, we have the **Eating Disorders Inventory** and the **Emotional Eating Index**. If we want to go with one year's worth of variables from the first scale, it seems like it makes sense to go with year 5 as this is the only year after the anxiety score for which we currently have all the data. We'll look at

``` r
# Select data
psychTrain3a <-
  lassoTrainingData %>% 
  dplyr::select(
    y10_QI,
    y5_PERFCT,
    y5_AWARE,
    y5_BULIM,
    y5_DISTRST,
    y5_INEFCT,
    y5_FEARS,
    contains("_eat_WORRYMR"),
    contains("_eat_SADMR"),
    contains("_eat_HAPPYMR"),
    contains("_eat_BOREDMR"),
    contains("_eat_REWARD"),
    contains("_eat_MAD"),
    -contains("y0"),
    -contains("y2"),
    -contains("y3")
  )

psychTrain3Xa <- 
  psychTrain3a %>% 
  dplyr::select(-y10_QI)

psychTrain3Ya <-
  psychTrain3a %>% 
  dplyr::select(y10_QI)
```

``` r
lassoFitPsych3a <- 
  train(
    psychTrain3Xa, 
    psychTrain3Ya$y10_QI, 
    method = "glmnet", 
    trControl = fitControl2, 
    preProc = c('scale','center'), 
    tuneGrid = trainGrid
  )

elnetFitPsych3a <-
  train(
    psychTrain3Xa, 
    psychTrain3Ya$y10_QI, 
    method = "glmnet", 
    trControl = fitControl2, 
    preProc = c('scale','center') 
  )

models <- list(lasso = lassoFitPsych3a, elastic = elnetFitPsych3a)
resamples(models) %>% summary()
```

    ## 
    ## Call:
    ## summary.resamples(object = .)
    ## 
    ## Models: lasso, elastic 
    ## Number of resamples: 50 
    ## 
    ## MAE 
    ##             Min.  1st Qu.   Median     Mean  3rd Qu.     Max. NA's
    ## lasso   4.573436 4.981640 5.148427 5.125950 5.244114 5.808048    0
    ## elastic 4.578789 4.971902 5.104362 5.122448 5.311009 5.659327    0
    ## 
    ## RMSE 
    ##             Min.  1st Qu.   Median     Mean  3rd Qu.     Max. NA's
    ## lasso   5.595913 6.405619 6.671328 6.631894 6.896816 7.325150    0
    ## elastic 5.945201 6.293953 6.584353 6.623858 6.909768 7.817223    0
    ## 
    ## Rsquared 
    ##                Min.    1st Qu.     Median       Mean    3rd Qu.      Max.
    ## lasso   0.004362874 0.02963180 0.04652204 0.04695640 0.06002297 0.1335476
    ## elastic 0.004623593 0.02418363 0.04590527 0.05060947 0.07659945 0.1496782
    ##         NA's
    ## lasso      0
    ## elastic    0

``` r
getBestResult(lassoFitPsych3a)
```

    ##   alpha    lambda     RMSE  Rsquared     MAE    RMSESD RsquaredSD
    ## 1     1 0.2154435 6.631894 0.0469564 5.12595 0.3566048 0.02757924
    ##       MAESD
    ## 1 0.2281727

``` r
getBestResult(elnetFitPsych3a)
```

    ##   alpha    lambda     RMSE   Rsquared      MAE    RMSESD RsquaredSD
    ## 1     1 0.2517374 6.623858 0.05060947 5.122448 0.4203304 0.03235578
    ##       MAESD
    ## 1 0.2375379

``` r
coef(lassoFitPsych3a$finalModel, lassoFitPsych3a$bestTune$lambda)
```

    ## 39 x 1 sparse Matrix of class "dgCMatrix"
    ##                           1
    ## (Intercept)    25.689961334
    ## y5_PERFCT       .          
    ## y5_AWARE        0.937872504
    ## y5_BULIM        .          
    ## y5_DISTRST      0.145725815
    ## y5_INEFCT       0.475634131
    ## y5_FEARS        .          
    ## y4_eat_WORRYMR  .          
    ## y5_eat_WORRYMR  .          
    ## y6_eat_WORRYMR  0.377880793
    ## y7_eat_WORRYMR  .          
    ## y9_eat_WORRYMR  .          
    ## y4_eat_SADMR    .          
    ## y5_eat_SADMR    .          
    ## y6_eat_SADMR    .          
    ## y7_eat_SADMR    .          
    ## y9_eat_SADMR    0.167983179
    ## y4_eat_HAPPYMR  .          
    ## y5_eat_HAPPYMR  .          
    ## y6_eat_HAPPYMR -0.147451458
    ## y7_eat_HAPPYMR -0.128700303
    ## y9_eat_HAPPYMR -0.115925390
    ## y4_eat_BOREDMR  .          
    ## y5_eat_BOREDMR -0.614694357
    ## y6_eat_BOREDMR -0.001388244
    ## y7_eat_BOREDMR -0.156025836
    ## y9_eat_BOREDMR  .          
    ## y4_eat_REWARD  -0.095095646
    ## y5_eat_REWARD  -0.110178495
    ## y6_eat_REWARD   .          
    ## y7_eat_REWARD   .          
    ## y9_eat_REWARD   .          
    ## y4_eat_MAD1     .          
    ## y4_eat_MAD2     .          
    ## y5_eat_MAD1     .          
    ## y5_eat_MAD2    -0.033743970
    ## y6_eat_MADMR    .          
    ## y7_eat_MADMR   -0.128257963
    ## y9_eat_MADMR    .

``` r
plotVarSelection(lassoFitPsych3a, psychTrain3Xa)
```

![](nghs1_var_selection_files/figure-markdown_github/unnamed-chunk-9-1.png)

Selection 4: Income Predicting Eating Surveys
---------------------------------------------

Now let's see which of these variables are associated with income.

``` r
# Select data
psychTrain5X <-
  lassoTrainingData %>% 
  dplyr::select(
    y5_PERFCT,
    y5_AWARE,
    y5_BULIM,
    y5_DISTRST,
    y5_INEFCT,
    y5_FEARS,
    contains("_eat_WORRYMR"),
    contains("_eat_SADMR"),
    contains("_eat_HAPPYMR"),
    contains("_eat_BOREDMR"),
    contains("_eat_REWARD"),
    contains("_eat_MAD"),
    -contains("y0"),
    -contains("y2"),
    -contains("y3")
  )

psychTrain5Y <-
  lassoTrainingData %>% 
  dplyr::select(y0_CATINC)
```

``` r
lassoFitPsych5 <- 
  train(
    psychTrain5X, 
    psychTrain5Y$y0_CATINC, 
    method = "glmnet", 
    trControl = fitControl2, 
    preProc = c('scale','center'), 
    tuneGrid = trainGrid
  )

elnetFitPsych5 <-
  train(
    psychTrain5X, 
    psychTrain5Y$y0_CATINC, 
    method = "glmnet", 
    trControl = fitControl2, 
    preProc = c('scale','center') 
  )

models <- list(lasso = lassoFitPsych5, elastic = elnetFitPsych5)
resamples(models) %>% summary()
```

    ## 
    ## Call:
    ## summary.resamples(object = .)
    ## 
    ## Models: lasso, elastic 
    ## Number of resamples: 50 
    ## 
    ## MAE 
    ##              Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
    ## lasso   0.8654761 0.8971513 0.9073258 0.9100555 0.9269050 0.9561085    0
    ## elastic 0.8576579 0.8916924 0.9070620 0.9065978 0.9239106 0.9620397    0
    ## 
    ## RMSE 
    ##             Min.  1st Qu.   Median     Mean  3rd Qu.     Max. NA's
    ## lasso   1.031544 1.073790 1.087828 1.087381 1.103135 1.145592    0
    ## elastic 1.032254 1.071633 1.088670 1.085495 1.102762 1.150051    0
    ## 
    ## Rsquared 
    ##               Min.    1st Qu.     Median       Mean    3rd Qu.      Max.
    ## lasso   0.01113038 0.05017802 0.06863003 0.07108061 0.08900610 0.1778541
    ## elastic 0.01099121 0.05114171 0.06397872 0.07458152 0.09108905 0.1662204
    ##         NA's
    ## lasso      0
    ## elastic    0

``` r
getBestResult(lassoFitPsych5)
```

    ##   alpha     lambda     RMSE   Rsquared       MAE     RMSESD RsquaredSD
    ## 1     1 0.04037017 1.087381 0.07108061 0.9100555 0.02557559 0.03442164
    ##        MAESD
    ## 1 0.02115109

``` r
getBestResult(elnetFitPsych5)
```

    ##   alpha     lambda     RMSE   Rsquared       MAE     RMSESD RsquaredSD
    ## 1     1 0.05256562 1.085495 0.07458152 0.9065978 0.02620748 0.03555888
    ##        MAESD
    ## 1 0.02339314

``` r
coef(elnetFitPsych5$finalModel, elnetFitPsych5$bestTune$lambda)
```

    ## 39 x 1 sparse Matrix of class "dgCMatrix"
    ##                           1
    ## (Intercept)     2.854981084
    ## y5_PERFCT       .          
    ## y5_AWARE       -0.185570646
    ## y5_BULIM       -0.011401154
    ## y5_DISTRST     -0.070783241
    ## y5_INEFCT       .          
    ## y5_FEARS       -0.040411843
    ## y4_eat_WORRYMR  .          
    ## y5_eat_WORRYMR  .          
    ## y6_eat_WORRYMR  .          
    ## y7_eat_WORRYMR  .          
    ## y9_eat_WORRYMR  .          
    ## y4_eat_SADMR    .          
    ## y5_eat_SADMR    .          
    ## y6_eat_SADMR    .          
    ## y7_eat_SADMR    .          
    ## y9_eat_SADMR    .          
    ## y4_eat_HAPPYMR  .          
    ## y5_eat_HAPPYMR  .          
    ## y6_eat_HAPPYMR  .          
    ## y7_eat_HAPPYMR  .          
    ## y9_eat_HAPPYMR  .          
    ## y4_eat_BOREDMR  .          
    ## y5_eat_BOREDMR  0.008152293
    ## y6_eat_BOREDMR  0.038126657
    ## y7_eat_BOREDMR  0.031870615
    ## y9_eat_BOREDMR  0.073578473
    ## y4_eat_REWARD  -0.021334968
    ## y5_eat_REWARD   .          
    ## y6_eat_REWARD   .          
    ## y7_eat_REWARD   .          
    ## y9_eat_REWARD   .          
    ## y4_eat_MAD1     .          
    ## y4_eat_MAD2     .          
    ## y5_eat_MAD1     .          
    ## y5_eat_MAD2     .          
    ## y6_eat_MADMR   -0.022977529
    ## y7_eat_MADMR    .          
    ## y9_eat_MADMR    .

``` r
plotVarSelection(elnetFitPsych5, psychTrain5X)
```

![](nghs1_var_selection_files/figure-markdown_github/unnamed-chunk-12-1.png) Higher levels of income are associated with lower levels of AWARE, lower levels of DISTRST.

Selection 5: Stress Predicting Eating Surveys
---------------------------------------------

Now let's see which of these variables are associated with our stress scale.

``` r
# Select data
psychTrain4dX <-
  lassoTrainingData %>% 
  dplyr::select(
    y5_PERFCT,
    y5_AWARE,
    y5_BULIM,
    y5_DISTRST,
    y5_INEFCT,
    y5_FEARS,
    contains("_eat_WORRYMR"),
    contains("_eat_SADMR"),
    contains("_eat_HAPPYMR"),
    contains("_eat_BOREDMR"),
    contains("_eat_REWARD"),
    contains("_eat_MAD"),
    -contains("y0"),
    -contains("y2"),
    -contains("y3")
  )

psychTrain4dY <-
  lassoTrainingData %>% 
  dplyr::select(y2_STRESS)
```

``` r
lassoFitPsych4d <- 
  train(
    psychTrain4dX, 
    psychTrain4dY$y2_STRESS, 
    method = "glmnet", 
    trControl = fitControl2, 
    preProc = c('scale','center'), 
    tuneGrid = trainGrid
  )

elnetFitPsych4d <-
  train(
    psychTrain4dX, 
    psychTrain4dY$y2_STRESS, 
    method = "glmnet", 
    trControl = fitControl2, 
    preProc = c('scale','center') 
  )

models <- list(lasso = lassoFitPsych4d, elastic = elnetFitPsych4d)
resamples(models) %>% summary()
```

    ## 
    ## Call:
    ## summary.resamples(object = .)
    ## 
    ## Models: lasso, elastic 
    ## Number of resamples: 50 
    ## 
    ## MAE 
    ##             Min.  1st Qu.   Median     Mean  3rd Qu.     Max. NA's
    ## lasso   4.365634 4.922288 5.131948 5.117775 5.304237 5.663409    0
    ## elastic 4.724506 4.977068 5.099794 5.113137 5.263533 5.585069    0
    ## 
    ## RMSE 
    ##             Min.  1st Qu.   Median     Mean  3rd Qu.     Max. NA's
    ## lasso   5.605089 6.346056 6.640816 6.628190 6.890208 7.335886    0
    ## elastic 6.007508 6.385638 6.583522 6.624935 6.877682 7.321096    0
    ## 
    ## Rsquared 
    ##               Min.    1st Qu.     Median       Mean   3rd Qu.      Max.
    ## lasso   0.01788253 0.05610943 0.08369065 0.09098774 0.1146068 0.2245513
    ## elastic 0.02088177 0.05801517 0.10219283 0.09171133 0.1176167 0.1797123
    ##         NA's
    ## lasso      0
    ## elastic    0

``` r
getBestResult(lassoFitPsych4d)
```

    ##   alpha    lambda    RMSE   Rsquared      MAE    RMSESD RsquaredSD
    ## 1     1 0.3764936 6.62819 0.09098774 5.117775 0.3828009 0.04473561
    ##       MAESD
    ## 1 0.2658845

``` r
getBestResult(elnetFitPsych4d)
```

    ##   alpha    lambda     RMSE   Rsquared      MAE    RMSESD RsquaredSD
    ## 1     1 0.3834098 6.624935 0.09171133 5.113137 0.3307415 0.03668047
    ##       MAESD
    ## 1 0.2092122

``` r
coef(elnetFitPsych4d$finalModel, elnetFitPsych4d$bestTune$lambda)
```

    ## 39 x 1 sparse Matrix of class "dgCMatrix"
    ##                          1
    ## (Intercept)    24.69363178
    ## y5_PERFCT       .         
    ## y5_AWARE        1.13745555
    ## y5_BULIM        .         
    ## y5_DISTRST      0.31001871
    ## y5_INEFCT       0.39653696
    ## y5_FEARS        .         
    ## y4_eat_WORRYMR  .         
    ## y5_eat_WORRYMR  .         
    ## y6_eat_WORRYMR  .         
    ## y7_eat_WORRYMR  0.14621904
    ## y9_eat_WORRYMR  .         
    ## y4_eat_SADMR    0.11750553
    ## y5_eat_SADMR    .         
    ## y6_eat_SADMR    .         
    ## y7_eat_SADMR    0.10517551
    ## y9_eat_SADMR    .         
    ## y4_eat_HAPPYMR  .         
    ## y5_eat_HAPPYMR  .         
    ## y6_eat_HAPPYMR  .         
    ## y7_eat_HAPPYMR  .         
    ## y9_eat_HAPPYMR  .         
    ## y4_eat_BOREDMR  0.14990795
    ## y5_eat_BOREDMR  .         
    ## y6_eat_BOREDMR  .         
    ## y7_eat_BOREDMR  0.21834758
    ## y9_eat_BOREDMR  .         
    ## y4_eat_REWARD   .         
    ## y5_eat_REWARD   .         
    ## y6_eat_REWARD   .         
    ## y7_eat_REWARD   .         
    ## y9_eat_REWARD   .         
    ## y4_eat_MAD1     0.08415741
    ## y4_eat_MAD2     .         
    ## y5_eat_MAD1     .         
    ## y5_eat_MAD2     .         
    ## y6_eat_MADMR    .         
    ## y7_eat_MADMR    0.07833096
    ## y9_eat_MADMR    .

``` r
plotVarSelection(elnetFitPsych4d, psychTrain4dX)
```

![](nghs1_var_selection_files/figure-markdown_github/unnamed-chunk-15-1.png)

Selection 6: Anxiety Predicting Eating Surveys
----------------------------------------------

Now let's see which of these variables are associated with our anxiety scale.

``` r
# Select data
psychTrain4X <-
  lassoTrainingData %>% 
  dplyr::select(
    y5_PERFCT,
    y5_AWARE,
    y5_BULIM,
    y5_DISTRST,
    y5_INEFCT,
    y5_FEARS,
    contains("_eat_WORRYMR"),
    contains("_eat_SADMR"),
    contains("_eat_HAPPYMR"),
    contains("_eat_BOREDMR"),
    contains("_eat_REWARD"),
    contains("_eat_MAD"),
    -contains("y0"),
    -contains("y2"),
    -contains("y3")
  )

psychTrain4Y <-
  lassoTrainingData %>% 
  dplyr::select(y3_ANXR)
```

``` r
lassoFitPsych4 <- 
  train(
    psychTrain4X, 
    psychTrain4Y$y3_ANXR, 
    method = "glmnet", 
    trControl = fitControl2, 
    preProc = c('scale','center'), 
    tuneGrid = trainGrid
  )

elnetFitPsych4 <-
  train(
    psychTrain4X, 
    psychTrain4Y$y3_ANXR, 
    method = "glmnet", 
    trControl = fitControl2, 
    preProc = c('scale','center') 
  )

models <- list(lasso = lassoFitPsych4, elastic = elnetFitPsych4)
resamples(models) %>% summary()
```

    ## 
    ## Call:
    ## summary.resamples(object = .)
    ## 
    ## Models: lasso, elastic 
    ## Number of resamples: 50 
    ## 
    ## MAE 
    ##             Min.  1st Qu.   Median     Mean  3rd Qu.     Max. NA's
    ## lasso   4.440182 4.689945 4.776456 4.785973 4.896805 5.169641    0
    ## elastic 4.414997 4.695557 4.777640 4.806243 4.951858 5.143048    0
    ## 
    ## RMSE 
    ##             Min.  1st Qu.   Median     Mean  3rd Qu.     Max. NA's
    ## lasso   5.488953 5.670559 5.809610 5.817672 5.961913 6.212471    0
    ## elastic 5.290515 5.691485 5.838593 5.836382 5.941523 6.296655    0
    ## 
    ## Rsquared 
    ##               Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
    ## lasso   0.07830947 0.1747121 0.2008263 0.2062969 0.2442689 0.3366817    0
    ## elastic 0.10644972 0.1683900 0.1941884 0.1991311 0.2357080 0.2985277    0

``` r
getBestResult(lassoFitPsych4)
```

    ##   alpha    lambda     RMSE  Rsquared      MAE    RMSESD RsquaredSD
    ## 1     1 0.2154435 5.817672 0.2062969 4.785973 0.1935478 0.05001568
    ##       MAESD
    ## 1 0.1567834

``` r
getBestResult(elnetFitPsych4)
```

    ##   alpha    lambda     RMSE  Rsquared      MAE    RMSESD RsquaredSD
    ## 1  0.55 0.5219682 5.836382 0.1991311 4.806243 0.1974746 0.04724354
    ##       MAESD
    ## 1 0.1622117

``` r
coef(lassoFitPsych4$finalModel, lassoFitPsych4$bestTune$lambda)
```

    ## 39 x 1 sparse Matrix of class "dgCMatrix"
    ##                           1
    ## (Intercept)    10.772799496
    ## y5_PERFCT       .          
    ## y5_AWARE        1.607659397
    ## y5_BULIM        .          
    ## y5_DISTRST      0.350803094
    ## y5_INEFCT       0.983351586
    ## y5_FEARS        .          
    ## y4_eat_WORRYMR  0.178991860
    ## y5_eat_WORRYMR -0.036208298
    ## y6_eat_WORRYMR  .          
    ## y7_eat_WORRYMR  0.157472078
    ## y9_eat_WORRYMR  .          
    ## y4_eat_SADMR    0.178137660
    ## y5_eat_SADMR    .          
    ## y6_eat_SADMR    .          
    ## y7_eat_SADMR    .          
    ## y9_eat_SADMR    .          
    ## y4_eat_HAPPYMR  0.168236191
    ## y5_eat_HAPPYMR  .          
    ## y6_eat_HAPPYMR  .          
    ## y7_eat_HAPPYMR  .          
    ## y9_eat_HAPPYMR  .          
    ## y4_eat_BOREDMR  0.127383983
    ## y5_eat_BOREDMR  .          
    ## y6_eat_BOREDMR  .          
    ## y7_eat_BOREDMR  0.047821297
    ## y9_eat_BOREDMR  .          
    ## y4_eat_REWARD   0.042863624
    ## y5_eat_REWARD   0.279546128
    ## y6_eat_REWARD   0.337895515
    ## y7_eat_REWARD   .          
    ## y9_eat_REWARD   .          
    ## y4_eat_MAD1     0.051892084
    ## y4_eat_MAD2     .          
    ## y5_eat_MAD1     .          
    ## y5_eat_MAD2    -0.204388266
    ## y6_eat_MADMR    0.006958773
    ## y7_eat_MADMR    0.217439475
    ## y9_eat_MADMR    .

``` r
plotVarSelection(lassoFitPsych4, psychTrain4X)
```

![](nghs1_var_selection_files/figure-markdown_github/unnamed-chunk-18-1.png)

The variables most consistently selected across 3 - 6 are year 5 interoceptive awareness, year 5 ineffectiveness, and year 5 interpersonal distrust.

Selection 7: Assorted Psychological Variables Predicting BMI
------------------------------------------------------------

In this next step, we'll do sort of a grab bag of other psychological variables. What's left in this domain are the **Coping Strategies Inventory** and the **The Perceived Competence Scale for Children**.

``` r
# Select data
psychTrain7 <-
  lassoTrainingData %>% 
  dplyr::select(
    y10_QI,
    y4_CR1,
    y6_CR1,
    y8_CR1,
    y4_EE1,
    y6_EE1,
    y8_EE1,
    y4_SCRT1,
    y6_SCRT1,
    y8_SCRT1,
    y5_SCHCP, 
    y5_BEHCD, 
    y5_SELWT
  )

psychTrain7X <- 
  psychTrain7 %>% 
  dplyr::select(-y10_QI)

psychTrain7Y <-
  psychTrain7 %>% 
  dplyr::select(y10_QI)
```

``` r
lassoFitPsych7 <- 
  train(
    psychTrain7X, 
    psychTrain7Y$y10_QI, 
    method = "glmnet", 
    trControl = fitControl2, 
    preProc = c('scale','center'), 
    tuneGrid = trainGrid
  )

elnetFitPsych7 <-
  train(
    psychTrain7X, 
    psychTrain7Y$y10_QI, 
    method = "glmnet", 
    trControl = fitControl2, 
    preProc = c('scale','center') 
  )

models <- list(lasso = lassoFitPsych7, elastic = elnetFitPsych7)
resamples(models) %>% summary()
```

    ## 
    ## Call:
    ## summary.resamples(object = .)
    ## 
    ## Models: lasso, elastic 
    ## Number of resamples: 50 
    ## 
    ## MAE 
    ##             Min.  1st Qu.   Median     Mean  3rd Qu.     Max. NA's
    ## lasso   4.722202 5.014945 5.176411 5.165628 5.322198 5.662845    0
    ## elastic 4.613256 5.013471 5.150593 5.167416 5.324997 5.592957    0
    ## 
    ## RMSE 
    ##             Min.  1st Qu.   Median     Mean  3rd Qu.     Max. NA's
    ## lasso   5.946107 6.464127 6.680414 6.684064 6.970134 7.478494    0
    ## elastic 5.875388 6.392098 6.661880 6.687742 6.991928 7.591171    0
    ## 
    ## Rsquared 
    ##                Min.    1st Qu.     Median       Mean    3rd Qu.      Max.
    ## lasso   0.001438266 0.01406465 0.03244022 0.03360841 0.05029738 0.0995979
    ## elastic 0.001314725 0.01111030 0.02164966 0.03017760 0.04762983 0.1235061
    ##         NA's
    ## lasso      0
    ## elastic    0

``` r
getBestResult(lassoFitPsych7)
```

    ##   alpha    lambda     RMSE   Rsquared      MAE    RMSESD RsquaredSD
    ## 1     1 0.3764936 6.684064 0.03360841 5.165628 0.3670939 0.02333509
    ##       MAESD
    ## 1 0.2089791

``` r
getBestResult(elnetFitPsych7)
```

    ##   alpha    lambda     RMSE  Rsquared      MAE    RMSESD RsquaredSD
    ## 1     1 0.2327007 6.687742 0.0301776 5.167416 0.4144637 0.02568236
    ##       MAESD
    ## 1 0.2254122

``` r
coef(lassoFitPsych7$finalModel, lassoFitPsych7$bestTune$lambda)
```

    ## 13 x 1 sparse Matrix of class "dgCMatrix"
    ##                      1
    ## (Intercept) 25.6899613
    ## y4_CR1       .        
    ## y6_CR1       .        
    ## y8_CR1       .        
    ## y4_EE1       .        
    ## y6_EE1       .        
    ## y8_EE1       .        
    ## y4_SCRT1     .        
    ## y6_SCRT1     .        
    ## y8_SCRT1     .        
    ## y5_SCHCP     .        
    ## y5_BEHCD     .        
    ## y5_SELWT    -0.7875069

``` r
plotVarSelection(lassoFitPsych7, psychTrain7X)
```

![](nghs1_var_selection_files/figure-markdown_github/unnamed-chunk-21-1.png)

Selection 8: Income Predicting Assorted Psychological Variables
---------------------------------------------------------------

``` r
# Select data
psychTrain6 <-
  lassoTrainingData %>% 
  dplyr::select(
    y0_CATINC,
    y4_CR1,
    y6_CR1,
    y8_CR1,
    y4_EE1,
    y6_EE1,
    y8_EE1,
    y4_SCRT1,
    y6_SCRT1,
    y8_SCRT1,
    y5_SCHCP, 
    y5_BEHCD, 
    y5_SELWT
  )

psychTrain6X <- 
  psychTrain6 %>% 
  dplyr::select(-y0_CATINC)

psychTrain6Y <-
  psychTrain6 %>% 
  dplyr::select(y0_CATINC)
```

``` r
lassoFitPsych6 <- 
  train(
    psychTrain6X, 
    psychTrain6Y$y0_CATINC, 
    method = "glmnet", 
    trControl = fitControl2, 
    preProc = c('scale','center'), 
    tuneGrid = trainGrid
  )

elnetFitPsych6 <-
  train(
    psychTrain6X, 
    psychTrain6Y$y0_CATINC, 
    method = "glmnet", 
    trControl = fitControl2, 
    preProc = c('scale','center') 
  )

models <- list(lasso = lassoFitPsych6, elastic = elnetFitPsych6)
resamples(models) %>% summary()
```

    ## 
    ## Call:
    ## summary.resamples(object = .)
    ## 
    ## Models: lasso, elastic 
    ## Number of resamples: 50 
    ## 
    ## MAE 
    ##              Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
    ## lasso   0.8773926 0.9204100 0.9484773 0.9412171 0.9621514 0.9989445    0
    ## elastic 0.8799785 0.9240597 0.9395650 0.9399068 0.9555552 0.9830553    0
    ## 
    ## RMSE 
    ##             Min.  1st Qu.   Median     Mean  3rd Qu.     Max. NA's
    ## lasso   1.037559 1.094092 1.125024 1.118490 1.144545 1.186282    0
    ## elastic 1.041040 1.097762 1.118810 1.116072 1.137232 1.172283    0
    ## 
    ## Rsquared 
    ##                 Min.     1st Qu.     Median       Mean    3rd Qu.
    ## lasso   0.0001557405 0.008616656 0.01519910 0.02274202 0.03528076
    ## elastic 0.0011132926 0.012370460 0.02294968 0.02467845 0.03209134
    ##               Max. NA's
    ## lasso   0.07449438    0
    ## elastic 0.07705853    0

``` r
getBestResult(lassoFitPsych6)
```

    ##   alpha lambda    RMSE   Rsquared       MAE    RMSESD RsquaredSD
    ## 1     1   0.01 1.11849 0.02274202 0.9412171 0.0335905  0.0209412
    ##        MAESD
    ## 1 0.02891767

``` r
getBestResult(elnetFitPsych6)
```

    ##   alpha     lambda     RMSE   Rsquared       MAE     RMSESD RsquaredSD
    ## 1   0.1 0.02015427 1.116072 0.02467845 0.9399068 0.02804794 0.01752052
    ##        MAESD
    ## 1 0.02287466

``` r
coef(elnetFitPsych6$finalModel, elnetFitPsych6$bestTune$lambda)
```

    ## 13 x 1 sparse Matrix of class "dgCMatrix"
    ##                       1
    ## (Intercept)  2.85498108
    ## y4_CR1       0.08468824
    ## y6_CR1       0.04220232
    ## y8_CR1       0.02898287
    ## y4_EE1      -0.08184202
    ## y6_EE1       0.05696929
    ## y8_EE1      -0.05499374
    ## y4_SCRT1     0.04687624
    ## y6_SCRT1     0.07041858
    ## y8_SCRT1     0.04583772
    ## y5_SCHCP     0.11097552
    ## y5_BEHCD     0.08862142
    ## y5_SELWT    -0.09621464

``` r
plotVarSelection(elnetFitPsych6, psychTrain6X)
```

![](nghs1_var_selection_files/figure-markdown_github/unnamed-chunk-24-1.png)

Selection 9: Stress Predicting Assorted Psychological Variables
---------------------------------------------------------------

``` r
# Select data
psychTrain8 <-
  lassoTrainingData %>% 
  dplyr::select(
    y2_STRESS,
    y4_CR1,
    y6_CR1,
    y8_CR1,
    y2_EE1,
    y4_EE1,
    y6_EE1,
    y8_EE1,
    y4_SCRT1,
    y6_SCRT1,
    y8_SCRT1,
    y5_SCHCP,  
    y5_BEHCD, 
    y5_SELWT
  )

psychTrain8X <- 
  psychTrain8 %>% 
  dplyr::select(-y2_STRESS)

psychTrain8Y <-
  psychTrain8 %>% 
  dplyr::select(y2_STRESS)
```

``` r
lassoFitPsych8 <- 
  train(
    psychTrain8X, 
    psychTrain8Y$y2_STRESS, 
    method = "glmnet", 
    trControl = fitControl2, 
    preProc = c('scale','center'), 
    tuneGrid = trainGrid
  )

elnetFitPsych8 <-
  train(
    psychTrain8X, 
    psychTrain8Y$y2_STRESS, 
    method = "glmnet", 
    trControl = fitControl2, 
    preProc = c('scale','center') 
  )

models <- list(lasso = lassoFitPsych8, elastic = elnetFitPsych8)
resamples(models) %>% summary()
```

    ## 
    ## Call:
    ## summary.resamples(object = .)
    ## 
    ## Models: lasso, elastic 
    ## Number of resamples: 50 
    ## 
    ## MAE 
    ##             Min.  1st Qu.   Median     Mean  3rd Qu.     Max. NA's
    ## lasso   4.705753 4.991141 5.135399 5.130059 5.203247 5.594837    0
    ## elastic 4.742958 4.972356 5.119754 5.122636 5.258055 5.594308    0
    ## 
    ## RMSE 
    ##             Min.  1st Qu.   Median     Mean  3rd Qu.     Max. NA's
    ## lasso   6.025382 6.421821 6.604763 6.614418 6.747447 7.277804    0
    ## elastic 6.157428 6.356527 6.658097 6.611029 6.812330 7.195236    0
    ## 
    ## Rsquared 
    ##               Min.    1st Qu.     Median       Mean   3rd Qu.      Max.
    ## lasso   0.02830175 0.06514789 0.08414532 0.09345895 0.1212207 0.1859959
    ## elastic 0.01652533 0.07091611 0.09242953 0.09350686 0.1139926 0.1896357
    ##         NA's
    ## lasso      0
    ## elastic    0

``` r
getBestResult(lassoFitPsych8)
```

    ##   alpha    lambda     RMSE   Rsquared      MAE    RMSESD RsquaredSD
    ## 1     1 0.1629751 6.614418 0.09345895 5.130059 0.2805709 0.03666102
    ##       MAESD
    ## 1 0.2121869

``` r
getBestResult(elnetFitPsych8)
```

    ##   alpha    lambda     RMSE   Rsquared      MAE    RMSESD RsquaredSD
    ## 1  0.55 0.3447671 6.611029 0.09350686 5.122636 0.2879373 0.03532392
    ##       MAESD
    ## 1 0.2048175

``` r
coef(elnetFitPsych8$finalModel, elnetFitPsych8$bestTune$lambda)
```

    ## 14 x 1 sparse Matrix of class "dgCMatrix"
    ##                      1
    ## (Intercept) 24.6936318
    ## y4_CR1      -0.3776817
    ## y6_CR1      -0.2054948
    ## y8_CR1      -0.5036317
    ## y2_EE1      -0.3742929
    ## y4_EE1       .        
    ## y6_EE1       .        
    ## y8_EE1       .        
    ## y4_SCRT1     0.6256443
    ## y6_SCRT1     0.1382091
    ## y8_SCRT1     .        
    ## y5_SCHCP    -0.7246016
    ## y5_BEHCD     .        
    ## y5_SELWT    -0.7367225

``` r
plotVarSelection(elnetFitPsych8, psychTrain8X)
```

![](nghs1_var_selection_files/figure-markdown_github/unnamed-chunk-27-1.png)

Selection 10: Anxiety Predicting Assorted Psychological Variables
-----------------------------------------------------------------

``` r
# Select data
psychTrain7 <-
  lassoTrainingData %>% 
  dplyr::select(
    y3_ANXR,
    y4_CR1,
    y6_CR1,
    y8_CR1,
    y4_EE1,
    y6_EE1,
    y8_EE1,
    y4_SCRT1,
    y6_SCRT1,
    y8_SCRT1,
    y5_SCHCP, 
    y5_BEHCD, 
    y5_SELWT
  )

psychTrain7X <- 
  psychTrain7 %>% 
  dplyr::select(-y3_ANXR)

psychTrain7Y <-
  psychTrain7 %>% 
  dplyr::select(y3_ANXR)
```

``` r
lassoFitPsych7 <- 
  train(
    psychTrain7X, 
    psychTrain7Y$y3_ANXR, 
    method = "glmnet", 
    trControl = fitControl2, 
    preProc = c('scale','center'), 
    tuneGrid = trainGrid
  )

elnetFitPsych7 <-
  train(
    psychTrain7X, 
    psychTrain7Y$y3_ANXR, 
    method = "glmnet", 
    trControl = fitControl2, 
    preProc = c('scale','center') 
  )

models <- list(lasso = lassoFitPsych7, elastic = elnetFitPsych7)
resamples(models) %>% summary()
```

    ## 
    ## Call:
    ## summary.resamples(object = .)
    ## 
    ## Models: lasso, elastic 
    ## Number of resamples: 50 
    ## 
    ## MAE 
    ##             Min.  1st Qu.   Median     Mean  3rd Qu.     Max. NA's
    ## lasso   4.372646 4.635880 4.754379 4.768434 4.893795 5.246585    0
    ## elastic 4.298001 4.626655 4.723661 4.753227 4.871104 5.213397    0
    ## 
    ## RMSE 
    ##             Min.  1st Qu.   Median     Mean  3rd Qu.     Max. NA's
    ## lasso   5.282772 5.677716 5.802047 5.814385 5.985455 6.382794    0
    ## elastic 5.108452 5.618192 5.773832 5.801408 6.004642 6.322189    0
    ## 
    ## Rsquared 
    ##               Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
    ## lasso   0.08295115 0.1702676 0.2035726 0.2059147 0.2447686 0.3511526    0
    ## elastic 0.12019697 0.1619785 0.2036845 0.2086569 0.2503995 0.3352757    0

``` r
getBestResult(lassoFitPsych7)
```

    ##   alpha    lambda     RMSE  Rsquared      MAE    RMSESD RsquaredSD
    ## 1     1 0.1232847 5.814385 0.2059147 4.768434 0.2521153 0.05401864
    ##       MAESD
    ## 1 0.2132256

``` r
getBestResult(elnetFitPsych7)
```

    ##   alpha    lambda     RMSE  Rsquared      MAE    RMSESD RsquaredSD
    ## 1   0.1 0.4736622 5.801408 0.2086569 4.753227 0.2665373 0.05612459
    ##       MAESD
    ## 1 0.1977271

``` r
coef(elnetFitPsych7$finalModel, elnetFitPsych7$bestTune$lambda)
```

    ## 13 x 1 sparse Matrix of class "dgCMatrix"
    ##                       1
    ## (Intercept) 10.77279950
    ## y4_CR1      -0.34121069
    ## y6_CR1      -0.31244673
    ## y8_CR1      -0.20171449
    ## y4_EE1       0.06415318
    ## y6_EE1       .         
    ## y8_EE1       0.31175295
    ## y4_SCRT1     0.85779030
    ## y6_SCRT1     0.67295320
    ## y8_SCRT1     0.25713928
    ## y5_SCHCP    -1.15504733
    ## y5_BEHCD    -0.39184643
    ## y5_SELWT    -0.81576557

``` r
plotVarSelection(elnetFitPsych7, psychTrain7X)
```

![](nghs1_var_selection_files/figure-markdown_github/unnamed-chunk-30-1.png)

The variable most consistently selected across 7-10 was year 5 self-worth. Importantly, this was the only variable that was selected to predict year 10 BMI.

Selection 11: Health Behavior Predicting BMI
--------------------------------------------

``` r
# Select data
behavTrain1 <-
  lassoTrainingData %>% 
  dplyr::select(
    y10_QI,
    y5_FSTFOOD4,
    y7_FSTFOOD4,
    y9_FSTFOOD4,
    y5_FSTFOOD5,
    y7_FSTFOOD5,
    y9_FSTFOOD5,
    y4_ALONE,
    y5_ALONE,
    y4_SECRET,
    y5_SECRET,
    y6_SECRET,
    y7_SECRET,
    y9_SECRET,
    y4_EATTV,
    y5_EATTV,
    y6_EATTV,
    y7_EATTV,
    y9_EATTV,
    y0_eat_VITAMINS:y9_eat_BOREDLS,
    -contains("_eat_SECRET"),
    -contains("_eat_ALONE"),
    -contains("DIETLOS"),
    -contains("LOSWT"),
    -contains("WGHLS"),
    -contains("WFAMLY"),
    -contains("EATLESS"),
    -contains("WGHMOR"),
    -contains("BOREDLS"),
    -contains("WGHLES"),
    -contains("BOREDLS"),
    -contains("SADLS"),
    -contains("ASTOLD"),
    -contains("BUYFMLY"),
    -contains("NAG"),
    -contains("PICKEAT"),
    -contains("HAPPYLS"),
    -contains("WORRYLS"),
    -contains("CERTFOOD"),
    -contains("GAINWT"),
    -contains("ALLWNT"),
    -contains("SCHLNCH"),
    -contains("STOPEAT"),
    -contains("BUYFMLY"),
    -contains("SCHLNCH")
    -contains("CERTFOOD"),
    -contains("FRIENDS"),
    -contains("FNSHPLT"),
    -contains("SKIPLNCH"),
    -contains("y9"),
    -contains("y0"),
    -contains("BREAKFST"),
    -contains("PHYSACT"),
    -contains("HOMEWRK"),
    -contains("SPORTEAT"),
    -contains("CRRYLNCH"),
    -contains("FIXOWN"),
    -contains("PARSNACK"),
    -contains("BOREDMR"),
    -contains("EATTV"),
    -contains("VRHUNGY"),
    -contains("DRINK")
  ) %>% 
  dplyr::select(
    -ends_with("MR"),
    -contains("eat_MAD"),
    -contains("REWARD")
  )

behavTrain1x <- 
  behavTrain1 %>% 
  dplyr::select(-y10_QI, -contains("y0"), -contains("y2"), -contains("y3"))

xfactors <-
  lassoTrainingData %>% 
  dplyr::select(
    y5_AFTSNK,
    y7_AFTSNK,
    y9_AFTSNK,
    y4_BREAKFST,
    y5_BREAKFST,
    y6_BREAKFST,
    y7_BREAKFST,
    y8_BREAKFST,
    y9_BREAKFST,
    y5_eat_STOPEAT
  ) %>% 
  mutate_all(as.factor)

behavTrain1x <-
  bind_cols(
    behavTrain1x,
    xfactors
  )

behavTrain1y <-
  behavTrain1 %>% 
  dplyr::select(y10_QI)

behavTrain1 <-
   bind_cols(
    behavTrain1x,
    behavTrain1y
  )

mm <- model.matrix(y10_QI ~ . - 1, behavTrain1)
```

``` r
lassoFitBehav1 <- 
  train(
    mm, 
    behavTrain1y$y10_QI, 
    method = "glmnet", 
    trControl = fitControl2, 
    preProc = c('scale','center'), 
    tuneGrid = trainGrid
  )

elnetFitBehav1 <-
  train(
    mm, 
    behavTrain1y$y10_QI, 
    method = "glmnet", 
    trControl = fitControl2, 
    preProc = c('scale','center') 
  )

models <- list(lasso = lassoFitBehav1, elastic = elnetFitBehav1)
resamples(models) %>% summary()
```

    ## 
    ## Call:
    ## summary.resamples(object = .)
    ## 
    ## Models: lasso, elastic 
    ## Number of resamples: 50 
    ## 
    ## MAE 
    ##             Min.  1st Qu.   Median     Mean  3rd Qu.     Max. NA's
    ## lasso   4.460708 4.699186 4.788055 4.834906 5.018309 5.268306    0
    ## elastic 4.428763 4.677443 4.831872 4.843438 5.002387 5.342029    0
    ## 
    ## RMSE 
    ##             Min.  1st Qu.   Median     Mean  3rd Qu.     Max. NA's
    ## lasso   5.646844 6.102147 6.306783 6.317096 6.566413 7.063953    0
    ## elastic 5.568925 6.089517 6.284302 6.326172 6.557488 7.065330    0
    ## 
    ## Rsquared 
    ##               Min.    1st Qu.    Median      Mean   3rd Qu.      Max. NA's
    ## lasso   0.05263043 0.09807173 0.1387288 0.1332128 0.1632565 0.2269137    0
    ## elastic 0.04949539 0.10000411 0.1394338 0.1320972 0.1558701 0.2366547    0

``` r
getBestResult(lassoFitBehav1)
```

    ##   alpha    lambda     RMSE  Rsquared      MAE    RMSESD RsquaredSD
    ## 1     1 0.1629751 6.317096 0.1332128 4.834906 0.3471571 0.04152726
    ##       MAESD
    ## 1 0.2127982

``` r
getBestResult(elnetFitBehav1)
```

    ##   alpha   lambda     RMSE  Rsquared      MAE    RMSESD RsquaredSD
    ## 1  0.55 0.283932 6.326172 0.1320972 4.843438 0.3584596 0.03965619
    ##       MAESD
    ## 1 0.2281498

``` r
round(coef(elnetFitBehav1$finalModel, elnetFitBehav1$bestTune$lambda), 2)
```

    ## 47 x 1 sparse Matrix of class "dgCMatrix"
    ##                     1
    ## (Intercept)     25.69
    ## y5_FSTFOOD4      .   
    ## y7_FSTFOOD4      .   
    ## y5_FSTFOOD5      0.30
    ## y7_FSTFOOD5      .   
    ## y4_ALONE         .   
    ## y5_ALONE         0.44
    ## y4_SECRET       -0.16
    ## y5_SECRET        .   
    ## y6_SECRET        0.28
    ## y7_SECRET        0.49
    ## y4_eat_VITAMINS -0.52
    ## y4_eat_VEGGIE    .   
    ## y4_eat_NOTHUNGY  .   
    ## y4_eat_SNKFOOD1  0.02
    ## y4_eat_BHELPS    .   
    ## y4_eat_BEDRM    -0.21
    ## y4_eat_SNKFOOD2 -0.01
    ## y4_eat_DESSERT   .   
    ## y5_eat_VITAMINS  .   
    ## y5_eat_VEGGIE    .   
    ## y5_eat_NOTHUNGY  .   
    ## y5_eat_SNKFOOD1  .   
    ## y5_eat_BHELPS   -0.06
    ## y5_eat_BEDRM     .   
    ## y5_eat_SNKFOOD2 -0.29
    ## y5_eat_DESSERT   0.00
    ## y6_eat_NOTHUNGY  0.09
    ## y6_eat_BHELPS   -0.28
    ## y6_eat_BEDRM    -0.06
    ## y6_eat_SNKFOOD2 -0.74
    ## y6_eat_DESSERT  -0.45
    ## y7_eat_NOTHUNGY  .   
    ## y7_eat_BHELPS    .   
    ## y7_eat_BEDRM    -0.30
    ## y7_eat_DESSERT   0.04
    ## y5_AFTSNK1      -0.22
    ## y5_AFTSNK2       0.19
    ## y7_AFTSNK2       0.30
    ## y9_AFTSNK2       0.37
    ## y4_BREAKFST2     0.59
    ## y5_BREAKFST2     .   
    ## y6_BREAKFST2     0.17
    ## y7_BREAKFST2     .   
    ## y8_BREAKFST2     0.41
    ## y9_BREAKFST2     0.43
    ## y5_eat_STOPEAT2 -0.81

``` r
tmp_coeffs <- coef(elnetFitBehav1$finalModel, elnetFitBehav1$bestTune$lambda)

coefs <- 
  data.frame(name = tmp_coeffs@Dimnames[[1]][tmp_coeffs@i + 1], coefficient = tmp_coeffs@x)

coefs %>%
  filter(name != "(Intercept)") %>% 
  ggplot(aes(fct_reorder(name, coefficient), coefficient)) +
  geom_point() +
  coord_flip()
```

![](nghs1_var_selection_files/figure-markdown_github/unnamed-chunk-33-1.png)

Selection 12: Income Predicting Health Behaviors
------------------------------------------------

``` r
# Select data
behavTrain2 <-
  lassoTrainingData %>% 
  dplyr::select(
    y0_CATINC,
    y5_FSTFOOD4,
    y7_FSTFOOD4,
    y9_FSTFOOD4,
    y5_FSTFOOD5,
    y7_FSTFOOD5,
    y9_FSTFOOD5,
    y4_ALONE,
    y5_ALONE,
    y4_SECRET,
    y5_SECRET,
    y6_SECRET,
    y7_SECRET,
    y9_SECRET,
    y4_EATTV,
    y5_EATTV,
    y6_EATTV,
    y7_EATTV,
    y9_EATTV,
    y0_eat_VITAMINS:y9_eat_BOREDLS,
    -contains("_eat_SECRET"),
    -contains("_eat_ALONE"),    
    -contains("DIETLOS"),
    -contains("LOSWT"),
    -contains("WGHLS"),
    -contains("WFAMLY"),
    -contains("EATLESS"),
    -contains("WGHMOR"),
    -contains("BOREDLS"),
    -contains("WGHLES"),
    -contains("BOREDLS"),
    -contains("SADLS"),
    -contains("ASTOLD"),
    -contains("BUYFMLY"),
    -contains("NAG"),
    -contains("PICKEAT"),
    -contains("HAPPYLS"),
    -contains("WORRYLS"),
    -contains("CERTFOOD"),
    -contains("GAINWT"),
    -contains("ALLWNT"),
    -contains("SCHLNCH"),
    -contains("STOPEAT"),
    -contains("BUYFMLY"),
    -contains("SCHLNCH")
    -contains("CERTFOOD"),
    -contains("FRIENDS"),
    -contains("FNSHPLT"),
    -contains("SKIPLNCH"),
    -contains("y9"),
    -contains("y0"),
    -contains("BREAKFST"),
    -contains("PHYSACT"),
    -contains("HOMEWRK"),
    -contains("SPORTEAT"),
    -contains("CRRYLNCH"),
    -contains("FIXOWN"),
    -contains("PARSNACK"),
    -contains("BOREDMR"),
    -contains("EATTV"),
    -contains("VRHUNGY"),
    -contains("DRINK"),
    -contains("DRINK")
  ) %>% 
  dplyr::select(
    -ends_with("MR"),
    -contains("eat_MAD"),
    -contains("REWARD")
  )


behavTrain2x <- 
  behavTrain2 %>% 
  dplyr::select(-contains("y0"), -contains("y2"), -contains("y3")) 

behavTrain2y <-
  lassoTrainingData %>% 
  dplyr::select(y0_CATINC)

xfactors <-
  lassoTrainingData %>% 
  dplyr::select(
    y5_AFTSNK,
    y7_AFTSNK,
    y9_AFTSNK,
    y4_BREAKFST,
    y5_BREAKFST,
    y6_BREAKFST,
    y7_BREAKFST,
    y8_BREAKFST,
    y9_BREAKFST,
    y5_eat_STOPEAT
  ) %>% 
  mutate_all(as.factor)

behavTrain2x <-
  bind_cols(
    behavTrain2x,
    xfactors
  )

behavTrain2 <-
   bind_cols(
    behavTrain2x,
    behavTrain2y
  )

mm <- model.matrix(y0_CATINC ~ . - 1, behavTrain2)
```

``` r
lassoFitBehav2 <- 
  train(
    mm, 
    behavTrain2y$y0_CATINC, 
    method = "glmnet", 
    trControl = fitControl2, 
    preProc = c('scale','center'), 
    tuneGrid = trainGrid
  )

elnetFitBehav2 <-
  train(
    mm, 
    behavTrain2y$y0_CATINC, 
    method = "glmnet", 
    trControl = fitControl2, 
    preProc = c('scale','center') 
  )

models <- list(lasso = lassoFitBehav2, elastic = elnetFitBehav2)
resamples(models) %>% summary()
```

    ## 
    ## Call:
    ## summary.resamples(object = .)
    ## 
    ## Models: lasso, elastic 
    ## Number of resamples: 50 
    ## 
    ## MAE 
    ##              Min.  1st Qu.    Median      Mean   3rd Qu.      Max. NA's
    ## lasso   0.8295043 0.868500 0.8824173 0.8892048 0.9086306 0.9659223    0
    ## elastic 0.8307596 0.860334 0.8865218 0.8851286 0.9001111 0.9472100    0
    ## 
    ## RMSE 
    ##              Min.  1st Qu.   Median     Mean  3rd Qu.     Max. NA's
    ## lasso   0.9924132 1.033213 1.052758 1.058652 1.085229 1.134027    0
    ## elastic 0.9824273 1.033600 1.055939 1.054775 1.073702 1.136146    0
    ## 
    ## Rsquared 
    ##               Min.    1st Qu.    Median      Mean   3rd Qu.      Max. NA's
    ## lasso   0.03878980 0.09477352 0.1161287 0.1183819 0.1417772 0.2210344    0
    ## elastic 0.06253367 0.09484683 0.1194679 0.1238879 0.1522071 0.2271904    0

``` r
getBestResult(lassoFitBehav2)
```

    ##   alpha     lambda     RMSE  Rsquared       MAE     RMSESD RsquaredSD
    ## 1     1 0.03053856 1.058652 0.1183819 0.8892048 0.03137525 0.03940677
    ##        MAESD
    ## 1 0.02812582

``` r
getBestResult(elnetFitBehav2)
```

    ##   alpha     lambda     RMSE  Rsquared       MAE     RMSESD RsquaredSD
    ## 1  0.55 0.05336029 1.054775 0.1238879 0.8851286 0.03200156 0.03829272
    ##       MAESD
    ## 1 0.0285085

``` r
coef(lassoFitBehav2$finalModel, lassoFitBehav2$bestTune$lambda)
```

    ## 47 x 1 sparse Matrix of class "dgCMatrix"
    ##                             1
    ## (Intercept)      2.8549810845
    ## y5_FSTFOOD4      .           
    ## y7_FSTFOOD4      .           
    ## y5_FSTFOOD5     -0.0394397585
    ## y7_FSTFOOD5      .           
    ## y4_ALONE         .           
    ## y5_ALONE         0.0188791302
    ## y4_SECRET        .           
    ## y5_SECRET        0.0725236344
    ## y6_SECRET        .           
    ## y7_SECRET       -0.0532289620
    ## y4_eat_VITAMINS  .           
    ## y4_eat_VEGGIE    0.0232340736
    ## y4_eat_NOTHUNGY  .           
    ## y4_eat_SNKFOOD1 -0.1454059887
    ## y4_eat_BHELPS    .           
    ## y4_eat_BEDRM     0.0166700698
    ## y4_eat_SNKFOOD2  .           
    ## y4_eat_DESSERT  -0.0061402414
    ## y5_eat_VITAMINS  .           
    ## y5_eat_VEGGIE    0.1668166113
    ## y5_eat_NOTHUNGY  .           
    ## y5_eat_SNKFOOD1 -0.0935974441
    ## y5_eat_BHELPS    .           
    ## y5_eat_BEDRM     .           
    ## y5_eat_SNKFOOD2 -0.1124737768
    ## y5_eat_DESSERT  -0.0384264530
    ## y6_eat_NOTHUNGY -0.0089446901
    ## y6_eat_BHELPS    .           
    ## y6_eat_BEDRM     .           
    ## y6_eat_SNKFOOD2  0.0257296883
    ## y6_eat_DESSERT  -0.0302771149
    ## y7_eat_NOTHUNGY  .           
    ## y7_eat_BHELPS    .           
    ## y7_eat_BEDRM     .           
    ## y7_eat_DESSERT   .           
    ## y5_AFTSNK1       0.0045228987
    ## y5_AFTSNK2       .           
    ## y7_AFTSNK2      -0.0437972231
    ## y9_AFTSNK2      -0.0000677621
    ## y4_BREAKFST2     .           
    ## y5_BREAKFST2    -0.0220853951
    ## y6_BREAKFST2    -0.0228675464
    ## y7_BREAKFST2     0.0406111112
    ## y8_BREAKFST2    -0.0510840921
    ## y9_BREAKFST2    -0.0507636891
    ## y5_eat_STOPEAT2  0.0579993135

``` r
tmp_coeffs <- coef(lassoFitBehav2$finalModel, lassoFitBehav2$bestTune$lambda)

coefs <- 
  data.frame(name = tmp_coeffs@Dimnames[[1]][tmp_coeffs@i + 1], coefficient = tmp_coeffs@x)

coefs %>%
  filter(name != "(Intercept)") %>% 
  ggplot(aes(fct_reorder(name, coefficient), coefficient)) +
  geom_point() +
  coord_flip()
```

![](nghs1_var_selection_files/figure-markdown_github/unnamed-chunk-36-1.png)

Selection 13: Anxiety Predicting Health Behaviors
-------------------------------------------------

``` r
# Select data
behavTrain4 <-
  lassoTrainingData %>% 
  dplyr::select(
    y3_ANXR,
    y5_FSTFOOD4,
    y7_FSTFOOD4,
    y9_FSTFOOD4,
    y5_FSTFOOD5,
    y7_FSTFOOD5,
    y9_FSTFOOD5,
    y4_ALONE,
    y5_ALONE,
    y4_SECRET,
    y5_SECRET,
    y6_SECRET,
    y7_SECRET,
    y9_SECRET,
    y4_EATTV,
    y5_EATTV,
    y6_EATTV,
    y7_EATTV,
    y9_EATTV,
    y0_eat_VITAMINS:y9_eat_BOREDLS,
    -contains("_eat_SECRET"),
    -contains("_eat_ALONE"),    
    -contains("DIETLOS"),
    -contains("LOSWT"),
    -contains("WGHLS"),
    -contains("WFAMLY"),
    -contains("EATLESS"),
    -contains("WGHMOR"),
    -contains("BOREDLS"),
    -contains("WGHLES"),
    -contains("BOREDLS"),
    -contains("SADLS"),
    -contains("ASTOLD"),
    -contains("BUYFMLY"),
    -contains("NAG"),
    -contains("PICKEAT"),
    -contains("HAPPYLS"),
    -contains("WORRYLS"),
    -contains("CERTFOOD"),
    -contains("GAINWT"),
    -contains("ALLWNT"),
    -contains("SCHLNCH"),
    -contains("STOPEAT"),
    -contains("BUYFMLY"),
    -contains("SCHLNCH")
    -contains("CERTFOOD"),
    -contains("FRIENDS"),
    -contains("FNSHPLT"),
    -contains("SKIPLNCH"),
    -contains("y9"),
    -contains("y0"),
    -contains("BREAKFST"),
    -contains("PHYSACT"),
    -contains("HOMEWRK"),
    -contains("SPORTEAT"),
    -contains("CRRYLNCH"),
    -contains("FIXOWN"),
    -contains("PARSNACK"),
    -contains("BOREDMR"),
    -contains("EATTV"),
    -contains("VRHUNGY"),
    -contains("DRINK")
  ) %>% 
  dplyr::select(
    -ends_with("MR"),
    -contains("eat_MAD"),
    -contains("REWARD")
  )

behavTrain4x <- 
  behavTrain4 %>% 
  dplyr::select(-y3_ANXR, -contains("y0"), -contains("y2"), -contains("y3"))

behavTrain4y <-
  behavTrain4 %>% 
  dplyr::select(y3_ANXR)

xfactors <-
  lassoTrainingData %>% 
  dplyr::select(
    y5_AFTSNK,
    y7_AFTSNK,
    y9_AFTSNK,
    y4_BREAKFST,
    y5_BREAKFST,
    y6_BREAKFST,
    y7_BREAKFST,
    y8_BREAKFST,
    y9_BREAKFST,
    y5_eat_STOPEAT
  ) %>% 
  mutate_all(as.factor)

behavTrain4x <-
  bind_cols(
    behavTrain4x,
    xfactors
  )

behavTrain4 <-
   bind_cols(
    behavTrain4x,
    behavTrain4y
  )

mm <- model.matrix(y3_ANXR ~ . - 1, behavTrain4)
```

``` r
lassoFitBehav4 <- 
  train(
    mm, 
    behavTrain4y$y3_ANXR, 
    method = "glmnet", 
    trControl = fitControl2, 
    preProc = c('scale','center'), 
    tuneGrid = trainGrid
  )

elnetFitBehav4 <-
  train(
    mm, 
    behavTrain4y$y3_ANXR, 
    method = "glmnet", 
    trControl = fitControl2, 
    preProc = c('scale','center') 
  )

models <- list(lasso = lassoFitBehav4, elastic = elnetFitBehav4)
resamples(models) %>% summary()
```

    ## 
    ## Call:
    ## summary.resamples(object = .)
    ## 
    ## Models: lasso, elastic 
    ## Number of resamples: 50 
    ## 
    ## MAE 
    ##             Min.  1st Qu.   Median     Mean  3rd Qu.     Max. NA's
    ## lasso   4.864622 5.042585 5.127093 5.149267 5.277248 5.431552    0
    ## elastic 4.807769 5.050496 5.167887 5.149017 5.243950 5.517975    0
    ## 
    ## RMSE 
    ##             Min.  1st Qu.   Median     Mean  3rd Qu.     Max. NA's
    ## lasso   5.812940 6.031787 6.116282 6.158487 6.291338 6.453240    0
    ## elastic 5.773148 6.041676 6.189464 6.166666 6.274757 6.547349    0
    ## 
    ## Rsquared 
    ##               Min.    1st Qu.     Median      Mean   3rd Qu.      Max.
    ## lasso   0.03777354 0.08555105 0.10671254 0.1060359 0.1319368 0.1628199
    ## elastic 0.03137320 0.08266804 0.09945016 0.1055022 0.1203610 0.2144843
    ##         NA's
    ## lasso      0
    ## elastic    0

``` r
getBestResult(lassoFitBehav4)
```

    ##   alpha    lambda     RMSE  Rsquared      MAE    RMSESD RsquaredSD
    ## 1     1 0.2154435 6.158487 0.1060359 5.149267 0.1713919 0.03224763
    ##       MAESD
    ## 1 0.1546698

``` r
getBestResult(elnetFitBehav4)
```

    ##   alpha    lambda     RMSE  Rsquared      MAE    RMSESD RsquaredSD
    ## 1  0.55 0.2820978 6.166666 0.1055022 5.149017 0.1805986 0.04142358
    ##       MAESD
    ## 1 0.1566996

``` r
coef(elnetFitBehav4$finalModel, elnetFitBehav4$bestTune$lambda)
```

    ## 47 x 1 sparse Matrix of class "dgCMatrix"
    ##                           1
    ## (Intercept)     10.77279950
    ## y5_FSTFOOD4      0.17903510
    ## y7_FSTFOOD4     -0.05061696
    ## y5_FSTFOOD5      0.22870955
    ## y7_FSTFOOD5      .         
    ## y4_ALONE         0.14097623
    ## y5_ALONE         0.20963066
    ## y4_SECRET        .         
    ## y5_SECRET        0.34490033
    ## y6_SECRET        0.48926295
    ## y7_SECRET        0.31493037
    ## y4_eat_VITAMINS -0.30894145
    ## y4_eat_VEGGIE   -0.24288874
    ## y4_eat_NOTHUNGY  0.04195840
    ## y4_eat_SNKFOOD1  .         
    ## y4_eat_BHELPS    0.08686401
    ## y4_eat_BEDRM     0.06391114
    ## y4_eat_SNKFOOD2 -0.03142113
    ## y4_eat_DESSERT   .         
    ## y5_eat_VITAMINS  0.29712497
    ## y5_eat_VEGGIE   -0.44239318
    ## y5_eat_NOTHUNGY  0.19365416
    ## y5_eat_SNKFOOD1  .         
    ## y5_eat_BHELPS    .         
    ## y5_eat_BEDRM     .         
    ## y5_eat_SNKFOOD2 -0.07285773
    ## y5_eat_DESSERT  -0.05016444
    ## y6_eat_NOTHUNGY  0.52155970
    ## y6_eat_BHELPS    0.04429088
    ## y6_eat_BEDRM    -0.03059218
    ## y6_eat_SNKFOOD2 -0.47649906
    ## y6_eat_DESSERT   .         
    ## y7_eat_NOTHUNGY  0.18265224
    ## y7_eat_BHELPS    0.08048814
    ## y7_eat_BEDRM     .         
    ## y7_eat_DESSERT   .         
    ## y5_AFTSNK1      -0.09621788
    ## y5_AFTSNK2       0.06814047
    ## y7_AFTSNK2       .         
    ## y9_AFTSNK2       0.09158953
    ## y4_BREAKFST2     .         
    ## y5_BREAKFST2     .         
    ## y6_BREAKFST2     .         
    ## y7_BREAKFST2     .         
    ## y8_BREAKFST2     .         
    ## y9_BREAKFST2     0.30184552
    ## y5_eat_STOPEAT2 -0.88691547

``` r
tmp_coeffs <- coef(elnetFitBehav4$finalModel, elnetFitBehav4$bestTune$lambda)

coefs <- 
  data.frame(name = tmp_coeffs@Dimnames[[1]][tmp_coeffs@i + 1], coefficient = tmp_coeffs@x)

coefs %>%
  filter(name != "(Intercept)") %>% 
  ggplot(aes(fct_reorder(name, coefficient), coefficient)) +
  geom_point() +
  coord_flip()
```

![](nghs1_var_selection_files/figure-markdown_github/unnamed-chunk-39-1.png)

Selection 14: Stress Predicting Health Behaviors
------------------------------------------------

``` r
# Select data
behavTrain5 <-
  lassoTrainingData %>% 
  dplyr::select(
    y2_STRESS,
    y5_FSTFOOD4,
    y7_FSTFOOD4,
    y9_FSTFOOD4,
    y5_FSTFOOD5,
    y7_FSTFOOD5,
    y9_FSTFOOD5,
    y4_ALONE,
    y5_ALONE,
    y4_SECRET,
    y5_SECRET,
    y6_SECRET,
    y7_SECRET,
    y9_SECRET,
    y4_EATTV,
    y5_EATTV,
    y6_EATTV,
    y7_EATTV,
    y9_EATTV,
    y0_eat_VITAMINS:y9_eat_BOREDLS,
    -contains("_eat_SECRET"),
    -contains("_eat_ALONE"),    
    -contains("DIETLOS"),
    -contains("LOSWT"),
    -contains("WGHLS"),
    -contains("WFAMLY"),
    -contains("EATLESS"),
    -contains("WGHMOR"),
    -contains("BOREDLS"),
    -contains("WGHLES"),
    -contains("BOREDLS"),
    -contains("SADLS"),
    -contains("ASTOLD"),
    -contains("BUYFMLY"),
    -contains("NAG"),
    -contains("PICKEAT"),
    -contains("HAPPYLS"),
    -contains("WORRYLS"),
    -contains("CERTFOOD"),
    -contains("GAINWT"),
    -contains("ALLWNT"),
    -contains("SCHLNCH"),
    -contains("STOPEAT"),
    -contains("BUYFMLY"),
    -contains("SCHLNCH")
    -contains("CERTFOOD"),
    -contains("FRIENDS"),
    -contains("FNSHPLT"),
    -contains("SKIPLNCH"),
    -contains("y9"),
    -contains("y0"),
    -contains("BREAKFST"),
    -contains("PHYSACT"),
    -contains("HOMEWRK"),
    -contains("SPORTEAT"),
    -contains("CRRYLNCH"),
    -contains("FIXOWN"),
    -contains("PARSNACK"),
    -contains("BOREDMR"),
    -contains("EATTV"),
    -contains("VRHUNGY"),
    -contains("DRINK")
  ) %>% 
  dplyr::select(
    -ends_with("MR"),
    -contains("eat_MAD"),
    -contains("REWARD")
  )

behavTrain5x <- 
  behavTrain5 %>% 
  dplyr::select(-y2_STRESS, -contains("y0"), -contains("y2"), -contains("y3")) 

behavTrain5y <-
  behavTrain5 %>% 
  dplyr::select(y2_STRESS)

xfactors <-
  lassoTrainingData %>% 
  dplyr::select(
    y5_AFTSNK,
    y7_AFTSNK,
    y9_AFTSNK,
    y4_BREAKFST,
    y5_BREAKFST,
    y6_BREAKFST,
    y7_BREAKFST,
    y8_BREAKFST,
    y9_BREAKFST,
    y5_eat_STOPEAT
  ) %>% 
  mutate_all(as.factor)

behavTrain5x <-
  bind_cols(
    behavTrain5x,
    xfactors
  )

behavTrain5 <-
   bind_cols(
    behavTrain5x,
    behavTrain5y
  )

mm <- model.matrix(y2_STRESS ~ . - 1, behavTrain5)
```

``` r
lassoFitBehav5 <- 
  train(
    mm, 
    behavTrain5y$y2_STRESS, 
    method = "glmnet", 
    trControl = fitControl2, 
    preProc = c('scale','center'), 
    tuneGrid = trainGrid
  )

elnetFitBehav5 <-
  train(
    mm, 
    behavTrain5y$y2_STRESS, 
    method = "glmnet", 
    trControl = fitControl2, 
    preProc = c('scale','center') 
  )

models <- list(lasso = lassoFitBehav5, elastic = elnetFitBehav5)
resamples(models) %>% summary()
```

    ## 
    ## Call:
    ## summary.resamples(object = .)
    ## 
    ## Models: lasso, elastic 
    ## Number of resamples: 50 
    ## 
    ## MAE 
    ##             Min.  1st Qu.   Median     Mean  3rd Qu.     Max. NA's
    ## lasso   4.747332 5.137325 5.345196 5.324517 5.461666 5.776778    0
    ## elastic 4.927860 5.216081 5.291198 5.329169 5.444307 5.730644    0
    ## 
    ## RMSE 
    ##             Min.  1st Qu.   Median     Mean  3rd Qu.     Max. NA's
    ## lasso   6.024870 6.598941 6.821584 6.829920 7.098564 7.508860    0
    ## elastic 6.233382 6.599144 6.822037 6.843093 7.058633 7.504382    0
    ## 
    ## Rsquared 
    ##                 Min.    1st Qu.     Median       Mean    3rd Qu.      Max.
    ## lasso   0.0005938722 0.01897897 0.02929642 0.03332571 0.04572922 0.0875957
    ## elastic 0.0028914633 0.01704876 0.02657078 0.03025047 0.03820239 0.1177595
    ##         NA's
    ## lasso      0
    ## elastic    0

``` r
getBestResult(lassoFitBehav5)
```

    ##   alpha    lambda    RMSE   Rsquared      MAE    RMSESD RsquaredSD
    ## 1     1 0.4977024 6.82992 0.03332571 5.324517 0.3513226 0.01963413
    ##       MAESD
    ## 1 0.2391003

``` r
getBestResult(elnetFitBehav5)
```

    ##   alpha    lambda     RMSE   Rsquared      MAE    RMSESD RsquaredSD
    ## 1     1 0.2629058 6.843093 0.03025047 5.329169 0.2939027 0.02235823
    ##       MAESD
    ## 1 0.1850066

``` r
coef(lassoFitBehav5$finalModel, lassoFitBehav5$bestTune$lambda)
```

    ## 47 x 1 sparse Matrix of class "dgCMatrix"
    ##                            1
    ## (Intercept)     24.693631778
    ## y5_FSTFOOD4      .          
    ## y7_FSTFOOD4      .          
    ## y5_FSTFOOD5      .          
    ## y7_FSTFOOD5      .          
    ## y4_ALONE         .          
    ## y5_ALONE         .          
    ## y4_SECRET        .          
    ## y5_SECRET        0.171141627
    ## y6_SECRET        .          
    ## y7_SECRET        0.126557662
    ## y4_eat_VITAMINS  .          
    ## y4_eat_VEGGIE    .          
    ## y4_eat_NOTHUNGY  .          
    ## y4_eat_SNKFOOD1  .          
    ## y4_eat_BHELPS    0.001367341
    ## y4_eat_BEDRM     .          
    ## y4_eat_SNKFOOD2  .          
    ## y4_eat_DESSERT   .          
    ## y5_eat_VITAMINS  .          
    ## y5_eat_VEGGIE   -0.040968797
    ## y5_eat_NOTHUNGY  .          
    ## y5_eat_SNKFOOD1  .          
    ## y5_eat_BHELPS    .          
    ## y5_eat_BEDRM     .          
    ## y5_eat_SNKFOOD2  .          
    ## y5_eat_DESSERT   .          
    ## y6_eat_NOTHUNGY  0.180641813
    ## y6_eat_BHELPS    .          
    ## y6_eat_BEDRM     .          
    ## y6_eat_SNKFOOD2  .          
    ## y6_eat_DESSERT   .          
    ## y7_eat_NOTHUNGY  0.672259881
    ## y7_eat_BHELPS    .          
    ## y7_eat_BEDRM     .          
    ## y7_eat_DESSERT   .          
    ## y5_AFTSNK1       .          
    ## y5_AFTSNK2       .          
    ## y7_AFTSNK2       .          
    ## y9_AFTSNK2       .          
    ## y4_BREAKFST2     .          
    ## y5_BREAKFST2     .          
    ## y6_BREAKFST2     .          
    ## y7_BREAKFST2     .          
    ## y8_BREAKFST2     .          
    ## y9_BREAKFST2     .          
    ## y5_eat_STOPEAT2 -0.026634375

``` r
tmp_coeffs <- coef(lassoFitBehav5$finalModel, lassoFitBehav5$bestTune$lambda)

coefs <- 
  data.frame(name = tmp_coeffs@Dimnames[[1]][tmp_coeffs@i + 1], coefficient = tmp_coeffs@x)

coefs %>%
  filter(name != "(Intercept)") %>% 
  ggplot(aes(fct_reorder(name, coefficient), coefficient)) +
  geom_point() +
  coord_flip()
```

![](nghs1_var_selection_files/figure-markdown_github/unnamed-chunk-42-1.png)

We don't see a ton of overlap between top predictors of BMI and eating behaviors predicted by affect. We select year 5 stop eat, year 6 snackfood, year 4 vitamins, and year 7 secret as these showed up in each of the two categories.
