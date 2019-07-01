NGHS1 Supplemental Analyses
================
Daniel O'Leary
April 15, 2019

-   [Setup](#setup)
    -   [Load Packages](#load-packages)
    -   [Load Data](#load-data)
-   [Models](#models)
    -   [Income predicting weight gain](#income-predicting-weight-gain)
    -   [Income predicting affect](#income-predicting-affect)
    -   [Affect predicting weight gain](#affect-predicting-weight-gain)
    -   [Psychological variables predicting weight gain](#psychological-variables-predicting-weight-gain)
    -   [Eating behavior variables predicting weight gain](#eating-behavior-variables-predicting-weight-gain)
-   [Mediation](#mediation)
    -   [Income --&gt; Stress --&gt; Weight](#income----stress----weight)
    -   [Income --&gt; Anxiety --&gt; Weight](#income----anxiety----weight)
    -   [Multiple mediation - stress & psychological variables](#multiple-mediation---stress-psychological-variables)
    -   [Multiple mediation - anxiety & psychological variables](#multiple-mediation---anxiety-psychological-variables)

Setup
=====

Load Packages
-------------

Load Data
---------

``` r
semTrainingData <- 
  read_csv("G:/My Drive/research/projects/phsr1/nghs1/nghs1_data/nghs1_sem_training_data.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double()
    ## )

    ## See spec(...) for full column specifications.

``` r
semTestingData <- 
  read_csv("G:/My Drive/research/projects/phsr1/nghs1/nghs1_data/nghs1_sem_testing_data.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double()
    ## )
    ## See spec(...) for full column specifications.

``` r
semAllData <-
  read_csv("G:/My Drive/research/projects/phsr1/nghs1/nghs1_data/nghs1_imputed_new2.csv") %>% 
  mutate(y0_pbmi = (y0_WT_fm2 * 703) / (y0_HTFT_fm2 * 12 * y0_HTFT_fm2 * 12))
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double()
    ## )
    ## See spec(...) for full column specifications.

Models
======

``` r
cor.test(semAllData$y2_STRESS, semAllData$y3_ANXR)
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  semAllData$y2_STRESS and semAllData$y3_ANXR
    ## t = 19.167, df = 2377, p-value < 2.2e-16
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  0.3305460 0.4001768
    ## sample estimates:
    ##       cor 
    ## 0.3658733

Income predicting weight gain
-----------------------------

``` r
income_bmi <-
  lm(
    y10_QI ~
      y0_CATINC + y0_QI,
    data = semAllData
  )

summary(income_bmi)
```

    ## 
    ## Call:
    ## lm(formula = y10_QI ~ y0_CATINC + y0_QI, data = semAllData)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -14.4089  -2.3751  -0.4744   1.8501  27.3400 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  2.04901    0.47881   4.279 1.95e-05 ***
    ## y0_CATINC   -0.63168    0.07519  -8.401  < 2e-16 ***
    ## y0_QI        1.36553    0.02173  62.838  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 4.041 on 2376 degrees of freedom
    ## Multiple R-squared:  0.6334, Adjusted R-squared:  0.6331 
    ## F-statistic:  2053 on 2 and 2376 DF,  p-value: < 2.2e-16

``` r
lm.beta(income_bmi)
```

    ## 
    ## Call:
    ## lm(formula = y10_QI ~ y0_CATINC + y0_QI, data = semAllData)
    ## 
    ## Standardized Coefficients::
    ## (Intercept)   y0_CATINC       y0_QI 
    ##   0.0000000  -0.1045745   0.7821978

Income predicting affect
------------------------

``` r
stress_inc <-
  lm(
    y2_STRESS ~
      y0_CATINC,
    data = semAllData
  )

summary(stress_inc)
```

    ## 
    ## Call:
    ## lm(formula = y2_STRESS ~ y0_CATINC, data = semAllData)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -25.9516  -4.2903   0.0768   4.0768  25.5910 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  26.4659     0.3945  67.096  < 2e-16 ***
    ## y0_CATINC    -0.5142     0.1264  -4.067 4.91e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 6.809 on 2377 degrees of freedom
    ## Multiple R-squared:  0.006912,   Adjusted R-squared:  0.006494 
    ## F-statistic: 16.54 on 1 and 2377 DF,  p-value: 4.91e-05

``` r
lm.beta(stress_inc)
```

    ## 
    ## Call:
    ## lm(formula = y2_STRESS ~ y0_CATINC, data = semAllData)
    ## 
    ## Standardized Coefficients::
    ## (Intercept)   y0_CATINC 
    ##  0.00000000 -0.08313633

``` r
anx_inc <-
  lm(
    y3_ANXR ~
      y0_CATINC,
    data = semAllData
  )

summary(anx_inc)
```

    ## 
    ## Call:
    ## lm(formula = y3_ANXR ~ y0_CATINC, data = semAllData)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -11.9181  -5.2296  -0.5411   4.8754  18.1475 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  12.6066     0.3710  33.984  < 2e-16 ***
    ## y0_CATINC    -0.6885     0.1189  -5.791 7.92e-09 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 6.404 on 2377 degrees of freedom
    ## Multiple R-squared:  0.01391,    Adjusted R-squared:  0.0135 
    ## F-statistic: 33.53 on 1 and 2377 DF,  p-value: 7.925e-09

``` r
lm.beta(anx_inc)
```

    ## 
    ## Call:
    ## lm(formula = y3_ANXR ~ y0_CATINC, data = semAllData)
    ## 
    ## Standardized Coefficients::
    ## (Intercept)   y0_CATINC 
    ##   0.0000000  -0.1179482

Affect predicting weight gain
-----------------------------

``` r
stress_bmi <-
  lm(
    y10_QI ~
      y2_STRESS + y0_QI,
    data = semAllData
  )

summary(stress_bmi)
```

    ## 
    ## Call:
    ## lm(formula = y10_QI ~ y2_STRESS + y0_QI, data = semAllData)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -13.6530  -2.3387  -0.5162   1.7978  28.1506 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -0.86009    0.50723  -1.696  0.09008 .  
    ## y2_STRESS    0.03590    0.01230   2.919  0.00354 ** 
    ## y0_QI        1.37466    0.02199  62.526  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 4.094 on 2376 degrees of freedom
    ## Multiple R-squared:  0.6238, Adjusted R-squared:  0.6235 
    ## F-statistic:  1970 on 2 and 2376 DF,  p-value: < 2.2e-16

``` r
lm.beta(stress_bmi)
```

    ## 
    ## Call:
    ## lm(formula = y10_QI ~ y2_STRESS + y0_QI, data = semAllData)
    ## 
    ## Standardized Coefficients::
    ## (Intercept)   y2_STRESS       y0_QI 
    ##  0.00000000  0.03676368  0.78742792

``` r
anx_bmi <-
  lm(
    y10_QI ~
      y3_ANXR + y0_QI,
    data = semAllData
  )

summary(anx_bmi)
```

    ## 
    ## Call:
    ## lm(formula = y10_QI ~ y3_ANXR + y0_QI, data = semAllData)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -14.2538  -2.3779  -0.5014   1.8183  27.9003 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -0.67135    0.42844  -1.567    0.117    
    ## y3_ANXR      0.07732    0.01298   5.959 2.92e-09 ***
    ## y0_QI        1.36864    0.02189  62.519  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 4.071 on 2376 degrees of freedom
    ## Multiple R-squared:  0.6281, Adjusted R-squared:  0.6277 
    ## F-statistic:  2006 on 2 and 2376 DF,  p-value: < 2.2e-16

``` r
lm.beta(anx_bmi)
```

    ## 
    ## Call:
    ## lm(formula = y10_QI ~ y3_ANXR + y0_QI, data = semAllData)
    ## 
    ## Standardized Coefficients::
    ## (Intercept)     y3_ANXR       y0_QI 
    ##  0.00000000  0.07472074  0.78397884

Psychological variables predicting weight gain
----------------------------------------------

``` r
edi_bmi1 <-
  lm(
    y10_QI ~
      y5_AWARE + y5_DISTRST + y5_INEFCT + y5_SELWT,
    data = semAllData
  )

summary(edi_bmi1)
```

    ## 
    ## Call:
    ## lm(formula = y10_QI ~ y5_AWARE + y5_DISTRST + y5_INEFCT + y5_SELWT, 
    ##     data = semAllData)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -16.452  -4.458  -1.670   2.896  28.612 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 26.24035    0.87225  30.083  < 2e-16 ***
    ## y5_AWARE     0.18442    0.03446   5.351 9.59e-08 ***
    ## y5_DISTRST   0.14984    0.04415   3.394 0.000701 ***
    ## y5_INEFCT    0.06663    0.04875   1.367 0.171886    
    ## y5_SELWT    -0.68863    0.25128  -2.740 0.006181 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 6.506 on 2374 degrees of freedom
    ## Multiple R-squared:  0.05078,    Adjusted R-squared:  0.04918 
    ## F-statistic: 31.75 on 4 and 2374 DF,  p-value: < 2.2e-16

``` r
lm.beta(edi_bmi1)
```

    ## 
    ## Call:
    ## lm(formula = y10_QI ~ y5_AWARE + y5_DISTRST + y5_INEFCT + y5_SELWT, 
    ##     data = semAllData)
    ## 
    ## Standardized Coefficients::
    ## (Intercept)    y5_AWARE  y5_DISTRST   y5_INEFCT    y5_SELWT 
    ##  0.00000000  0.12772894  0.07443009  0.03833306 -0.06545793

``` r
edi_bmi2 <-
  lm(
    y10_QI ~
      y5_AWARE + y5_DISTRST + y5_INEFCT + y5_SELWT + y0_QI,
    data = semAllData
  )

summary(edi_bmi2)
```

    ## 
    ## Call:
    ## lm(formula = y10_QI ~ y5_AWARE + y5_DISTRST + y5_INEFCT + y5_SELWT + 
    ##     y0_QI, data = semAllData)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -13.5008  -2.3792  -0.4011   1.8384  27.0363 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -0.28536    0.69041  -0.413 0.679413    
    ## y5_AWARE     0.08309    0.02142   3.880 0.000107 ***
    ## y5_DISTRST   0.13541    0.02736   4.949 7.96e-07 ***
    ## y5_INEFCT    0.03757    0.03021   1.243 0.213808    
    ## y5_SELWT    -0.05221    0.15603  -0.335 0.737937    
    ## y0_QI        1.35168    0.02189  61.736  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 4.031 on 2373 degrees of freedom
    ## Multiple R-squared:  0.6358, Adjusted R-squared:  0.635 
    ## F-statistic: 828.4 on 5 and 2373 DF,  p-value: < 2.2e-16

``` r
lm.beta(edi_bmi2)
```

    ## 
    ## Call:
    ## lm(formula = y10_QI ~ y5_AWARE + y5_DISTRST + y5_INEFCT + y5_SELWT + 
    ##     y0_QI, data = semAllData)
    ## 
    ## Standardized Coefficients::
    ##  (Intercept)     y5_AWARE   y5_DISTRST    y5_INEFCT     y5_SELWT 
    ##  0.000000000  0.057546542  0.067259711  0.021613670 -0.004962943 
    ##        y0_QI 
    ##  0.774262824

Eating behavior variables predicting weight gain
------------------------------------------------

``` r
eat_bmi1 <-
  lm(
    y10_QI ~
      y6_eat_SNKFOOD2 + y5_eat_STOPEAT + y7_SECRET,
    data = semAllData
  )

summary(eat_bmi1)
```

    ## 
    ## Call:
    ## lm(formula = y10_QI ~ y6_eat_SNKFOOD2 + y5_eat_STOPEAT + y7_SECRET, 
    ##     data = semAllData)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -12.237  -4.366  -1.492   2.815  32.218 
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)      35.0654     0.8379  41.850  < 2e-16 ***
    ## y6_eat_SNKFOOD2  -1.3927     0.2117  -6.579  5.8e-11 ***
    ## y5_eat_STOPEAT   -4.2518     0.2934 -14.490  < 2e-16 ***
    ## y7_SECRET         1.0032     0.3574   2.807  0.00504 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 6.33 on 2375 degrees of freedom
    ## Multiple R-squared:  0.1008, Adjusted R-squared:  0.0997 
    ## F-statistic: 88.78 on 3 and 2375 DF,  p-value: < 2.2e-16

``` r
lm.beta(eat_bmi1)
```

    ## 
    ## Call:
    ## lm(formula = y10_QI ~ y6_eat_SNKFOOD2 + y5_eat_STOPEAT + y7_SECRET, 
    ##     data = semAllData)
    ## 
    ## Standardized Coefficients::
    ##     (Intercept) y6_eat_SNKFOOD2  y5_eat_STOPEAT       y7_SECRET 
    ##      0.00000000     -0.12827418     -0.28196406      0.05472123

``` r
eat_bmi2 <-
  lm(
    y10_QI ~
      y6_eat_SNKFOOD2 + y5_eat_STOPEAT + y7_SECRET + y0_QI,
    data = semAllData
  )

summary(eat_bmi2)
```

    ## 
    ## Call:
    ## lm(formula = y10_QI ~ y6_eat_SNKFOOD2 + y5_eat_STOPEAT + y7_SECRET + 
    ##     y0_QI, data = semAllData)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -13.224  -2.345  -0.445   1.829  27.601 
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)      3.51750    0.76047   4.625 3.94e-06 ***
    ## y6_eat_SNKFOOD2 -0.56325    0.13623  -4.135 3.68e-05 ***
    ## y5_eat_STOPEAT  -1.15966    0.19510  -5.944 3.20e-09 ***
    ## y7_SECRET        0.61331    0.22887   2.680  0.00742 ** 
    ## y0_QI            1.32946    0.02272  58.511  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 4.052 on 2374 degrees of freedom
    ## Multiple R-squared:  0.6318, Adjusted R-squared:  0.6312 
    ## F-statistic:  1018 on 4 and 2374 DF,  p-value: < 2.2e-16

``` r
lm.beta(eat_bmi2)
```

    ## 
    ## Call:
    ## lm(formula = y10_QI ~ y6_eat_SNKFOOD2 + y5_eat_STOPEAT + y7_SECRET + 
    ##     y0_QI, data = semAllData)
    ## 
    ## Standardized Coefficients::
    ##     (Intercept) y6_eat_SNKFOOD2  y5_eat_STOPEAT       y7_SECRET 
    ##      0.00000000     -0.05187915     -0.07690383      0.03345271 
    ##           y0_QI 
    ##      0.76153483

Mediation
=========

Income --&gt; Stress --&gt; Weight
----------------------------------

``` r
lm1 <-
  lm(
    y2_STRESS ~
      y0_CATINC,
    data =
      semAllData %>%
      dplyr::select(y10_QI, y2_STRESS, y0_CATINC) %>%
      distinct() %>%
      filter(!is.na(y10_QI))
  )

summary(lm1)
```

    ## 
    ## Call:
    ## lm(formula = y2_STRESS ~ y0_CATINC, data = semAllData %>% dplyr::select(y10_QI, 
    ##     y2_STRESS, y0_CATINC) %>% distinct() %>% filter(!is.na(y10_QI)))
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -25.9516  -4.2903   0.0768   4.0768  25.5910 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  26.4659     0.3945  67.096  < 2e-16 ***
    ## y0_CATINC    -0.5142     0.1264  -4.067 4.91e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 6.809 on 2377 degrees of freedom
    ## Multiple R-squared:  0.006912,   Adjusted R-squared:  0.006494 
    ## F-statistic: 16.54 on 1 and 2377 DF,  p-value: 4.91e-05

``` r
lm2 <-
  lm(
    y10_QI ~
      y2_STRESS + y0_CATINC,
    data =
      semAllData %>%
      dplyr::select(y10_QI, y2_STRESS, y0_CATINC) %>%
      distinct() %>%
      filter(!is.na(y10_QI))
  )

summary(lm2)
```

    ## 
    ## Call:
    ## lm(formula = y10_QI ~ y2_STRESS + y0_CATINC, data = semAllData %>% 
    ##     dplyr::select(y10_QI, y2_STRESS, y0_CATINC) %>% distinct() %>% 
    ##     filter(!is.na(y10_QI)))
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -11.232  -4.577  -1.854   2.814  30.861 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 26.80543    0.64864  41.326  < 2e-16 ***
    ## y2_STRESS    0.05629    0.01983   2.839  0.00456 ** 
    ## y0_CATINC   -0.90948    0.12263  -7.416 1.67e-13 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 6.582 on 2376 degrees of freedom
    ## Multiple R-squared:  0.02743,    Adjusted R-squared:  0.02662 
    ## F-statistic: 33.51 on 2 and 2376 DF,  p-value: 4.441e-15

``` r
med.output <- mediation::mediate(lm1, lm2, treat = "y0_CATINC", mediator = "y2_STRESS")

summary(med.output)
```

    ## 
    ## Causal Mediation Analysis 
    ## 
    ## Quasi-Bayesian Confidence Intervals
    ## 
    ##                Estimate 95% CI Lower 95% CI Upper p-value    
    ## ACME            -0.0297      -0.0555        -0.01   0.008 ** 
    ## ADE             -0.9096      -1.1443        -0.67  <2e-16 ***
    ## Total Effect    -0.9393      -1.1768        -0.70  <2e-16 ***
    ## Prop. Mediated   0.0312       0.0084         0.06   0.008 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Sample Size Used: 2379 
    ## 
    ## 
    ## Simulations: 1000

Income --&gt; Anxiety --&gt; Weight
-----------------------------------

``` r
lm1 <-
  lm(
    y3_ANXR ~
      y0_CATINC,
    data =
      semAllData %>%
      dplyr::select(y10_QI, y3_ANXR, y0_CATINC) %>%
      distinct() %>%
      filter(!is.na(y10_QI))
  )

summary(lm1)
```

    ## 
    ## Call:
    ## lm(formula = y3_ANXR ~ y0_CATINC, data = semAllData %>% dplyr::select(y10_QI, 
    ##     y3_ANXR, y0_CATINC) %>% distinct() %>% filter(!is.na(y10_QI)))
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -11.9181  -5.2296  -0.5411   4.8754  18.1475 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  12.6066     0.3710  33.984  < 2e-16 ***
    ## y0_CATINC    -0.6885     0.1189  -5.791 7.92e-09 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 6.404 on 2377 degrees of freedom
    ## Multiple R-squared:  0.01391,    Adjusted R-squared:  0.0135 
    ## F-statistic: 33.53 on 1 and 2377 DF,  p-value: 7.925e-09

``` r
lm2 <-
  lm(
    y10_QI ~
      y3_ANXR + y0_CATINC,
    data =
      semAllData %>%
      dplyr::select(y10_QI, y3_ANXR, y0_CATINC) %>%
      distinct() %>%
      filter(!is.na(y10_QI))
  )

summary(lm2)
```

    ## 
    ## Call:
    ## lm(formula = y10_QI ~ y3_ANXR + y0_CATINC, data = semAllData %>% 
    ##     dplyr::select(y10_QI, y3_ANXR, y0_CATINC) %>% distinct() %>% 
    ##     filter(!is.na(y10_QI)))
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -11.963  -4.508  -1.743   2.790  30.246 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 26.85399    0.46269  58.039  < 2e-16 ***
    ## y3_ANXR      0.11432    0.02099   5.447 5.65e-08 ***
    ## y0_CATINC   -0.85971    0.12251  -7.017 2.94e-12 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 6.553 on 2376 degrees of freedom
    ## Multiple R-squared:  0.03617,    Adjusted R-squared:  0.03536 
    ## F-statistic: 44.58 on 2 and 2376 DF,  p-value: < 2.2e-16

``` r
med.output <- mediation::mediate(lm1, lm2, treat = "y0_CATINC", mediator = "y3_ANXR")

summary(med.output)
```

    ## 
    ## Causal Mediation Analysis 
    ## 
    ## Quasi-Bayesian Confidence Intervals
    ## 
    ##                Estimate 95% CI Lower 95% CI Upper p-value    
    ## ACME            -0.0788      -0.1225        -0.04  <2e-16 ***
    ## ADE             -0.8644      -1.1041        -0.64  <2e-16 ***
    ## Total Effect    -0.9432      -1.1828        -0.71  <2e-16 ***
    ## Prop. Mediated   0.0817       0.0437         0.14  <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Sample Size Used: 2379 
    ## 
    ## 
    ## Simulations: 1000

Multiple mediation - stress & psychological variables
-----------------------------------------------------

``` r
data <-
  data.frame(
    X = semAllData$y0_CATINC,
    X2 = semAllData$y0_QI,
    X3 = semAllData$y0_RACE,
    X4 = semAllData$y0_pbmi,
    Y = semAllData$y10_QI,
    M1 = semAllData$y2_STRESS,
    M2 = semAllData$y5_AWARE,
    M3 = semAllData$y5_INEFCT,
    M4 = semAllData$y5_SELWT,
    M5 = semAllData$y5_DISTRST
  )

model <- 
  '
    # direct effect
      Y ~ b1 * M1 + b2 * M2 + b3 * M3 + b4 * M4 + b5* M5 + c * X + c2 * X2 + c3 * X3 + c4 * X4
    
    # mediator
      M1 ~ a1 * X
      M2 ~ a2 * X
      M3 ~ a3 * X
      M4 ~ a4 * X
      M5 ~ a5 * X  
    
    
    # direct effect
      direct := c
    
    # indirect effect (a*b)
      indirect1 := a1 * b1
      indirect2 := a2 * b2
      indirect3 := a3 * b3
      indirect4 := a4 * b4
      indirect5 := a5 * b5
    
    
    # total effect
      total := c + (a1 * b1) + (a2 * b2)  + (a3 * b3) + (a4 * b4) + (a5 * b5)
      prop_mediated := (abs(indirect1) + abs(indirect2) + abs(indirect3) + abs(indirect4) + abs(indirect5) )/abs(total)
    
    # covariances
    
      M1 ~~ M2 + M3 + M4 + M5
      M2 ~~ M3 + M4 + M5

'
fit <- 
  sem(
    model, 
    data = data
  )

summary(
  fit, 
  standardized = TRUE, 
  fit.measures = T, 
  rsq = T
)
```

    ## lavaan 0.6-3 ended normally after 111 iterations
    ## 
    ##   Optimization method                           NLMINB
    ##   Number of free parameters                         27
    ## 
    ##   Number of observations                          2379
    ## 
    ##   Estimator                                         ML
    ##   Model Fit Test Statistic                    1659.382
    ##   Degrees of freedom                                18
    ##   P-value (Chi-square)                           0.000
    ## 
    ## Model test baseline model:
    ## 
    ##   Minimum Function Test Statistic             5398.430
    ##   Degrees of freedom                                39
    ##   P-value                                        0.000
    ## 
    ## User model versus baseline model:
    ## 
    ##   Comparative Fit Index (CFI)                    0.694
    ##   Tucker-Lewis Index (TLI)                       0.336
    ## 
    ## Loglikelihood and Information Criteria:
    ## 
    ##   Loglikelihood user model (H0)             -36076.105
    ##   Loglikelihood unrestricted model (H1)     -35246.414
    ## 
    ##   Number of free parameters                         27
    ##   Akaike (AIC)                               72206.210
    ##   Bayesian (BIC)                             72362.119
    ##   Sample-size adjusted Bayesian (BIC)        72276.335
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.196
    ##   90 Percent Confidence Interval          0.188  0.204
    ##   P-value RMSEA <= 0.05                          0.000
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.125
    ## 
    ## Parameter Estimates:
    ## 
    ##   Information                                 Expected
    ##   Information saturated (h1) model          Structured
    ##   Standard Errors                             Standard
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   Y ~                                                                   
    ##     M1        (b1)    0.005    0.012    0.404    0.686    0.005    0.005
    ##     M2        (b2)    0.049    0.021    2.340    0.019    0.049    0.034
    ##     M3        (b3)    0.036    0.025    1.471    0.141    0.036    0.021
    ##     M4        (b4)   -0.204    0.129   -1.588    0.112   -0.204   -0.020
    ##     M5        (b5)    0.077    0.025    3.085    0.002    0.077    0.039
    ##     X          (c)   -0.419    0.080   -5.231    0.000   -0.419   -0.070
    ##     X2        (c2)    1.292    0.022   58.151    0.000    1.292    0.748
    ##     X3        (c3)    0.479    0.175    2.742    0.006    0.479    0.036
    ##     X4        (c4)    0.090    0.012    7.194    0.000    0.090    0.093
    ##   M1 ~                                                                  
    ##     X         (a1)   -0.514    0.124   -4.130    0.000   -0.514   -0.084
    ##   M2 ~                                                                  
    ##     X         (a2)   -0.815    0.083   -9.856    0.000   -0.815   -0.198
    ##   M3 ~                                                                  
    ##     X         (a3)   -0.417    0.071   -5.899    0.000   -0.417   -0.120
    ##   M4 ~                                                                  
    ##     X         (a4)    0.009    0.012    0.752    0.452    0.009    0.015
    ##   M5 ~                                                                  
    ##     X         (a5)   -0.607    0.060  -10.073    0.000   -0.607   -0.202
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##  .M1 ~~                                                                 
    ##    .M2                3.891    0.615    6.328    0.000    3.891    0.130
    ##    .M3                3.542    0.523    6.778    0.000    3.542    0.139
    ##    .M4               -0.625    0.087   -7.175    0.000   -0.625   -0.147
    ##    .M5                0.980    0.437    2.241    0.025    0.980    0.045
    ##  .M2 ~~                                                                 
    ##    .M3                8.587    0.389   22.064    0.000    8.587    0.506
    ##    .M4               -0.070    0.050   -1.414    0.157   -0.070   -0.025
    ##    .M5                0.798    0.256    3.119    0.002    0.798    0.055
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .Y                15.495    0.449   34.489    0.000   15.495    0.356
    ##    .M1               44.963    1.303   34.506    0.000   44.963    0.993
    ##    .M2               19.815    0.574   34.522    0.000   19.815    0.961
    ##    .M3               14.516    0.421   34.489    0.000   14.516    0.986
    ##    .M4                0.402    0.012   34.489    0.000    0.402    1.000
    ##    .M5               10.529    0.305   34.489    0.000   10.529    0.959
    ## 
    ## R-Square:
    ##                    Estimate
    ##     Y                 0.644
    ##     M1                0.007
    ##     M2                0.039
    ##     M3                0.014
    ##     M4                0.000
    ##     M5                0.041
    ## 
    ## Defined Parameters:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##     direct           -0.419    0.080   -5.231    0.000   -0.419   -0.070
    ##     indirect1        -0.003    0.006   -0.402    0.687   -0.003   -0.000
    ##     indirect2        -0.040    0.018   -2.277    0.023   -0.040   -0.007
    ##     indirect3        -0.015    0.011   -1.427    0.153   -0.015   -0.003
    ##     indirect4        -0.002    0.003   -0.680    0.496   -0.002   -0.000
    ##     indirect5        -0.047    0.016   -2.950    0.003   -0.047   -0.008
    ##     total            -0.526    0.078   -6.760    0.000   -0.526   -0.088
    ##     prop_mediated     0.203                               0.203    0.203

Multiple mediation - anxiety & psychological variables
------------------------------------------------------

``` r
data <-
  data.frame(
    X = semAllData$y0_CATINC,
    X2 = semAllData$y0_QI,
    X3 = semAllData$y0_RACE,
    X4 = semAllData$y0_pbmi,
    Y = semAllData$y10_QI,
    M1 = semAllData$y3_ANXR,
    M2 = semAllData$y5_AWARE,
    M3 = semAllData$y5_INEFCT,
    M4 = semAllData$y5_SELWT,
    M5 = semAllData$y5_DISTRST
  )

model <- 
  '
    # direct effect
      Y ~ b1 * M1 + b2 * M2 + b3 * M3 + b4 * M4 + b5* M5 + c * X + c2 * X2 + c3 * X3 + c4 * X4
    
    # mediator
      M1 ~ a1 * X
      M2 ~ a2 * X
      M3 ~ a3 * X
      M4 ~ a4 * X
      M5 ~ a5 * X  
    
    
    # direct effect
      direct := c
    
    # indirect effect (a*b)
      indirect1 := a1 * b1
      indirect2 := a2 * b2
      indirect3 := a3 * b3
      indirect4 := a4 * b4
      indirect5 := a5 * b5
    
    
    # total effect
      total := c + (a1 * b1) + (a2 * b2)  + (a3 * b3) + (a4 * b4) + (a5 * b5)
      prop_mediated := (abs(indirect1) + abs(indirect2) + abs(indirect3) + abs(indirect4) + abs(indirect5) )/abs(total)
    
    # covariances
    
      M1 ~~ M2 + M3 + M4 + M5
      M2 ~~ M3 + M4 + M5

'
fit <- 
  sem(
    model, 
    data = data
  )

summary(
  fit, 
  standardized = TRUE, 
  fit.measures = T, 
  rsq = T
)
```

    ## lavaan 0.6-3 ended normally after 110 iterations
    ## 
    ##   Optimization method                           NLMINB
    ##   Number of free parameters                         27
    ## 
    ##   Number of observations                          2379
    ## 
    ##   Estimator                                         ML
    ##   Model Fit Test Statistic                    1664.007
    ##   Degrees of freedom                                18
    ##   P-value (Chi-square)                           0.000
    ## 
    ## Model test baseline model:
    ## 
    ##   Minimum Function Test Statistic             5858.300
    ##   Degrees of freedom                                39
    ##   P-value                                        0.000
    ## 
    ## User model versus baseline model:
    ## 
    ##   Comparative Fit Index (CFI)                    0.717
    ##   Tucker-Lewis Index (TLI)                       0.387
    ## 
    ## Loglikelihood and Information Criteria:
    ## 
    ##   Loglikelihood user model (H0)             -35710.839
    ##   Loglikelihood unrestricted model (H1)     -34878.835
    ## 
    ##   Number of free parameters                         27
    ##   Akaike (AIC)                               71475.678
    ##   Bayesian (BIC)                             71631.588
    ##   Sample-size adjusted Bayesian (BIC)        71545.803
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.196
    ##   90 Percent Confidence Interval          0.188  0.204
    ##   P-value RMSEA <= 0.05                          0.000
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.129
    ## 
    ## Parameter Estimates:
    ## 
    ##   Information                                 Expected
    ##   Information saturated (h1) model          Structured
    ##   Standard Errors                             Standard
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   Y ~                                                                   
    ##     M1        (b1)    0.027    0.014    1.850    0.064    0.027    0.025
    ##     M2        (b2)    0.041    0.022    1.910    0.056    0.041    0.028
    ##     M3        (b3)    0.035    0.025    1.412    0.158    0.035    0.020
    ##     M4        (b4)   -0.138    0.133   -1.038    0.299   -0.138   -0.013
    ##     M5        (b5)    0.074    0.025    2.954    0.003    0.074    0.037
    ##     X          (c)   -0.414    0.080   -5.168    0.000   -0.414   -0.069
    ##     X2        (c2)    1.293    0.022   58.239    0.000    1.293    0.748
    ##     X3        (c3)    0.478    0.175    2.737    0.006    0.478    0.036
    ##     X4        (c4)    0.089    0.012    7.104    0.000    0.089    0.092
    ##   M1 ~                                                                  
    ##     X         (a1)   -0.689    0.114   -6.025    0.000   -0.689   -0.123
    ##   M2 ~                                                                  
    ##     X         (a2)   -0.815    0.083   -9.856    0.000   -0.815   -0.198
    ##   M3 ~                                                                  
    ##     X         (a3)   -0.417    0.071   -5.899    0.000   -0.417   -0.120
    ##   M4 ~                                                                  
    ##     X         (a4)    0.009    0.012    0.752    0.452    0.009    0.015
    ##   M5 ~                                                                  
    ##     X         (a5)   -0.607    0.060  -10.073    0.000   -0.607   -0.202
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##  .M1 ~~                                                                 
    ##    .M2                7.681    0.576   13.329    0.000    7.681    0.280
    ##    .M3                4.222    0.466    9.056    0.000    4.222    0.180
    ##    .M4               -1.132    0.082  -13.851    0.000   -1.132   -0.290
    ##    .M5                1.748    0.387    4.522    0.000    1.748    0.088
    ##  .M2 ~~                                                                 
    ##    .M3                8.587    0.389   22.064    0.000    8.587    0.506
    ##    .M4               -0.070    0.050   -1.414    0.157   -0.070   -0.025
    ##    .M5                0.798    0.256    3.119    0.002    0.798    0.055
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .Y                15.474    0.449   34.489    0.000   15.474    0.355
    ##    .M1               37.882    1.094   34.615    0.000   37.882    0.985
    ##    .M2               19.815    0.574   34.522    0.000   19.815    0.961
    ##    .M3               14.516    0.421   34.489    0.000   14.516    0.986
    ##    .M4                0.402    0.012   34.489    0.000    0.402    1.000
    ##    .M5               10.529    0.305   34.489    0.000   10.529    0.959
    ## 
    ## R-Square:
    ##                    Estimate
    ##     Y                 0.645
    ##     M1                0.015
    ##     M2                0.039
    ##     M3                0.014
    ##     M4                0.000
    ##     M5                0.041
    ## 
    ## Defined Parameters:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##     direct           -0.414    0.080   -5.168    0.000   -0.414   -0.069
    ##     indirect1        -0.018    0.010   -1.769    0.077   -0.018   -0.003
    ##     indirect2        -0.034    0.018   -1.875    0.061   -0.034   -0.006
    ##     indirect3        -0.014    0.011   -1.373    0.170   -0.014   -0.002
    ##     indirect4        -0.001    0.002   -0.609    0.542   -0.001   -0.000
    ##     indirect5        -0.045    0.016   -2.834    0.005   -0.045   -0.007
    ##     total            -0.526    0.078   -6.768    0.000   -0.526   -0.088
    ##     prop_mediated     0.214                               0.214    0.214
