NGHS1 Path Analysis
================
Daniel O'Leary
November 14, 2018

-   [Setup](#setup)
    -   [Load Packages](#load-packages)
    -   [Load Data](#load-data)
    -   [Check Dimensions](#check-dimensions)
-   [Mediation](#mediation)
-   [Path Models](#path-models)
    -   [Stress Psych Initial Model](#stress-psych-initial-model)
    -   [Stress Psych Pruned Model](#stress-psych-pruned-model)
    -   [Anxiety Psych Initial Model](#anxiety-psych-initial-model)
    -   [Anxiety Psych Pruned Model](#anxiety-psych-pruned-model)
    -   [Stress Behavior Initial Model](#stress-behavior-initial-model)
    -   [Stress Behavior Pruned Model](#stress-behavior-pruned-model)
    -   [Anxiety Behavior Initial Model](#anxiety-behavior-initial-model)
    -   [Anxiety Behavior Pruned Model](#anxiety-behavior-pruned-model)
    -   [Anxiety Psych & Behavior Combined Model](#anxiety-psych-behavior-combined-model)

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

Check Dimensions
----------------

``` r
dim(semTrainingData)
```

    ## [1]  793 1627

``` r
dim(semTestingData)
```

    ## [1]  793 1627

Mediation
=========

``` r
# lm1 <- 
#   lm(
#     y5_AWARE ~ 
#       y0_CATINC, 
#     data = 
#       dfTest %>% 
#       dplyr::select(y10_QI, y5_AWARE, y0_CATINC) %>% 
#       distinct() %>% 
#       filter(!is.na(y10_QI))
#   )
# 
# summary(lm1)
# 
# lm2 <- 
#   lm(
#     y10_QI ~ 
#       y5_AWARE + y0_CATINC + y5_QI, 
#     data = 
#       dfTest %>% 
#       dplyr::select(y10_QI, y5_AWARE, y0_CATINC, y5_QI) %>% 
#       distinct() %>% 
#       filter(!is.na(y10_QI))
#   )
# 
# summary(lm2)
# 
# 
# med.output <- mediation::mediate(lm1, lm2, treat = "y0_CATINC", mediator = "y5_AWARE")
# 
# summary(med.output)
```

Good fit for structural equation models (SEM):

-   Chi-square p-val &gt; 0.05 \[not necc. w/ large sample size\]
-   TLI &gt;= 0.95
-   CFI &gt;= 0.90
-   RMSEA &lt; 0.08
-   SRMR &lt; 0.08

Path Models
===========

Stress Psych Initial Model
--------------------------

``` r
mod_psych_stress_initial <-
  '
    #################
    ## Regressions ##
    #################  
     
    y10_QI ~ y0_CATINC + y0_QI + y0_pbmi_fm2 + y0_RACE  

    y10_QI ~ y5_AWARE + y5_DISTRST + y5_INEFCT + y5_SELWT + y2_STRESS

    y5_AWARE + y5_DISTRST + y5_INEFCT + y5_SELWT ~ y2_STRESS

    y5_AWARE + y5_DISTRST + y5_INEFCT + y5_SELWT + y2_STRESS ~ y0_CATINC + y0_QI

    y0_QI ~ y0_pbmi_fm2

    #################
    ## Covariances ##
    #################    

    y5_AWARE ~~ y5_DISTRST + y5_INEFCT + y5_SELWT + y0_pbmi_fm2 + y0_RACE

    y5_DISTRST ~~ y5_INEFCT + y5_SELWT + y0_pbmi_fm2 + y0_RACE

    y5_INEFCT ~~ y5_SELWT + y0_pbmi_fm2 + y0_RACE

    y5_SELWT + y2_STRESS ~~ y0_pbmi_fm2 + y0_RACE

    y0_QI + y0_pbmi_fm2 ~~ y0_RACE

    y0_QI + y0_pbmi_fm2 + y0_RACE ~~ y0_CATINC


  '

fit_psych_stress_initial <- 
  sem(mod_psych_stress_initial, data = semTrainingData)

summary(fit_psych_stress_initial, standardized = TRUE, fit.measures = TRUE)
```

    ## lavaan 0.6-3 ended normally after 152 iterations
    ## 
    ##   Optimization method                           NLMINB
    ##   Number of free parameters                         55
    ## 
    ##   Number of observations                           793
    ## 
    ##   Estimator                                         ML
    ##   Model Fit Test Statistic                       0.000
    ##   Degrees of freedom                                 0
    ## 
    ## Model test baseline model:
    ## 
    ##   Minimum Function Test Statistic             2032.772
    ##   Degrees of freedom                                45
    ##   P-value                                        0.000
    ## 
    ## User model versus baseline model:
    ## 
    ##   Comparative Fit Index (CFI)                    1.000
    ##   Tucker-Lewis Index (TLI)                       1.000
    ## 
    ## Loglikelihood and Information Criteria:
    ## 
    ##   Loglikelihood user model (H0)             -15447.286
    ##   Loglikelihood unrestricted model (H1)     -15447.286
    ## 
    ##   Number of free parameters                         55
    ##   Akaike (AIC)                               31004.571
    ##   Bayesian (BIC)                             31261.742
    ##   Sample-size adjusted Bayesian (BIC)        31087.087
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.000
    ##   90 Percent Confidence Interval          0.000  0.000
    ##   P-value RMSEA <= 0.05                             NA
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.000
    ## 
    ## Parameter Estimates:
    ## 
    ##   Information                                 Expected
    ##   Information saturated (h1) model          Structured
    ##   Standard Errors                             Standard
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   y10_QI ~                                                              
    ##     y0_CATINC        -0.384    0.130   -2.956    0.003   -0.384   -0.066
    ##     y0_QI             1.275    0.038   33.659    0.000    1.275    0.741
    ##     y0_pbmi_fm2       2.215    0.530    4.180    0.000    2.215    0.093
    ##     y0_RACE           0.575    0.314    1.833    0.067    0.575    0.046
    ##     y5_AWARE          0.053    0.037    1.440    0.150    0.053    0.037
    ##     y5_DISTRST        0.070    0.047    1.477    0.140    0.070    0.036
    ##     y5_INEFCT         0.100    0.050    2.009    0.045    0.100    0.058
    ##     y5_SELWT          0.277    0.261    1.060    0.289    0.277    0.027
    ##     y2_STRESS         0.009    0.020    0.449    0.653    0.009    0.010
    ##   y5_AWARE ~                                                            
    ##     y2_STRESS         0.069    0.023    2.983    0.003    0.069    0.103
    ##   y5_DISTRST ~                                                          
    ##     y2_STRESS         0.061    0.017    3.650    0.000    0.061    0.124
    ##   y5_INEFCT ~                                                           
    ##     y2_STRESS         0.119    0.019    6.328    0.000    0.119    0.217
    ##   y5_SELWT ~                                                            
    ##     y2_STRESS        -0.016    0.003   -4.903    0.000   -0.016   -0.171
    ##   y5_AWARE ~                                                            
    ##     y0_CATINC        -0.639    0.142   -4.511    0.000   -0.639   -0.156
    ##     y0_QI             0.091    0.043    2.093    0.036    0.091    0.075
    ##   y5_DISTRST ~                                                          
    ##     y0_CATINC        -0.762    0.102   -7.459    0.000   -0.762   -0.254
    ##     y0_QI             0.028    0.031    0.892    0.373    0.028    0.031
    ##   y5_INEFCT ~                                                           
    ##     y0_CATINC        -0.320    0.115   -2.781    0.005   -0.320   -0.095
    ##     y0_QI             0.079    0.035    2.245    0.025    0.079    0.080
    ##   y5_SELWT ~                                                            
    ##     y0_CATINC         0.012    0.020    0.590    0.556    0.012    0.021
    ##     y0_QI            -0.011    0.006   -1.832    0.067   -0.011   -0.066
    ##   y2_STRESS ~                                                           
    ##     y0_CATINC        -0.167    0.217   -0.770    0.441   -0.167   -0.027
    ##     y0_QI             0.021    0.067    0.320    0.749    0.021    0.012
    ##   y0_QI ~                                                               
    ##     y0_pbmi_fm2       3.807    0.472    8.067    0.000    3.807    0.275
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##  .y5_AWARE ~~                                                           
    ##    .y5_DISTRST        3.906    0.502    7.783    0.000    3.906    0.288
    ##    .y5_INEFCT         8.150    0.616   13.239    0.000    8.150    0.534
    ##    .y5_SELWT         -0.770    0.096   -7.995    0.000   -0.770   -0.296
    ##     y0_pbmi_fm2       0.167    0.043    3.914    0.000    0.167    0.145
    ##     y0_RACE           0.283    0.073    3.882    0.000    0.283    0.131
    ##  .y5_DISTRST ~~                                                         
    ##    .y5_INEFCT         3.716    0.413    8.993    0.000    3.716    0.337
    ##    .y5_SELWT         -0.457    0.069   -6.667    0.000   -0.457   -0.244
    ##     y0_pbmi_fm2       0.069    0.030    2.267    0.023    0.069    0.083
    ##     y0_RACE           0.384    0.054    7.127    0.000    0.384    0.245
    ##  .y5_INEFCT ~~                                                          
    ##    .y5_SELWT         -1.055    0.084  -12.585    0.000   -1.055   -0.500
    ##     y0_pbmi_fm2       0.078    0.034    2.280    0.023    0.078    0.084
    ##     y0_RACE          -0.105    0.059   -1.784    0.074   -0.105   -0.060
    ##  .y5_SELWT ~~                                                           
    ##     y0_pbmi_fm2      -0.010    0.006   -1.752    0.080   -0.010   -0.064
    ##     y0_RACE           0.053    0.010    5.234    0.000    0.053    0.177
    ##  .y2_STRESS ~~                                                          
    ##     y0_pbmi_fm2       0.093    0.065    1.440    0.150    0.093    0.053
    ##     y0_RACE          -0.071    0.110   -0.642    0.521   -0.071   -0.021
    ##  .y0_QI ~~                                                              
    ##     y0_RACE           0.232    0.061    3.774    0.000    0.232    0.132
    ##   y0_pbmi_fm2 ~~                                                        
    ##     y0_RACE           0.030    0.005    6.220    0.000    0.030    0.226
    ##  .y0_QI ~~                                                              
    ##     y0_CATINC         0.001    0.135    0.007    0.994    0.001    0.000
    ##   y0_CATINC ~~                                                          
    ##     y0_pbmi_fm2      -0.028    0.010   -2.680    0.007   -0.028   -0.096
    ##     y0_RACE          -0.177    0.020   -8.720    0.000   -0.177   -0.326
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .y10_QI           13.628    0.684   19.912    0.000   13.628    0.343
    ##    .y5_AWARE         18.802    0.946   19.879    0.000   18.802    0.951
    ##    .y5_DISTRST        9.781    0.491   19.901    0.000    9.781    0.915
    ##    .y5_INEFCT        12.410    0.624   19.901    0.000   12.410    0.931
    ##    .y5_SELWT          0.359    0.018   19.906    0.000    0.359    0.963
    ##    .y2_STRESS        43.993    2.210   19.908    0.000   43.993    0.999
    ##    .y0_QI            12.398    0.623   19.912    0.000   12.398    0.924
    ##     y0_CATINC         1.183    0.059   19.912    0.000    1.183    1.000
    ##     y0_pbmi_fm2       0.070    0.004   19.912    0.000    0.070    1.000
    ##     y0_RACE           0.250    0.013   19.912    0.000    0.250    1.000

Stress Psych Pruned Model
-------------------------

``` r
mod_pysch_stress_pruned <-
  '
    ######################
    ## Latent Variables ##
    ######################

    #################
    ## Regressions ##
    #################  
     
    y10_QI ~ y0_CATINC + y0_QI + y0_pbmi_fm2 + y0_RACE  

    y10_QI ~ y5_AWARE + y5_DISTRST + y5_INEFCT + y5_SELWT

    y5_AWARE + y5_DISTRST + y5_INEFCT + y5_SELWT ~ y2_STRESS

    y5_AWARE + y5_DISTRST + y5_INEFCT + y2_STRESS ~ y0_CATINC 

    y5_AWARE + y5_INEFCT + y5_SELWT ~ y0_QI
  
    y0_QI ~ y0_pbmi_fm2

    
    #################
    ## Covariances ##
    #################    

    y5_AWARE ~~ y5_DISTRST + y5_INEFCT + y5_SELWT + y0_pbmi_fm2 + y0_RACE

    y5_DISTRST ~~ y5_INEFCT + y5_SELWT + y0_pbmi_fm2 + y0_RACE

    y5_INEFCT ~~ y5_SELWT + y0_pbmi_fm2 + y0_RACE

    y5_SELWT ~~ y0_pbmi_fm2 + y0_RACE

    y2_STRESS ~~ 0*y0_QI + y0_pbmi_fm2 + 0*y0_RACE  

    y0_QI + y0_pbmi_fm2 ~~ y0_RACE

    y0_pbmi_fm2 + y0_RACE ~~ y0_CATINC

    y0_CATINC ~~ 0*y0_QI
  
  '

fit_pysch_stress_pruned <- sem(mod_pysch_stress_pruned, data = semTestingData)
summary(fit_pysch_stress_pruned, standardized = TRUE, fit.measures = TRUE, rsq = T)
```

    ## lavaan 0.6-3 ended normally after 141 iterations
    ## 
    ##   Optimization method                           NLMINB
    ##   Number of free parameters                         49
    ## 
    ##   Number of observations                           793
    ## 
    ##   Estimator                                         ML
    ##   Model Fit Test Statistic                       3.553
    ##   Degrees of freedom                                 6
    ##   P-value (Chi-square)                           0.737
    ## 
    ## Model test baseline model:
    ## 
    ##   Minimum Function Test Statistic             2124.792
    ##   Degrees of freedom                                45
    ##   P-value                                        0.000
    ## 
    ## User model versus baseline model:
    ## 
    ##   Comparative Fit Index (CFI)                    1.000
    ##   Tucker-Lewis Index (TLI)                       1.009
    ## 
    ## Loglikelihood and Information Criteria:
    ## 
    ##   Loglikelihood user model (H0)             -15712.861
    ##   Loglikelihood unrestricted model (H1)     -15711.084
    ## 
    ##   Number of free parameters                         49
    ##   Akaike (AIC)                               31523.721
    ##   Bayesian (BIC)                             31752.837
    ##   Sample-size adjusted Bayesian (BIC)        31597.235
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.000
    ##   90 Percent Confidence Interval          0.000  0.033
    ##   P-value RMSEA <= 0.05                          0.995
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.010
    ## 
    ## Parameter Estimates:
    ## 
    ##   Information                                 Expected
    ##   Information saturated (h1) model          Structured
    ##   Standard Errors                             Standard
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   y10_QI ~                                                              
    ##     y0_CATINC        -0.322    0.137   -2.345    0.019   -0.322   -0.051
    ##     y0_QI             1.312    0.038   34.307    0.000    1.312    0.753
    ##     y0_pbmi_fm2       2.659    0.585    4.545    0.000    2.659    0.101
    ##     y0_RACE           0.217    0.319    0.681    0.496    0.217    0.016
    ##     y5_AWARE          0.046    0.037    1.251    0.211    0.046    0.031
    ##     y5_DISTRST        0.148    0.050    2.957    0.003    0.148    0.072
    ##     y5_INEFCT        -0.049    0.055   -0.891    0.373   -0.049   -0.027
    ##     y5_SELWT         -0.442    0.272   -1.627    0.104   -0.442   -0.042
    ##   y5_AWARE ~                                                            
    ##     y2_STRESS         0.110    0.023    4.767    0.000    0.110    0.163
    ##   y5_DISTRST ~                                                          
    ##     y2_STRESS         0.063    0.016    3.851    0.000    0.063    0.131
    ##   y5_INEFCT ~                                                           
    ##     y2_STRESS         0.137    0.019    7.307    0.000    0.137    0.249
    ##   y5_SELWT ~                                                            
    ##     y2_STRESS        -0.024    0.003   -7.588    0.000   -0.024   -0.257
    ##   y5_AWARE ~                                                            
    ##     y0_CATINC        -0.659    0.141   -4.668    0.000   -0.659   -0.155
    ##   y5_DISTRST ~                                                          
    ##     y0_CATINC        -0.518    0.103   -5.039    0.000   -0.518   -0.170
    ##   y5_INEFCT ~                                                           
    ##     y0_CATINC        -0.383    0.099   -3.864    0.000   -0.383   -0.110
    ##   y2_STRESS ~                                                           
    ##     y0_CATINC        -0.814    0.222   -3.671    0.000   -0.814   -0.129
    ##   y5_AWARE ~                                                            
    ##     y0_QI             0.129    0.041    3.158    0.002    0.129    0.110
    ##   y5_INEFCT ~                                                           
    ##     y0_QI             0.078    0.031    2.506    0.012    0.078    0.080
    ##   y5_SELWT ~                                                            
    ##     y0_QI            -0.013    0.006   -2.279    0.023   -0.013   -0.079
    ##   y0_QI ~                                                               
    ##     y0_pbmi_fm2       4.794    0.507    9.455    0.000    4.794    0.318
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##  .y5_AWARE ~~                                                           
    ##    .y5_DISTRST        3.807    0.537    7.092    0.000    3.807    0.260
    ##    .y5_INEFCT         8.278    0.649   12.749    0.000    8.278    0.508
    ##    .y5_SELWT         -0.736    0.103   -7.152    0.000   -0.736   -0.263
    ##     y0_pbmi_fm2       0.051    0.043    1.179    0.239    0.051    0.043
    ##     y0_RACE           0.216    0.076    2.827    0.005    0.216    0.096
    ##  .y5_DISTRST ~~                                                         
    ##    .y5_INEFCT         5.126    0.459   11.170    0.000    5.126    0.432
    ##    .y5_SELWT         -0.522    0.075   -6.977    0.000   -0.522   -0.256
    ##     y0_pbmi_fm2       0.094    0.030    3.111    0.002    0.094    0.110
    ##     y0_RACE           0.353    0.057    6.241    0.000    0.353    0.217
    ##  .y5_INEFCT ~~                                                          
    ##    .y5_SELWT         -1.247    0.092  -13.545    0.000   -1.247   -0.549
    ##     y0_pbmi_fm2       0.081    0.035    2.303    0.021    0.081    0.084
    ##     y0_RACE          -0.005    0.062   -0.080    0.937   -0.005   -0.003
    ##  .y5_SELWT ~~                                                           
    ##     y0_pbmi_fm2      -0.010    0.006   -1.583    0.113   -0.010   -0.058
    ##     y0_RACE           0.034    0.011    3.172    0.002    0.034    0.108
    ##  .y2_STRESS ~~                                                          
    ##    .y0_QI             0.000                               0.000    0.000
    ##     y0_pbmi_fm2       0.176    0.062    2.818    0.005    0.176    0.098
    ##     y0_RACE           0.000                               0.000    0.000
    ##  .y0_QI ~~                                                              
    ##     y0_RACE           0.154    0.061    2.510    0.012    0.154    0.082
    ##   y0_pbmi_fm2 ~~                                                        
    ##     y0_RACE           0.029    0.005    6.141    0.000    0.029    0.222
    ##   y0_CATINC ~~                                                          
    ##     y0_pbmi_fm2      -0.039    0.010   -3.765    0.000   -0.039   -0.135
    ##     y0_RACE          -0.160    0.020   -7.979    0.000   -0.160   -0.293
    ##  .y0_QI ~~                                                              
    ##     y0_CATINC         0.000                               0.000    0.000
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .y10_QI           15.932    0.800   19.912    0.000   15.932    0.335
    ##    .y5_AWARE         20.090    1.009   19.908    0.000   20.090    0.925
    ##    .y5_DISTRST       10.638    0.534   19.912    0.000   10.638    0.948
    ##    .y5_INEFCT        13.215    0.664   19.899    0.000   13.215    0.906
    ##    .y5_SELWT          0.391    0.020   19.905    0.000    0.391    0.923
    ##    .y2_STRESS        46.893    2.355   19.912    0.000   46.893    0.983
    ##    .y0_QI            14.088    0.707   19.912    0.000   14.088    0.899
    ##     y0_CATINC         1.204    0.060   19.912    0.000    1.204    1.000
    ##     y0_pbmi_fm2       0.069    0.003   19.920    0.000    0.069    1.000
    ##     y0_RACE           0.250    0.013   19.946    0.000    0.250    1.000
    ## 
    ## R-Square:
    ##                    Estimate
    ##     y10_QI            0.665
    ##     y5_AWARE          0.075
    ##     y5_DISTRST        0.052
    ##     y5_INEFCT         0.094
    ##     y5_SELWT          0.077
    ##     y2_STRESS         0.017
    ##     y0_QI             0.101

Anxiety Psych Initial Model
---------------------------

``` r
mod_psych_anx_initial <-
  '
    #################
    ## Regressions ##
    #################  
     
    y10_QI ~ y0_CATINC + y0_QI + y0_pbmi_fm2 + y0_RACE  

    y10_QI ~ y5_AWARE + y5_DISTRST + y5_INEFCT + y5_SELWT 

    y5_AWARE + y5_DISTRST + y5_INEFCT + y5_SELWT ~ y3_ANXR

    y5_AWARE + y5_DISTRST + y5_INEFCT + y5_SELWT + y3_ANXR ~ y0_CATINC + y0_QI

    y0_QI ~ y0_pbmi_fm2

    #################
    ## Covariances ##
    #################    

    y5_AWARE ~~ y5_DISTRST + y5_INEFCT + y5_SELWT + y0_pbmi_fm2 + y0_RACE

    y5_DISTRST ~~ y5_INEFCT + y5_SELWT + y0_pbmi_fm2 + y0_RACE

    y5_INEFCT ~~ y5_SELWT + y0_pbmi_fm2 + y0_RACE

    y5_SELWT + y3_ANXR ~~ y0_pbmi_fm2 + y0_RACE

    y0_QI + y0_pbmi_fm2 ~~ y0_RACE

    y0_QI + y0_pbmi_fm2 + y0_RACE ~~ y0_CATINC


  '

fit_psych_anx_initial <- 
  sem(mod_psych_anx_initial, data = semTrainingData)

summary(fit_psych_anx_initial, standardized = TRUE, fit.measures = TRUE)
```

    ## lavaan 0.6-3 ended normally after 146 iterations
    ## 
    ##   Optimization method                           NLMINB
    ##   Number of free parameters                         54
    ## 
    ##   Number of observations                           793
    ## 
    ##   Estimator                                         ML
    ##   Model Fit Test Statistic                       0.496
    ##   Degrees of freedom                                 1
    ##   P-value (Chi-square)                           0.481
    ## 
    ## Model test baseline model:
    ## 
    ##   Minimum Function Test Statistic             2207.264
    ##   Degrees of freedom                                45
    ##   P-value                                        0.000
    ## 
    ## User model versus baseline model:
    ## 
    ##   Comparative Fit Index (CFI)                    1.000
    ##   Tucker-Lewis Index (TLI)                       1.010
    ## 
    ## Loglikelihood and Information Criteria:
    ## 
    ##   Loglikelihood user model (H0)             -15326.611
    ##   Loglikelihood unrestricted model (H1)     -15326.363
    ## 
    ##   Number of free parameters                         54
    ##   Akaike (AIC)                               30761.223
    ##   Bayesian (BIC)                             31013.717
    ##   Sample-size adjusted Bayesian (BIC)        30842.239
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.000
    ##   90 Percent Confidence Interval          0.000  0.083
    ##   P-value RMSEA <= 0.05                          0.776
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.002
    ## 
    ## Parameter Estimates:
    ## 
    ##   Information                                 Expected
    ##   Information saturated (h1) model          Structured
    ##   Standard Errors                             Standard
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   y10_QI ~                                                              
    ##     y0_CATINC        -0.384    0.130   -2.955    0.003   -0.384   -0.066
    ##     y0_QI             1.275    0.038   33.653    0.000    1.275    0.741
    ##     y0_pbmi_fm2       2.223    0.530    4.197    0.000    2.223    0.093
    ##     y0_RACE           0.573    0.314    1.826    0.068    0.573    0.045
    ##     y5_AWARE          0.053    0.037    1.428    0.153    0.053    0.037
    ##     y5_DISTRST        0.071    0.047    1.503    0.133    0.071    0.037
    ##     y5_INEFCT         0.103    0.049    2.086    0.037    0.103    0.060
    ##     y5_SELWT          0.270    0.261    1.035    0.301    0.270    0.026
    ##   y5_AWARE ~                                                            
    ##     y3_ANXR           0.246    0.023   10.739    0.000    0.246    0.352
    ##   y5_DISTRST ~                                                          
    ##     y3_ANXR           0.114    0.017    6.629    0.000    0.114    0.222
    ##   y5_INEFCT ~                                                           
    ##     y3_ANXR           0.212    0.019   11.260    0.000    0.212    0.369
    ##   y5_SELWT ~                                                            
    ##     y3_ANXR          -0.039    0.003  -12.335    0.000   -0.039   -0.402
    ##   y5_AWARE ~                                                            
    ##     y0_CATINC        -0.491    0.134   -3.672    0.000   -0.491   -0.120
    ##     y0_QI             0.104    0.041    2.557    0.011    0.104    0.086
    ##   y5_DISTRST ~                                                          
    ##     y0_CATINC        -0.698    0.101   -6.924    0.000   -0.698   -0.232
    ##     y0_QI             0.035    0.031    1.134    0.257    0.035    0.039
    ##   y5_INEFCT ~                                                           
    ##     y0_CATINC        -0.203    0.110   -1.841    0.066   -0.203   -0.060
    ##     y0_QI             0.093    0.034    2.743    0.006    0.093    0.093
    ##   y5_SELWT ~                                                            
    ##     y0_CATINC        -0.011    0.018   -0.591    0.555   -0.011   -0.019
    ##     y0_QI            -0.013    0.006   -2.370    0.018   -0.013   -0.080
    ##   y3_ANXR ~                                                             
    ##     y0_CATINC        -0.647    0.207   -3.132    0.002   -0.647   -0.111
    ##     y0_QI            -0.050    0.063   -0.783    0.434   -0.050   -0.029
    ##   y0_QI ~                                                               
    ##     y0_pbmi_fm2       3.807    0.472    8.067    0.000    3.807    0.275
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##  .y5_AWARE ~~                                                           
    ##    .y5_DISTRST        2.967    0.457    6.497    0.000    2.967    0.237
    ##    .y5_INEFCT         6.428    0.536   11.985    0.000    6.428    0.471
    ##    .y5_SELWT         -0.438    0.082   -5.340    0.000   -0.438   -0.193
    ##     y0_pbmi_fm2       0.125    0.040    3.146    0.002    0.125    0.115
    ##     y0_RACE           0.268    0.069    3.917    0.000    0.268    0.132
    ##  .y5_DISTRST ~~                                                         
    ##    .y5_INEFCT         3.067    0.381    8.040    0.000    3.067    0.298
    ##    .y5_SELWT         -0.323    0.062   -5.220    0.000   -0.323   -0.189
    ##     y0_pbmi_fm2       0.052    0.030    1.761    0.078    0.052    0.064
    ##     y0_RACE           0.375    0.053    7.095    0.000    0.375    0.244
    ##  .y5_INEFCT ~~                                                          
    ##    .y5_SELWT         -0.810    0.072  -11.208    0.000   -0.810   -0.434
    ##     y0_pbmi_fm2       0.048    0.032    1.471    0.141    0.048    0.054
    ##     y0_RACE          -0.122    0.056   -2.174    0.030   -0.122   -0.073
    ##  .y5_SELWT ~~                                                           
    ##     y0_pbmi_fm2      -0.004    0.005   -0.760    0.447   -0.004   -0.028
    ##     y0_RACE           0.056    0.009    5.881    0.000    0.056    0.200
    ##  .y3_ANXR ~~                                                            
    ##     y0_pbmi_fm2       0.197    0.062    3.175    0.001    0.197    0.117
    ##     y0_RACE           0.039    0.105    0.375    0.708    0.039    0.012
    ##  .y0_QI ~~                                                              
    ##     y0_RACE           0.232    0.061    3.774    0.000    0.232    0.132
    ##   y0_pbmi_fm2 ~~                                                        
    ##     y0_RACE           0.030    0.005    6.220    0.000    0.030    0.226
    ##  .y0_QI ~~                                                              
    ##     y0_CATINC         0.001    0.135    0.007    0.994    0.001    0.000
    ##   y0_CATINC ~~                                                          
    ##     y0_pbmi_fm2      -0.028    0.010   -2.680    0.007   -0.028   -0.096
    ##     y0_RACE          -0.177    0.020   -8.720    0.000   -0.177   -0.326
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .y10_QI           13.631    0.685   19.912    0.000   13.631    0.343
    ##    .y5_AWARE         16.595    0.834   19.891    0.000   16.595    0.840
    ##    .y5_DISTRST        9.423    0.473   19.906    0.000    9.423    0.882
    ##    .y5_INEFCT        11.238    0.565   19.908    0.000   11.238    0.843
    ##    .y5_SELWT          0.310    0.016   19.911    0.000    0.310    0.832
    ##    .y3_ANXR          40.014    2.012   19.890    0.000   40.014    0.989
    ##    .y0_QI            12.398    0.623   19.912    0.000   12.398    0.924
    ##     y0_CATINC         1.183    0.059   19.912    0.000    1.183    1.000
    ##     y0_pbmi_fm2       0.070    0.004   19.912    0.000    0.070    1.000
    ##     y0_RACE           0.250    0.013   19.912    0.000    0.250    1.000

Anxiety Psych Pruned Model
--------------------------

``` r
mod_psych_anx_pruned <-
  '
    #################
    ## Regressions ##
    #################  
     
    y10_QI ~ y0_CATINC + y0_QI + y0_pbmi_fm2 + y0_RACE  

    y10_QI ~ y5_AWARE + y5_DISTRST + y5_INEFCT + y5_SELWT 

    y5_AWARE + y5_DISTRST + y5_INEFCT + y5_SELWT ~ y3_ANXR

    y5_AWARE + y5_DISTRST + y5_INEFCT + y3_ANXR ~ y0_CATINC

    y5_AWARE + y5_INEFCT + y5_SELWT ~ y0_QI

    y0_QI ~ y0_pbmi_fm2

    
    #################
    ## Covariances ##
    #################    

    y5_AWARE ~~ y5_DISTRST + y5_INEFCT + y5_SELWT + y0_pbmi_fm2 + y0_RACE

    y5_DISTRST ~~ y5_INEFCT + y5_SELWT + y0_pbmi_fm2 + y0_RACE

    y5_INEFCT ~~ y5_SELWT + y0_pbmi_fm2 + 0*y0_RACE

    y5_SELWT ~~ 0*y0_pbmi_fm2 + y0_RACE

    y3_ANXR ~~ 0*y0_QI + y0_pbmi_fm2 + 0*y0_RACE  

    y0_QI + y0_pbmi_fm2 ~~ y0_RACE

    y0_pbmi_fm2 + y0_RACE ~~ y0_CATINC

    y0_CATINC ~~ 0*y0_QI

  '

fit_psych_anx_pruned <- sem(mod_psych_anx_pruned, data = semTestingData)
summary(fit_psych_anx_pruned, standardized = TRUE, fit.measures = TRUE, rsq = T)
```

    ## lavaan 0.6-3 ended normally after 133 iterations
    ## 
    ##   Optimization method                           NLMINB
    ##   Number of free parameters                         47
    ## 
    ##   Number of observations                           793
    ## 
    ##   Estimator                                         ML
    ##   Model Fit Test Statistic                       4.203
    ##   Degrees of freedom                                 8
    ##   P-value (Chi-square)                           0.838
    ## 
    ## Model test baseline model:
    ## 
    ##   Minimum Function Test Statistic             2267.046
    ##   Degrees of freedom                                45
    ##   P-value                                        0.000
    ## 
    ## User model versus baseline model:
    ## 
    ##   Comparative Fit Index (CFI)                    1.000
    ##   Tucker-Lewis Index (TLI)                       1.010
    ## 
    ## Loglikelihood and Information Criteria:
    ## 
    ##   Loglikelihood user model (H0)             -15590.810
    ##   Loglikelihood unrestricted model (H1)     -15588.708
    ## 
    ##   Number of free parameters                         47
    ##   Akaike (AIC)                               31275.619
    ##   Bayesian (BIC)                             31495.383
    ##   Sample-size adjusted Bayesian (BIC)        31346.133
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.000
    ##   90 Percent Confidence Interval          0.000  0.024
    ##   P-value RMSEA <= 0.05                          0.999
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.012
    ## 
    ## Parameter Estimates:
    ## 
    ##   Information                                 Expected
    ##   Information saturated (h1) model          Structured
    ##   Standard Errors                             Standard
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   y10_QI ~                                                              
    ##     y0_CATINC        -0.322    0.137   -2.344    0.019   -0.322   -0.051
    ##     y0_QI             1.312    0.038   34.341    0.000    1.312    0.754
    ##     y0_pbmi_fm2       2.659    0.584    4.552    0.000    2.659    0.101
    ##     y0_RACE           0.217    0.319    0.681    0.496    0.217    0.016
    ##     y5_AWARE          0.046    0.037    1.251    0.211    0.046    0.031
    ##     y5_DISTRST        0.148    0.050    2.957    0.003    0.148    0.072
    ##     y5_INEFCT        -0.049    0.055   -0.891    0.373   -0.049   -0.027
    ##     y5_SELWT         -0.442    0.272   -1.629    0.103   -0.442   -0.042
    ##   y5_AWARE ~                                                            
    ##     y3_ANXR           0.253    0.023   10.837    0.000    0.253    0.351
    ##   y5_DISTRST ~                                                          
    ##     y3_ANXR           0.122    0.017    7.120    0.000    0.122    0.236
    ##   y5_INEFCT ~                                                           
    ##     y3_ANXR           0.210    0.019   10.846    0.000    0.210    0.356
    ##   y5_SELWT ~                                                            
    ##     y3_ANXR          -0.041    0.003  -12.626    0.000   -0.041   -0.405
    ##   y5_AWARE ~                                                            
    ##     y0_CATINC        -0.587    0.137   -4.301    0.000   -0.587   -0.138
    ##   y5_DISTRST ~                                                          
    ##     y0_CATINC        -0.495    0.102   -4.854    0.000   -0.495   -0.162
    ##   y5_INEFCT ~                                                           
    ##     y0_CATINC        -0.376    0.099   -3.811    0.000   -0.376   -0.108
    ##   y3_ANXR ~                                                             
    ##     y0_CATINC        -0.710    0.208   -3.414    0.001   -0.710   -0.120
    ##   y5_AWARE ~                                                            
    ##     y0_QI             0.117    0.039    2.993    0.003    0.117    0.100
    ##   y5_INEFCT ~                                                           
    ##     y0_QI             0.073    0.030    2.449    0.014    0.073    0.076
    ##   y5_SELWT ~                                                            
    ##     y0_QI            -0.013    0.005   -2.431    0.015   -0.013   -0.077
    ##   y0_QI ~                                                               
    ##     y0_pbmi_fm2       4.793    0.507    9.452    0.000    4.793    0.318
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##  .y5_AWARE ~~                                                           
    ##    .y5_DISTRST        2.882    0.488    5.912    0.000    2.882    0.212
    ##    .y5_INEFCT         6.807    0.578   11.776    0.000    6.807    0.458
    ##    .y5_SELWT         -0.434    0.090   -4.796    0.000   -0.434   -0.173
    ##     y0_pbmi_fm2       0.015    0.040    0.367    0.714    0.015    0.013
    ##     y0_RACE           0.227    0.065    3.521    0.000    0.227    0.107
    ##  .y5_DISTRST ~~                                                         
    ##    .y5_INEFCT         4.471    0.417   10.709    0.000    4.471    0.400
    ##    .y5_SELWT         -0.382    0.068   -5.619    0.000   -0.382   -0.202
    ##     y0_pbmi_fm2       0.074    0.029    2.552    0.011    0.074    0.088
    ##     y0_RACE           0.357    0.051    6.974    0.000    0.357    0.223
    ##  .y5_INEFCT ~~                                                          
    ##    .y5_SELWT         -1.045    0.082  -12.792    0.000   -1.045   -0.505
    ##     y0_pbmi_fm2       0.046    0.029    1.624    0.104    0.046    0.050
    ##     y0_RACE           0.000                               0.000    0.000
    ##  .y5_SELWT ~~                                                           
    ##     y0_pbmi_fm2       0.000                               0.000    0.000
    ##     y0_RACE           0.035    0.009    4.066    0.000    0.035    0.118
    ##  .y3_ANXR ~~                                                            
    ##    .y0_QI             0.000                               0.000    0.000
    ##     y0_pbmi_fm2       0.214    0.059    3.643    0.000    0.214    0.127
    ##     y0_RACE           0.000                               0.000    0.000
    ##  .y0_QI ~~                                                              
    ##     y0_RACE           0.155    0.061    2.537    0.011    0.155    0.083
    ##   y0_pbmi_fm2 ~~                                                        
    ##     y0_RACE           0.030    0.005    6.289    0.000    0.030    0.226
    ##   y0_CATINC ~~                                                          
    ##     y0_pbmi_fm2      -0.039    0.010   -3.782    0.000   -0.039   -0.136
    ##     y0_RACE          -0.160    0.020   -7.941    0.000   -0.160   -0.291
    ##  .y0_QI ~~                                                              
    ##     y0_CATINC         0.000                               0.000    0.000
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .y10_QI           15.932    0.800   19.912    0.000   15.932    0.335
    ##    .y5_AWARE         18.050    0.904   19.965    0.000   18.050    0.831
    ##    .y5_DISTRST       10.213    0.508   20.090    0.000   10.213    0.909
    ##    .y5_INEFCT        12.262    0.616   19.922    0.000   12.262    0.841
    ##    .y5_SELWT          0.349    0.017   19.991    0.000    0.349    0.827
    ##    .y3_ANXR          41.301    2.074   19.912    0.000   41.301    0.986
    ##    .y0_QI            14.088    0.707   19.912    0.000   14.088    0.899
    ##     y0_CATINC         1.204    0.060   19.912    0.000    1.204    1.000
    ##     y0_pbmi_fm2       0.069    0.003   19.926    0.000    0.069    1.000
    ##     y0_RACE           0.250    0.013   19.956    0.000    0.250    1.000
    ## 
    ## R-Square:
    ##                    Estimate
    ##     y10_QI            0.665
    ##     y5_AWARE          0.169
    ##     y5_DISTRST        0.091
    ##     y5_INEFCT         0.159
    ##     y5_SELWT          0.173
    ##     y3_ANXR           0.014
    ##     y0_QI             0.101

Stress Behavior Initial Model
-----------------------------

``` r
mod_behav_stress_initial <-
  '
    ######################
    ## Latent Variables ##
    ######################

    #################
    ## Regressions ##
    #################  
     
    y10_QI ~ y0_CATINC + y0_QI + y0_pbmi_fm2 + y0_RACE    

    y10_QI ~ y5_AFTSNK + y5_ALONE + y6_SECRET + y5_FSTFOOD5 

    y5_AFTSNK + y5_ALONE + y6_SECRET + y5_FSTFOOD5 + y2_STRESS ~ y0_CATINC

    y5_AFTSNK + y5_ALONE + y6_SECRET + y5_FSTFOOD5 ~ y2_STRESS

    y0_QI ~ y0_pbmi_fm2

    #################
    ## Covariances ##
    #################  
  
    y5_AFTSNK ~~ y5_ALONE + y6_SECRET + y5_FSTFOOD5 + y0_QI + y0_RACE + y0_pbmi_fm2

    y5_ALONE ~~ y6_SECRET + y5_FSTFOOD5 + y0_QI + y0_RACE + y0_pbmi_fm2

    y6_SECRET ~~ y5_FSTFOOD5 + y0_QI + y0_RACE + y0_pbmi_fm2

    y5_FSTFOOD5 ~~ y0_QI + y0_RACE + y0_pbmi_fm2

    y0_QI ~~ y0_RACE + y2_STRESS + y0_CATINC

    y0_RACE ~~ y0_pbmi_fm2 + y2_STRESS + y0_CATINC

    y0_pbmi_fm2 ~~ y2_STRESS + y0_CATINC
    
  '

fit_behav_stress_initial <- sem(mod_behav_stress_initial, data = semTrainingData)
summary(fit_behav_stress_initial, standardized = TRUE, fit.measures = TRUE)
```

    ## lavaan 0.6-3 ended normally after 171 iterations
    ## 
    ##   Optimization method                           NLMINB
    ##   Number of free parameters                         54
    ## 
    ##   Number of observations                           793
    ## 
    ##   Estimator                                         ML
    ##   Model Fit Test Statistic                       2.150
    ##   Degrees of freedom                                 1
    ##   P-value (Chi-square)                           0.143
    ## 
    ## Model test baseline model:
    ## 
    ##   Minimum Function Test Statistic             1181.531
    ##   Degrees of freedom                                45
    ##   P-value                                        0.000
    ## 
    ## User model versus baseline model:
    ## 
    ##   Comparative Fit Index (CFI)                    0.999
    ##   Tucker-Lewis Index (TLI)                       0.954
    ## 
    ## Loglikelihood and Information Criteria:
    ## 
    ##   Loglikelihood user model (H0)             -10539.358
    ##   Loglikelihood unrestricted model (H1)     -10538.283
    ## 
    ##   Number of free parameters                         54
    ##   Akaike (AIC)                               21186.716
    ##   Bayesian (BIC)                             21439.210
    ##   Sample-size adjusted Bayesian (BIC)        21267.731
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.038
    ##   90 Percent Confidence Interval          0.000  0.110
    ##   P-value RMSEA <= 0.05                          0.479
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.004
    ## 
    ## Parameter Estimates:
    ## 
    ##   Information                                 Expected
    ##   Information saturated (h1) model          Structured
    ##   Standard Errors                             Standard
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   y10_QI ~                                                              
    ##     y0_CATINC        -0.426    0.130   -3.283    0.001   -0.426   -0.074
    ##     y0_QI             1.279    0.038   33.585    0.000    1.279    0.744
    ##     y0_pbmi_fm2       2.432    0.528    4.605    0.000    2.432    0.102
    ##     y0_RACE           0.829    0.294    2.822    0.005    0.829    0.066
    ##     y5_AFTSNK         1.116    0.459    2.432    0.015    1.116    0.051
    ##     y5_ALONE         -0.300    0.260   -1.151    0.250   -0.300   -0.025
    ##     y6_SECRET         0.901    0.362    2.488    0.013    0.901    0.053
    ##     y5_FSTFOOD5      -0.075    0.194   -0.385    0.700   -0.075   -0.008
    ##   y5_AFTSNK ~                                                           
    ##     y0_CATINC        -0.012    0.009   -1.277    0.201   -0.012   -0.045
    ##   y5_ALONE ~                                                            
    ##     y0_CATINC        -0.001    0.017   -0.082    0.935   -0.001   -0.003
    ##   y6_SECRET ~                                                           
    ##     y0_CATINC        -0.035    0.012   -2.899    0.004   -0.035   -0.102
    ##   y5_FSTFOOD5 ~                                                         
    ##     y0_CATINC        -0.112    0.023   -4.923    0.000   -0.112   -0.172
    ##   y2_STRESS ~                                                           
    ##     y0_CATINC        -0.169    0.217   -0.779    0.436   -0.169   -0.028
    ##   y5_AFTSNK ~                                                           
    ##     y2_STRESS        -0.001    0.002   -0.951    0.341   -0.001   -0.034
    ##   y5_ALONE ~                                                            
    ##     y2_STRESS         0.007    0.003    2.457    0.014    0.007    0.087
    ##   y6_SECRET ~                                                           
    ##     y2_STRESS        -0.000    0.002   -0.153    0.878   -0.000   -0.005
    ##   y5_FSTFOOD5 ~                                                         
    ##     y2_STRESS         0.003    0.004    0.808    0.419    0.003    0.028
    ##   y0_QI ~                                                               
    ##     y0_pbmi_fm2       3.807    0.472    8.067    0.000    3.807    0.275
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##  .y5_AFTSNK ~~                                                          
    ##    .y5_ALONE         -0.005    0.005   -0.858    0.391   -0.005   -0.030
    ##    .y6_SECRET        -0.003    0.004   -0.783    0.434   -0.003   -0.028
    ##    .y5_FSTFOOD5      -0.011    0.007   -1.595    0.111   -0.011   -0.057
    ##    .y0_QI             0.070    0.036    1.932    0.053    0.070    0.069
    ##     y0_RACE          -0.009    0.005   -1.914    0.056   -0.009   -0.064
    ##     y0_pbmi_fm2       0.000    0.003    0.071    0.943    0.000    0.003
    ##  .y5_ALONE ~~                                                           
    ##    .y6_SECRET        -0.003    0.007   -0.501    0.616   -0.003   -0.018
    ##    .y5_FSTFOOD5       0.048    0.013    3.772    0.000    0.048    0.135
    ##    .y0_QI             0.113    0.064    1.753    0.080    0.113    0.062
    ##     y0_RACE           0.037    0.009    4.252    0.000    0.037    0.144
    ##     y0_pbmi_fm2       0.011    0.005    2.254    0.024    0.011    0.080
    ##  .y6_SECRET ~~                                                          
    ##    .y5_FSTFOOD5      -0.013    0.009   -1.491    0.136   -0.013   -0.053
    ##    .y0_QI            -0.017    0.046   -0.382    0.702   -0.017   -0.014
    ##     y0_RACE           0.011    0.006    1.733    0.083    0.011    0.058
    ##     y0_pbmi_fm2       0.006    0.003    1.864    0.062    0.006    0.066
    ##  .y5_FSTFOOD5 ~~                                                        
    ##    .y0_QI            -0.181    0.087   -2.079    0.038   -0.181   -0.074
    ##     y0_RACE           0.050    0.012    4.219    0.000    0.050    0.143
    ##     y0_pbmi_fm2       0.012    0.007    1.870    0.062    0.012    0.066
    ##  .y0_QI ~~                                                              
    ##     y0_RACE           0.232    0.061    3.774    0.000    0.232    0.132
    ##  .y2_STRESS ~~                                                          
    ##    .y0_QI             0.265    0.828    0.320    0.749    0.265    0.011
    ##  .y0_QI ~~                                                              
    ##     y0_CATINC         0.001    0.135    0.007    0.994    0.001    0.000
    ##   y0_pbmi_fm2 ~~                                                        
    ##     y0_RACE           0.030    0.005    6.220    0.000    0.030    0.226
    ##  .y2_STRESS ~~                                                          
    ##     y0_RACE          -0.064    0.111   -0.574    0.566   -0.064   -0.019
    ##   y0_CATINC ~~                                                          
    ##     y0_RACE          -0.177    0.020   -8.720    0.000   -0.177   -0.326
    ##  .y2_STRESS ~~                                                          
    ##     y0_pbmi_fm2       0.099    0.062    1.587    0.112    0.099    0.056
    ##   y0_CATINC ~~                                                          
    ##     y0_pbmi_fm2      -0.028    0.010   -2.680    0.007   -0.028   -0.096
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .y10_QI           13.720    0.689   19.912    0.000   13.720    0.346
    ##    .y5_AFTSNK         0.083    0.004   19.912    0.000    0.083    0.997
    ##    .y5_ALONE          0.265    0.013   19.912    0.000    0.265    0.992
    ##    .y6_SECRET         0.134    0.007   19.912    0.000    0.134    0.990
    ##    .y5_FSTFOOD5       0.485    0.024   19.912    0.000    0.485    0.969
    ##    .y2_STRESS        44.014    2.210   19.912    0.000   44.014    0.999
    ##    .y0_QI            12.398    0.623   19.912    0.000   12.398    0.924
    ##     y0_CATINC         1.183    0.059   19.912    0.000    1.183    1.000
    ##     y0_pbmi_fm2       0.070    0.004   19.912    0.000    0.070    1.000
    ##     y0_RACE           0.250    0.013   19.912    0.000    0.250    1.000

Stress Behavior Pruned Model
----------------------------

``` r
mod_behav_stress_pruned <-
  '
    #################
    ## Regressions ##
    #################  
     
    y10_QI ~ y0_CATINC + y0_QI + y0_pbmi_fm2 + y0_RACE    

    y10_QI ~ y5_AFTSNK + y5_ALONE + y6_SECRET

    y5_AFTSNK + y6_SECRET + y2_STRESS ~ y0_CATINC

    y5_ALONE + y6_SECRET ~ y2_STRESS

    y0_QI ~ y0_pbmi_fm2

    #################
    ## Covariances ##
    #################  
  
    y5_AFTSNK ~~ y5_ALONE + 0*y6_SECRET + y0_QI + y0_RACE + 0*y0_pbmi_fm2

    y5_ALONE ~~ 0*y6_SECRET + y0_QI + y0_RACE + y0_pbmi_fm2

    y6_SECRET ~~ 0*y0_QI + 0*y0_RACE + 0*y0_pbmi_fm2

    y0_QI ~~ y0_RACE + 0*y2_STRESS + 0*y0_CATINC

    y0_RACE ~~ y0_pbmi_fm2 + 0*y2_STRESS + y0_CATINC

    y0_pbmi_fm2 ~~ y2_STRESS + y0_CATINC
    
  '

fit_behav_stress_pruned <- 
  sem(mod_behav_stress_pruned, data = semTestingData)

summary(fit_behav_stress_pruned, standardized = TRUE, fit.measures = TRUE)
```

    ## lavaan 0.6-3 ended normally after 115 iterations
    ## 
    ##   Optimization method                           NLMINB
    ##   Number of free parameters                         33
    ## 
    ##   Number of observations                           793
    ## 
    ##   Estimator                                         ML
    ##   Model Fit Test Statistic                      20.208
    ##   Degrees of freedom                                12
    ##   P-value (Chi-square)                           0.063
    ## 
    ## Model test baseline model:
    ## 
    ##   Minimum Function Test Statistic             1160.740
    ##   Degrees of freedom                                36
    ##   P-value                                        0.000
    ## 
    ## User model versus baseline model:
    ## 
    ##   Comparative Fit Index (CFI)                    0.993
    ##   Tucker-Lewis Index (TLI)                       0.978
    ## 
    ## Loglikelihood and Information Criteria:
    ## 
    ##   Loglikelihood user model (H0)              -9904.346
    ##   Loglikelihood unrestricted model (H1)      -9894.242
    ## 
    ##   Number of free parameters                         33
    ##   Akaike (AIC)                               19874.692
    ##   Bayesian (BIC)                             20028.994
    ##   Sample-size adjusted Bayesian (BIC)        19924.201
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.029
    ##   90 Percent Confidence Interval          0.000  0.051
    ##   P-value RMSEA <= 0.05                          0.941
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.025
    ## 
    ## Parameter Estimates:
    ## 
    ##   Information                                 Expected
    ##   Information saturated (h1) model          Structured
    ##   Standard Errors                             Standard
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   y10_QI ~                                                              
    ##     y0_CATINC        -0.371    0.135   -2.754    0.006   -0.371   -0.059
    ##     y0_QI             1.315    0.038   34.584    0.000    1.315    0.755
    ##     y0_pbmi_fm2       2.625    0.578    4.540    0.000    2.625    0.100
    ##     y0_RACE           0.512    0.305    1.676    0.094    0.512    0.037
    ##     y5_AFTSNK         2.214    0.483    4.582    0.000    2.214    0.095
    ##     y5_ALONE         -0.197    0.267   -0.736    0.462   -0.197   -0.015
    ##     y6_SECRET         0.644    0.390    1.649    0.099    0.644    0.034
    ##   y5_AFTSNK ~                                                           
    ##     y0_CATINC        -0.012    0.009   -1.282    0.200   -0.012   -0.045
    ##   y6_SECRET ~                                                           
    ##     y0_CATINC         0.000    0.012    0.004    0.997    0.000    0.000
    ##   y2_STRESS ~                                                           
    ##     y0_CATINC        -0.814    0.222   -3.671    0.000   -0.814   -0.129
    ##   y5_ALONE ~                                                            
    ##     y2_STRESS         0.004    0.003    1.343    0.179    0.004    0.047
    ##   y6_SECRET ~                                                           
    ##     y2_STRESS         0.005    0.002    2.865    0.004    0.005    0.102
    ##   y0_QI ~                                                               
    ##     y0_pbmi_fm2       4.701    0.506    9.284    0.000    4.701    0.313
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##  .y5_AFTSNK ~~                                                          
    ##    .y5_ALONE          0.007    0.006    1.221    0.222    0.007    0.043
    ##    .y6_SECRET         0.000                               0.000    0.000
    ##    .y0_QI             0.068    0.039    1.741    0.082    0.068    0.062
    ##     y0_RACE          -0.009    0.005   -1.907    0.057   -0.009   -0.064
    ##     y0_pbmi_fm2       0.000                               0.000    0.000
    ##  .y6_SECRET ~~                                                          
    ##    .y5_ALONE          0.000                               0.000    0.000
    ##  .y5_ALONE ~~                                                           
    ##    .y0_QI             0.190    0.072    2.656    0.008    0.190    0.095
    ##     y0_RACE           0.033    0.009    3.623    0.000    0.033    0.125
    ##     y0_pbmi_fm2       0.008    0.005    1.564    0.118    0.008    0.055
    ##  .y6_SECRET ~~                                                          
    ##    .y0_QI             0.000                               0.000    0.000
    ##     y0_RACE           0.000                               0.000    0.000
    ##     y0_pbmi_fm2       0.000                               0.000    0.000
    ##  .y0_QI ~~                                                              
    ##     y0_RACE           0.163    0.063    2.604    0.009    0.163    0.087
    ##  .y2_STRESS ~~                                                          
    ##    .y0_QI             0.000                               0.000    0.000
    ##  .y0_QI ~~                                                              
    ##     y0_CATINC         0.000                               0.000    0.000
    ##   y0_pbmi_fm2 ~~                                                        
    ##     y0_RACE           0.030    0.005    6.235    0.000    0.030    0.225
    ##  .y2_STRESS ~~                                                          
    ##     y0_RACE           0.000                               0.000    0.000
    ##   y0_CATINC ~~                                                          
    ##     y0_RACE          -0.153    0.020   -7.677    0.000   -0.153   -0.280
    ##  .y2_STRESS ~~                                                          
    ##     y0_pbmi_fm2       0.176    0.062    2.829    0.005    0.176    0.098
    ##   y0_CATINC ~~                                                          
    ##     y0_pbmi_fm2      -0.038    0.010   -3.641    0.000   -0.038   -0.130
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .y10_QI           15.823    0.795   19.912    0.000   15.823    0.334
    ##    .y5_AFTSNK         0.086    0.004   19.912    0.000    0.086    0.998
    ##    .y6_SECRET         0.130    0.007   19.912    0.000    0.130    0.990
    ##    .y2_STRESS        46.893    2.355   19.912    0.000   46.893    0.983
    ##    .y5_ALONE          0.287    0.014   19.912    0.000    0.287    0.998
    ##    .y0_QI            14.088    0.708   19.912    0.000   14.088    0.902
    ##     y0_CATINC         1.204    0.060   19.912    0.000    1.204    1.000
    ##     y0_pbmi_fm2       0.069    0.003   19.921    0.000    0.069    1.000
    ##     y0_RACE           0.248    0.012   19.946    0.000    0.248    1.000

Anxiety Behavior Initial Model
------------------------------

``` r
mod_behav_anx_initial <-
  '
    #################
    ## Regressions ##
    #################  
     
    y10_QI ~ y0_CATINC + y0_QI + y0_pbmi_fm2 + y0_RACE    

    y10_QI ~ y5_AFTSNK + y5_ALONE + y7_SECRET + y5_FSTFOOD5 + y3_ANXR

    y5_AFTSNK + y5_ALONE + y7_SECRET + y5_FSTFOOD5 + y3_ANXR ~ y0_CATINC

    y5_AFTSNK + y5_ALONE + y7_SECRET + y5_FSTFOOD5 ~ y3_ANXR

    y0_QI ~ y0_pbmi_fm2

    #################
    ## Covariances ##
    #################  
  
    y5_AFTSNK ~~ y5_ALONE + y7_SECRET + y5_FSTFOOD5 + y0_QI + y0_RACE + y0_pbmi_fm2

    y5_ALONE ~~ y7_SECRET + y5_FSTFOOD5 + y0_QI + y0_RACE + y0_pbmi_fm2

    y7_SECRET ~~ y5_FSTFOOD5 + y0_QI + y0_RACE + y0_pbmi_fm2

    y5_FSTFOOD5 ~~ y0_QI + y0_RACE + y0_pbmi_fm2

    y0_QI ~~ y0_RACE + y3_ANXR + y0_CATINC

    y0_RACE ~~ y0_pbmi_fm2 + y3_ANXR + y0_CATINC

    y0_pbmi_fm2 ~~ y3_ANXR + y0_CATINC
    
  '

fit_behav_anx_initial <- 
  sem(mod_behav_anx_initial, data = semTrainingData)

summary(fit_behav_anx_initial, standardized = TRUE, fit.measures = TRUE)
```

    ## lavaan 0.6-3 ended normally after 166 iterations
    ## 
    ##   Optimization method                           NLMINB
    ##   Number of free parameters                         55
    ## 
    ##   Number of observations                           793
    ## 
    ##   Estimator                                         ML
    ##   Model Fit Test Statistic                       0.000
    ##   Degrees of freedom                                 0
    ##   Minimum Function Value               0.0000000000000
    ## 
    ## Model test baseline model:
    ## 
    ##   Minimum Function Test Statistic             1225.883
    ##   Degrees of freedom                                45
    ##   P-value                                        0.000
    ## 
    ## User model versus baseline model:
    ## 
    ##   Comparative Fit Index (CFI)                    1.000
    ##   Tucker-Lewis Index (TLI)                       1.000
    ## 
    ## Loglikelihood and Information Criteria:
    ## 
    ##   Loglikelihood user model (H0)             -10484.998
    ##   Loglikelihood unrestricted model (H1)     -10484.998
    ## 
    ##   Number of free parameters                         55
    ##   Akaike (AIC)                               21079.997
    ##   Bayesian (BIC)                             21337.167
    ##   Sample-size adjusted Bayesian (BIC)        21162.513
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.000
    ##   90 Percent Confidence Interval          0.000  0.000
    ##   P-value RMSEA <= 0.05                             NA
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.000
    ## 
    ## Parameter Estimates:
    ## 
    ##   Information                                 Expected
    ##   Information saturated (h1) model          Structured
    ##   Standard Errors                             Standard
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   y10_QI ~                                                              
    ##     y0_CATINC        -0.426    0.130   -3.286    0.001   -0.426   -0.074
    ##     y0_QI             1.279    0.038   33.627    0.000    1.279    0.743
    ##     y0_pbmi_fm2       2.383    0.529    4.504    0.000    2.383    0.100
    ##     y0_RACE           0.917    0.293    3.127    0.002    0.917    0.073
    ##     y5_AFTSNK         1.097    0.458    2.396    0.017    1.097    0.050
    ##     y5_ALONE         -0.394    0.263   -1.499    0.134   -0.394   -0.032
    ##     y7_SECRET         0.568    0.361    1.574    0.116    0.568    0.033
    ##     y5_FSTFOOD5      -0.162    0.195   -0.827    0.408   -0.162   -0.018
    ##     y3_ANXR           0.048    0.022    2.232    0.026    0.048    0.049
    ##   y5_AFTSNK ~                                                           
    ##     y0_CATINC        -0.012    0.009   -1.310    0.190   -0.012   -0.047
    ##   y5_ALONE ~                                                            
    ##     y0_CATINC         0.006    0.017    0.346    0.730    0.006    0.012
    ##   y7_SECRET ~                                                           
    ##     y0_CATINC         0.009    0.012    0.712    0.476    0.009    0.025
    ##   y5_FSTFOOD5 ~                                                         
    ##     y0_CATINC        -0.101    0.023   -4.487    0.000   -0.101   -0.156
    ##   y3_ANXR ~                                                             
    ##     y0_CATINC        -0.643    0.206   -3.114    0.002   -0.643   -0.110
    ##   y5_AFTSNK ~                                                           
    ##     y3_ANXR          -0.001    0.002   -0.602    0.547   -0.001   -0.021
    ##   y5_ALONE ~                                                            
    ##     y3_ANXR           0.013    0.003    4.510    0.000    0.013    0.159
    ##   y7_SECRET ~                                                           
    ##     y3_ANXR           0.009    0.002    4.211    0.000    0.009    0.149
    ##   y5_FSTFOOD5 ~                                                         
    ##     y3_ANXR           0.017    0.004    4.452    0.000    0.017    0.155
    ##   y0_QI ~                                                               
    ##     y0_pbmi_fm2       3.807    0.472    8.067    0.000    3.807    0.275
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##  .y5_AFTSNK ~~                                                          
    ##    .y5_ALONE         -0.004    0.005   -0.852    0.394   -0.004   -0.030
    ##    .y7_SECRET         0.001    0.004    0.183    0.855    0.001    0.006
    ##    .y5_FSTFOOD5      -0.011    0.007   -1.547    0.122   -0.011   -0.055
    ##    .y0_QI             0.069    0.036    1.905    0.057    0.069    0.068
    ##     y0_RACE          -0.009    0.005   -1.889    0.059   -0.009   -0.064
    ##     y0_pbmi_fm2       0.000    0.003    0.084    0.933    0.000    0.003
    ##  .y5_ALONE ~~                                                           
    ##    .y7_SECRET        -0.007    0.007   -1.039    0.299   -0.007   -0.037
    ##    .y5_FSTFOOD5       0.040    0.013    3.224    0.001    0.040    0.115
    ##    .y0_QI             0.122    0.064    1.920    0.055    0.122    0.068
    ##     y0_RACE           0.036    0.009    4.206    0.000    0.036    0.143
    ##     y0_pbmi_fm2       0.009    0.005    1.928    0.054    0.009    0.068
    ##  .y7_SECRET ~~                                                          
    ##    .y5_FSTFOOD5      -0.011    0.009   -1.188    0.235   -0.011   -0.042
    ##    .y0_QI             0.001    0.046    0.018    0.986    0.001    0.001
    ##     y0_RACE          -0.004    0.006   -0.605    0.545   -0.004   -0.020
    ##     y0_pbmi_fm2      -0.001    0.003   -0.265    0.791   -0.001   -0.009
    ##  .y5_FSTFOOD5 ~~                                                        
    ##    .y0_QI            -0.170    0.086   -1.971    0.049   -0.170   -0.070
    ##     y0_RACE           0.049    0.012    4.219    0.000    0.049    0.143
    ##     y0_pbmi_fm2       0.009    0.006    1.456    0.145    0.009    0.051
    ##  .y0_QI ~~                                                              
    ##     y0_RACE           0.232    0.061    3.774    0.000    0.232    0.132
    ##  .y3_ANXR ~~                                                            
    ##    .y0_QI            -0.615    0.786   -0.782    0.434   -0.615   -0.028
    ##  .y0_QI ~~                                                              
    ##     y0_CATINC         0.001    0.135    0.007    0.994    0.001    0.000
    ##   y0_pbmi_fm2 ~~                                                        
    ##     y0_RACE           0.030    0.005    6.220    0.000    0.030    0.226
    ##  .y3_ANXR ~~                                                            
    ##     y0_RACE           0.023    0.106    0.217    0.828    0.023    0.007
    ##   y0_CATINC ~~                                                          
    ##     y0_RACE          -0.177    0.020   -8.720    0.000   -0.177   -0.326
    ##  .y3_ANXR ~~                                                            
    ##     y0_pbmi_fm2       0.184    0.060    3.083    0.002    0.184    0.110
    ##   y0_CATINC ~~                                                          
    ##     y0_pbmi_fm2      -0.028    0.010   -2.680    0.007   -0.028   -0.096
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .y10_QI           13.676    0.687   19.912    0.000   13.676    0.344
    ##    .y5_AFTSNK         0.083    0.004   19.912    0.000    0.083    0.998
    ##    .y5_ALONE          0.260    0.013   19.912    0.000    0.260    0.975
    ##    .y7_SECRET         0.133    0.007   19.912    0.000    0.133    0.978
    ##    .y5_FSTFOOD5       0.473    0.024   19.912    0.000    0.473    0.946
    ##    .y3_ANXR          39.973    2.007   19.912    0.000   39.973    0.988
    ##    .y0_QI            12.398    0.623   19.912    0.000   12.398    0.924
    ##     y0_CATINC         1.183    0.059   19.912    0.000    1.183    1.000
    ##     y0_pbmi_fm2       0.070    0.004   19.912    0.000    0.070    1.000
    ##     y0_RACE           0.250    0.013   19.912    0.000    0.250    1.000

Anxiety Behavior Pruned Model
-----------------------------

``` r
mod_behav_anx_pruned <-
  '

    #################
    ## Regressions ##
    #################  
     
    y10_QI ~ y0_CATINC + y0_QI + y0_pbmi_fm2 + y0_RACE    

    y10_QI ~ y5_AFTSNK + y5_ALONE + y6_SECRET

    y3_ANXR ~ y0_CATINC

    y5_AFTSNK + y5_ALONE + y6_SECRET ~ y3_ANXR

    y0_QI ~ y0_pbmi_fm2

    #################
    ## Covariances ##
    #################  
  
    y5_AFTSNK ~~ y5_ALONE + 0*y6_SECRET + y0_QI + y0_RACE + y0_pbmi_fm2

    y5_ALONE ~~ 0*y6_SECRET + y0_QI + y0_RACE + y0_pbmi_fm2

    y6_SECRET ~~ 0*y0_QI + 0*y0_RACE + 0*y0_pbmi_fm2

    y0_QI ~~ y0_RACE + 0*y3_ANXR + 0*y0_CATINC

    y0_RACE ~~ y0_pbmi_fm2 + 0*y3_ANXR + y0_CATINC

    y0_pbmi_fm2 ~~ y3_ANXR + y0_CATINC
    
  '

fit_behav_anx_pruned <- sem(mod_behav_anx_pruned, data = semTestingData)
summary(fit_behav_anx_pruned, standardized = TRUE, fit.measures = TRUE)
```

    ## lavaan 0.6-3 ended normally after 121 iterations
    ## 
    ##   Optimization method                           NLMINB
    ##   Number of free parameters                         33
    ## 
    ##   Number of observations                           793
    ## 
    ##   Estimator                                         ML
    ##   Model Fit Test Statistic                      13.211
    ##   Degrees of freedom                                12
    ##   P-value (Chi-square)                           0.354
    ## 
    ## Model test baseline model:
    ## 
    ##   Minimum Function Test Statistic             1200.456
    ##   Degrees of freedom                                36
    ##   P-value                                        0.000
    ## 
    ## User model versus baseline model:
    ## 
    ##   Comparative Fit Index (CFI)                    0.999
    ##   Tucker-Lewis Index (TLI)                       0.997
    ## 
    ## Loglikelihood and Information Criteria:
    ## 
    ##   Loglikelihood user model (H0)              -9829.741
    ##   Loglikelihood unrestricted model (H1)      -9823.135
    ## 
    ##   Number of free parameters                         33
    ##   Akaike (AIC)                               19725.482
    ##   Bayesian (BIC)                             19879.784
    ##   Sample-size adjusted Bayesian (BIC)        19774.991
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.011
    ##   90 Percent Confidence Interval          0.000  0.039
    ##   P-value RMSEA <= 0.05                          0.995
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.019
    ## 
    ## Parameter Estimates:
    ## 
    ##   Information                                 Expected
    ##   Information saturated (h1) model          Structured
    ##   Standard Errors                             Standard
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   y10_QI ~                                                              
    ##     y0_CATINC        -0.371    0.135   -2.757    0.006   -0.371   -0.059
    ##     y0_QI             1.315    0.038   34.606    0.000    1.315    0.753
    ##     y0_pbmi_fm2       2.625    0.580    4.523    0.000    2.625    0.100
    ##     y0_RACE           0.512    0.305    1.676    0.094    0.512    0.037
    ##     y5_AFTSNK         2.214    0.485    4.568    0.000    2.214    0.094
    ##     y5_ALONE         -0.197    0.267   -0.736    0.462   -0.197   -0.015
    ##     y6_SECRET         0.644    0.391    1.647    0.099    0.644    0.034
    ##   y3_ANXR ~                                                             
    ##     y0_CATINC        -0.710    0.208   -3.414    0.001   -0.710   -0.120
    ##   y5_AFTSNK ~                                                           
    ##     y3_ANXR           0.003    0.002    2.179    0.029    0.003    0.077
    ##   y5_ALONE ~                                                            
    ##     y3_ANXR           0.011    0.003    3.982    0.000    0.011    0.138
    ##   y6_SECRET ~                                                           
    ##     y3_ANXR           0.011    0.002    5.589    0.000    0.011    0.195
    ##   y0_QI ~                                                               
    ##     y0_pbmi_fm2       4.776    0.507    9.412    0.000    4.776    0.317
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##  .y5_AFTSNK ~~                                                          
    ##    .y5_ALONE          0.007    0.006    1.213    0.225    0.007    0.043
    ##    .y6_SECRET         0.000                               0.000    0.000
    ##    .y0_QI             0.064    0.039    1.627    0.104    0.064    0.058
    ##     y0_RACE          -0.007    0.005   -1.323    0.186   -0.007   -0.045
    ##     y0_pbmi_fm2       0.006    0.003    2.259    0.024    0.006    0.079
    ##  .y5_ALONE ~~                                                           
    ##    .y6_SECRET         0.000                               0.000    0.000
    ##    .y0_QI             0.179    0.071    2.524    0.012    0.179    0.090
    ##     y0_RACE           0.034    0.009    3.695    0.000    0.034    0.127
    ##     y0_pbmi_fm2       0.006    0.005    1.331    0.183    0.006    0.047
    ##  .y6_SECRET ~~                                                          
    ##    .y0_QI             0.000                               0.000    0.000
    ##     y0_RACE           0.000                               0.000    0.000
    ##     y0_pbmi_fm2       0.000                               0.000    0.000
    ##  .y0_QI ~~                                                              
    ##     y0_RACE           0.165    0.063    2.630    0.009    0.165    0.088
    ##  .y3_ANXR ~~                                                            
    ##    .y0_QI             0.000                               0.000    0.000
    ##  .y0_QI ~~                                                              
    ##     y0_CATINC         0.000                               0.000    0.000
    ##   y0_pbmi_fm2 ~~                                                        
    ##     y0_RACE           0.029    0.005    6.066    0.000    0.029    0.219
    ##  .y3_ANXR ~~                                                            
    ##     y0_RACE           0.000                               0.000    0.000
    ##   y0_CATINC ~~                                                          
    ##     y0_RACE          -0.155    0.020   -7.768    0.000   -0.155   -0.283
    ##  .y3_ANXR ~~                                                            
    ##     y0_pbmi_fm2       0.215    0.059    3.656    0.000    0.215    0.127
    ##   y0_CATINC ~~                                                          
    ##     y0_pbmi_fm2      -0.037    0.010   -3.609    0.000   -0.037   -0.129
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .y10_QI           15.823    0.795   19.912    0.000   15.823    0.332
    ##    .y3_ANXR          41.301    2.074   19.912    0.000   41.301    0.986
    ##    .y5_AFTSNK         0.086    0.004   19.912    0.000    0.086    0.994
    ##    .y5_ALONE          0.282    0.014   19.912    0.000    0.282    0.981
    ##    .y6_SECRET         0.126    0.006   19.912    0.000    0.126    0.962
    ##    .y0_QI            14.088    0.707   19.912    0.000   14.088    0.900
    ##     y0_CATINC         1.204    0.060   19.912    0.000    1.204    1.000
    ##     y0_pbmi_fm2       0.069    0.003   19.926    0.000    0.069    1.000
    ##     y0_RACE           0.248    0.012   19.950    0.000    0.248    1.000

Anxiety Psych & Behavior Combined Model
---------------------------------------

``` r
mod_psych_behav_anx_initial <-
  '
    ######################
    ## Latent Variables ##
    ######################

    #################
    ## Regressions ##
    #################  
     
    y10_QI ~ y0_CATINC + y0_QI + y0_pbmi_fm2 + y0_RACE    

    y10_QI ~ y5_AFTSNK + y5_ALONE + y6_SECRET

    y10_QI ~ y5_AWARE + y5_DISTRST + y5_INEFCT + y5_SELWT

    
    y5_AWARE + y5_DISTRST + y5_INEFCT + y5_SELWT ~ y3_ANXR

    y5_AWARE + y5_DISTRST + y5_INEFCT + y5_SELWT + y3_ANXR ~ y0_CATINC

    y5_AWARE + y5_INEFCT + y5_SELWT ~ y0_QI

    y5_AFTSNK + y5_ALONE + y6_SECRET ~ y3_ANXR

   

    y0_QI ~ y0_pbmi_fm2

    #################
    ## Covariances ##
    #################  

    y5_AWARE ~~ y5_DISTRST + y5_INEFCT + y5_SELWT + y0_pbmi_fm2 + y0_RACE

    y5_DISTRST ~~ y5_INEFCT + y5_SELWT + y0_pbmi_fm2 + y0_RACE

    y5_INEFCT ~~ y5_SELWT + y0_pbmi_fm2 + 0*y0_RACE

    y5_SELWT ~~ 0*y0_pbmi_fm2 + y0_RACE

    y3_ANXR ~~ 0*y0_QI + y0_pbmi_fm2 + 0*y0_RACE  

    y0_QI + y0_pbmi_fm2 ~~ y0_RACE

    y0_pbmi_fm2 + y0_RACE ~~ y0_CATINC

    y0_CATINC ~~ 0*y0_QI
    

    y5_AFTSNK ~~ y5_ALONE + 0*y6_SECRET + y0_QI + y0_RACE + y0_pbmi_fm2

    y5_ALONE ~~ 0*y6_SECRET + y0_QI + y0_RACE + y0_pbmi_fm2

    y6_SECRET ~~ 0*y0_QI + 0*y0_RACE + 0*y0_pbmi_fm2
    
  '

fit_psych_behav_anx_initial <- sem(mod_psych_behav_anx_initial, data = semTestingData)
summary(fit_psych_behav_anx_initial, standardized = TRUE, fit.measures = TRUE)
```

    ## lavaan 0.6-3 ended normally after 169 iterations
    ## 
    ##   Optimization method                           NLMINB
    ##   Number of free parameters                         64
    ## 
    ##   Number of observations                           793
    ## 
    ##   Estimator                                         ML
    ##   Model Fit Test Statistic                      56.135
    ##   Degrees of freedom                                27
    ##   P-value (Chi-square)                           0.001
    ## 
    ## Model test baseline model:
    ## 
    ##   Minimum Function Test Statistic             2421.859
    ##   Degrees of freedom                                78
    ##   P-value                                        0.000
    ## 
    ## User model versus baseline model:
    ## 
    ##   Comparative Fit Index (CFI)                    0.988
    ##   Tucker-Lewis Index (TLI)                       0.964
    ## 
    ## Loglikelihood and Information Criteria:
    ## 
    ##   Loglikelihood user model (H0)             -16644.637
    ##   Loglikelihood unrestricted model (H1)     -16616.570
    ## 
    ##   Number of free parameters                         64
    ##   Akaike (AIC)                               33417.274
    ##   Bayesian (BIC)                             33716.527
    ##   Sample-size adjusted Bayesian (BIC)        33513.292
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.037
    ##   90 Percent Confidence Interval          0.023  0.050
    ##   P-value RMSEA <= 0.05                          0.943
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.035
    ## 
    ## Parameter Estimates:
    ## 
    ##   Information                                 Expected
    ##   Information saturated (h1) model          Structured
    ##   Standard Errors                             Standard
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   y10_QI ~                                                              
    ##     y0_CATINC        -0.315    0.135   -2.328    0.020   -0.315   -0.050
    ##     y0_QI             1.309    0.038   34.579    0.000    1.309    0.752
    ##     y0_pbmi_fm2       2.461    0.577    4.264    0.000    2.461    0.094
    ##     y0_RACE           0.367    0.317    1.157    0.247    0.367    0.027
    ##     y5_AFTSNK         2.079    0.480    4.332    0.000    2.079    0.089
    ##     y5_ALONE         -0.372    0.265   -1.404    0.160   -0.372   -0.029
    ##     y6_SECRET         0.553    0.388    1.424    0.154    0.553    0.029
    ##     y5_AWARE          0.038    0.037    1.038    0.299    0.038    0.026
    ##     y5_DISTRST        0.148    0.049    3.006    0.003    0.148    0.072
    ##     y5_INEFCT        -0.071    0.054   -1.318    0.188   -0.071   -0.040
    ##     y5_SELWT         -0.496    0.269   -1.845    0.065   -0.496   -0.047
    ##   y5_AWARE ~                                                            
    ##     y3_ANXR           0.253    0.023   10.826    0.000    0.253    0.351
    ##   y5_DISTRST ~                                                          
    ##     y3_ANXR           0.122    0.017    7.087    0.000    0.122    0.236
    ##   y5_INEFCT ~                                                           
    ##     y3_ANXR           0.210    0.019   10.820    0.000    0.210    0.355
    ##   y5_SELWT ~                                                            
    ##     y3_ANXR          -0.041    0.003  -12.554    0.000   -0.041   -0.405
    ##   y5_AWARE ~                                                            
    ##     y0_CATINC        -0.586    0.139   -4.226    0.000   -0.586   -0.138
    ##   y5_DISTRST ~                                                          
    ##     y0_CATINC        -0.494    0.104   -4.743    0.000   -0.494   -0.162
    ##   y5_INEFCT ~                                                           
    ##     y0_CATINC        -0.373    0.114   -3.262    0.001   -0.373   -0.107
    ##   y5_SELWT ~                                                            
    ##     y0_CATINC        -0.001    0.019   -0.049    0.961   -0.001   -0.002
    ##   y3_ANXR ~                                                             
    ##     y0_CATINC        -0.710    0.208   -3.414    0.001   -0.710   -0.120
    ##   y5_AWARE ~                                                            
    ##     y0_QI             0.120    0.039    3.062    0.002    0.120    0.102
    ##   y5_INEFCT ~                                                           
    ##     y0_QI             0.076    0.030    2.526    0.012    0.076    0.079
    ##   y5_SELWT ~                                                            
    ##     y0_QI            -0.013    0.005   -2.415    0.016   -0.013   -0.077
    ##   y5_AFTSNK ~                                                           
    ##     y3_ANXR           0.003    0.002    2.173    0.030    0.003    0.077
    ##   y5_ALONE ~                                                            
    ##     y3_ANXR           0.011    0.003    3.959    0.000    0.011    0.138
    ##   y6_SECRET ~                                                           
    ##     y3_ANXR           0.011    0.002    5.589    0.000    0.011    0.195
    ##   y0_QI ~                                                               
    ##     y0_pbmi_fm2       4.760    0.508    9.375    0.000    4.760    0.316
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##  .y5_AWARE ~~                                                           
    ##    .y5_DISTRST        2.886    0.488    5.914    0.000    2.886    0.212
    ##    .y5_INEFCT         6.812    0.578   11.781    0.000    6.812    0.458
    ##    .y5_SELWT         -0.435    0.090   -4.805    0.000   -0.435   -0.173
    ##     y0_pbmi_fm2       0.004    0.040    0.102    0.919    0.004    0.004
    ##     y0_RACE           0.226    0.064    3.541    0.000    0.226    0.107
    ##  .y5_DISTRST ~~                                                         
    ##    .y5_INEFCT         4.479    0.419   10.701    0.000    4.479    0.400
    ##    .y5_SELWT         -0.384    0.068   -5.640    0.000   -0.384   -0.203
    ##     y0_pbmi_fm2       0.067    0.029    2.340    0.019    0.067    0.080
    ##     y0_RACE           0.342    0.051    6.757    0.000    0.342    0.215
    ##  .y5_INEFCT ~~                                                          
    ##    .y5_SELWT         -1.046    0.082  -12.803    0.000   -1.046   -0.505
    ##     y0_pbmi_fm2       0.036    0.028    1.263    0.206    0.036    0.039
    ##     y0_RACE           0.000                               0.000    0.000
    ##  .y5_SELWT ~~                                                           
    ##     y0_pbmi_fm2       0.000                               0.000    0.000
    ##     y0_RACE           0.036    0.009    4.283    0.000    0.036    0.124
    ##  .y3_ANXR ~~                                                            
    ##    .y0_QI             0.000                               0.000    0.000
    ##     y0_pbmi_fm2       0.215    0.059    3.658    0.000    0.215    0.127
    ##     y0_RACE           0.000                               0.000    0.000
    ##  .y0_QI ~~                                                              
    ##     y0_RACE           0.155    0.061    2.540    0.011    0.155    0.083
    ##   y0_pbmi_fm2 ~~                                                        
    ##     y0_RACE           0.029    0.005    6.181    0.000    0.029    0.221
    ##   y0_CATINC ~~                                                          
    ##     y0_pbmi_fm2      -0.037    0.010   -3.636    0.000   -0.037   -0.130
    ##     y0_RACE          -0.156    0.020   -7.801    0.000   -0.156   -0.285
    ##  .y0_QI ~~                                                              
    ##     y0_CATINC         0.000                               0.000    0.000
    ##  .y5_AFTSNK ~~                                                          
    ##    .y5_ALONE          0.007    0.006    1.213    0.225    0.007    0.043
    ##    .y6_SECRET         0.000                               0.000    0.000
    ##    .y0_QI             0.064    0.039    1.629    0.103    0.064    0.058
    ##     y0_RACE          -0.008    0.005   -1.613    0.107   -0.008   -0.052
    ##     y0_pbmi_fm2       0.006    0.003    2.099    0.036    0.006    0.073
    ##  .y5_ALONE ~~                                                           
    ##    .y6_SECRET         0.000                               0.000    0.000
    ##    .y0_QI             0.179    0.071    2.525    0.012    0.179    0.090
    ##     y0_RACE           0.029    0.009    3.354    0.001    0.029    0.110
    ##     y0_pbmi_fm2       0.005    0.005    1.070    0.285    0.005    0.037
    ##  .y6_SECRET ~~                                                          
    ##    .y0_QI             0.000                               0.000    0.000
    ##     y0_RACE           0.000                               0.000    0.000
    ##     y0_pbmi_fm2       0.000                               0.000    0.000
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .y10_QI           15.504    0.779   19.912    0.000   15.504    0.327
    ##    .y5_AWARE         18.054    0.904   19.966    0.000   18.054    0.831
    ##    .y5_DISTRST       10.218    0.509   20.078    0.000   10.218    0.909
    ##    .y5_INEFCT        12.270    0.616   19.918    0.000   12.270    0.842
    ##    .y5_SELWT          0.349    0.017   20.000    0.000    0.349    0.827
    ##    .y3_ANXR          41.301    2.074   19.912    0.000   41.301    0.986
    ##    .y5_AFTSNK         0.086    0.004   19.912    0.000    0.086    0.994
    ##    .y5_ALONE          0.282    0.014   19.912    0.000    0.282    0.981
    ##    .y6_SECRET         0.126    0.006   19.912    0.000    0.126    0.962
    ##    .y0_QI            14.088    0.707   19.912    0.000   14.088    0.900
    ##     y0_CATINC         1.204    0.060   19.912    0.000    1.204    1.000
    ##     y0_pbmi_fm2       0.069    0.003   19.928    0.000    0.069    1.000
    ##     y0_RACE           0.248    0.012   19.981    0.000    0.248    1.000
