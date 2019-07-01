NGHS1 Path Model Sensitivity Analyses
================
Daniel O'Leary
April 22, 2019

-   [Setup](#setup)
    -   [Load Packages](#load-packages)
    -   [Load Data](#load-data)
-   [Models](#models)
    -   [Stress Psych Pruned Model - WAIST MIN](#stress-psych-pruned-model---waist-min)
    -   [Anxiety Psych Pruned Model - WAIST MIN](#anxiety-psych-pruned-model---waist-min)
    -   [Stress Psych Pruned Model - WAIST UMB](#stress-psych-pruned-model---waist-umb)
    -   [Anxiety Psych Pruned Model - WAIST UMB](#anxiety-psych-pruned-model---waist-umb)
    -   [Stress Psych Pruned Model - SUMSKIN](#stress-psych-pruned-model---sumskin)
    -   [Anxiety Psych Pruned Model - SUMSKIN](#anxiety-psych-pruned-model---sumskin)
    -   [Stress Psych Pruned Model - ARMCIRAV](#stress-psych-pruned-model---armcirav)
    -   [Anxiety Psych Pruned Model - ARMCIRAV](#anxiety-psych-pruned-model---armcirav)

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
  read_csv("G:/My Drive/research/projects/phsr1/nghs1/nghs1_data/nghs1_imputed_new2.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double()
    ## )
    ## See spec(...) for full column specifications.

Models
======

Stress Psych Pruned Model - WAIST MIN
-------------------------------------

``` r
mod_pysch_stress_pruned_wm <-
  '
    ######################
    ## Latent Variables ##
    ######################

    #################
    ## Regressions ##
    #################  
     
    y10_WAISTMIN ~ y0_CATINC + y0_QI + y0_pbmi + y0_RACE  

    y10_WAISTMIN ~ y5_AWARE + y5_DISTRST + y5_INEFCT + y5_SELWT

    y5_AWARE + y5_DISTRST + y5_INEFCT + y5_SELWT ~ y2_STRESS 

    y5_AWARE + y5_DISTRST + y5_INEFCT + y2_STRESS ~ y0_CATINC + y0_RACE + y0_pbmi

    y5_AWARE + y5_INEFCT + y5_SELWT ~ y0_QI 

    y5_SELWT ~ y0_pbmi + y0_CATINC + y0_RACE

    y0_QI ~ y0_pbmi + 0*y0_CATINC + y0_RACE

    y0_pbmi + y0_CATINC ~ y0_RACE

    
    #################
    ## Covariances ##
    #################    

    y5_AWARE ~~ y5_DISTRST + y5_INEFCT + y5_SELWT 

    y5_DISTRST ~~ y5_INEFCT + y5_SELWT 

    y5_INEFCT ~~ y5_SELWT 

    y2_STRESS ~~ 0*y0_QI   

    y0_pbmi ~~ 0*y0_CATINC
  
  '

fit_psych_stress_pruned_wm <- 
  sem(
    mod_pysch_stress_pruned_wm, 
    data = semTestingData, 
    estimator = "MLM"
  )

summary(
  fit_psych_stress_pruned_wm, 
  standardized = TRUE, 
  fit.measures = TRUE
)
```

    ## lavaan 0.6-3 ended normally after 159 iterations
    ## 
    ##   Optimization method                           NLMINB
    ##   Number of free parameters                         49
    ## 
    ##   Number of observations                           793
    ## 
    ##   Estimator                                         ML      Robust
    ##   Model Fit Test Statistic                       6.456       6.017
    ##   Degrees of freedom                                 5           5
    ##   P-value (Chi-square)                           0.264       0.305
    ##   Scaling correction factor                                  1.073
    ##     for the Satorra-Bentler correction
    ## 
    ## Model test baseline model:
    ## 
    ##   Minimum Function Test Statistic             2030.690    1603.909
    ##   Degrees of freedom                                45          45
    ##   P-value                                        0.000       0.000
    ## 
    ## User model versus baseline model:
    ## 
    ##   Comparative Fit Index (CFI)                    0.999       0.999
    ##   Tucker-Lewis Index (TLI)                       0.993       0.994
    ## 
    ##   Robust Comparative Fit Index (CFI)                         0.999
    ##   Robust Tucker-Lewis Index (TLI)                            0.995
    ## 
    ## Loglikelihood and Information Criteria:
    ## 
    ##   Loglikelihood user model (H0)             -18313.270  -18313.270
    ##   Loglikelihood unrestricted model (H1)     -18310.042  -18310.042
    ## 
    ##   Number of free parameters                         49          49
    ##   Akaike (AIC)                               36724.540   36724.540
    ##   Bayesian (BIC)                             36953.655   36953.655
    ##   Sample-size adjusted Bayesian (BIC)        36798.054   36798.054
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.019       0.016
    ##   90 Percent Confidence Interval          0.000  0.056       0.000  0.053
    ##   P-value RMSEA <= 0.05                          0.908       0.932
    ## 
    ##   Robust RMSEA                                               0.017
    ##   90 Percent Confidence Interval                             0.000  0.056
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.011       0.011
    ## 
    ## Parameter Estimates:
    ## 
    ##   Information                                 Expected
    ##   Information saturated (h1) model          Structured
    ##   Standard Errors                           Robust.sem
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   y10_WAISTMIN ~                                                        
    ##     y0_CATINC        -0.628    0.291   -2.158    0.031   -0.628   -0.051
    ##     y0_QI             2.488    0.095   26.184    0.000    2.488    0.735
    ##     y0_pbmi           0.210    0.044    4.734    0.000    0.210    0.109
    ##     y0_RACE          -0.934    0.683   -1.367    0.171   -0.934   -0.035
    ##     y5_AWARE          0.063    0.078    0.812    0.417    0.063    0.022
    ##     y5_DISTRST        0.293    0.120    2.440    0.015    0.293    0.073
    ##     y5_INEFCT        -0.044    0.124   -0.353    0.724   -0.044   -0.013
    ##     y5_SELWT         -0.708    0.558   -1.270    0.204   -0.708   -0.034
    ##   y5_AWARE ~                                                            
    ##     y2_STRESS         0.108    0.022    4.865    0.000    0.108    0.160
    ##   y5_DISTRST ~                                                          
    ##     y2_STRESS         0.059    0.019    3.200    0.001    0.059    0.122
    ##   y5_INEFCT ~                                                           
    ##     y2_STRESS         0.132    0.021    6.275    0.000    0.132    0.238
    ##   y5_SELWT ~                                                            
    ##     y2_STRESS        -0.023    0.003   -7.063    0.000   -0.023   -0.247
    ##   y5_AWARE ~                                                            
    ##     y0_CATINC        -0.543    0.163   -3.330    0.001   -0.543   -0.128
    ##     y0_RACE           0.910    0.330    2.755    0.006    0.910    0.098
    ##     y0_pbmi           0.016    0.024    0.654    0.513    0.016    0.023
    ##   y5_DISTRST ~                                                          
    ##     y0_CATINC        -0.309    0.112   -2.766    0.006   -0.309   -0.101
    ##     y0_RACE           1.458    0.242    6.028    0.000    1.458    0.218
    ##     y0_pbmi           0.037    0.016    2.321    0.020    0.037    0.078
    ##   y5_INEFCT ~                                                           
    ##     y0_CATINC        -0.394    0.130   -3.022    0.003   -0.394   -0.113
    ##     y0_RACE          -0.158    0.285   -0.556    0.578   -0.158   -0.021
    ##     y0_pbmi           0.048    0.023    2.092    0.036    0.048    0.088
    ##   y2_STRESS ~                                                           
    ##     y0_CATINC        -0.865    0.237   -3.641    0.000   -0.865   -0.137
    ##     y0_RACE          -1.014    0.522   -1.943    0.052   -1.014   -0.073
    ##     y0_pbmi           0.099    0.035    2.779    0.005    0.099    0.100
    ##   y5_AWARE ~                                                            
    ##     y0_QI             0.126    0.046    2.771    0.006    0.126    0.107
    ##   y5_INEFCT ~                                                           
    ##     y0_QI             0.088    0.040    2.218    0.027    0.088    0.091
    ##   y5_SELWT ~                                                            
    ##     y0_QI            -0.016    0.006   -2.483    0.013   -0.016   -0.097
    ##     y0_pbmi          -0.007    0.004   -1.875    0.061   -0.007   -0.077
    ##     y0_CATINC         0.025    0.020    1.231    0.218    0.025    0.042
    ##     y0_RACE           0.171    0.047    3.629    0.000    0.171    0.131
    ##   y0_QI ~                                                               
    ##     y0_pbmi           0.164    0.027    6.068    0.000    0.164    0.289
    ##     y0_CATINC         0.000                               0.000    0.000
    ##     y0_RACE           0.684    0.270    2.535    0.011    0.684    0.086
    ##   y0_pbmi ~                                                             
    ##     y0_RACE           2.981    0.480    6.205    0.000    2.981    0.213
    ##   y0_CATINC ~                                                           
    ##     y0_RACE          -0.637    0.074   -8.618    0.000   -0.637   -0.290
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##  .y5_AWARE ~~                                                           
    ##    .y5_DISTRST        3.443    0.608    5.658    0.000    3.443    0.244
    ##    .y5_INEFCT         8.246    1.100    7.500    0.000    8.246    0.511
    ##    .y5_SELWT         -0.762    0.113   -6.739    0.000   -0.762   -0.276
    ##  .y5_DISTRST ~~                                                         
    ##    .y5_INEFCT         5.046    0.646    7.811    0.000    5.046    0.440
    ##    .y5_SELWT         -0.561    0.082   -6.831    0.000   -0.561   -0.286
    ##  .y5_INEFCT ~~                                                          
    ##    .y5_SELWT         -1.229    0.120  -10.223    0.000   -1.229   -0.549
    ##  .y2_STRESS ~~                                                          
    ##    .y0_QI             0.000                               0.000    0.000
    ##  .y0_pbmi ~~                                                            
    ##    .y0_CATINC         0.000                               0.000    0.000
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .y10_WAISTMIN     67.120    4.491   14.944    0.000   67.120    0.374
    ##    .y5_AWARE         19.873    1.742   11.408    0.000   19.873    0.916
    ##    .y5_DISTRST       10.018    0.713   14.059    0.000   10.018    0.896
    ##    .y5_INEFCT        13.099    1.169   11.205    0.000   13.099    0.898
    ##    .y5_SELWT          0.383    0.020   18.853    0.000    0.383    0.903
    ##    .y2_STRESS        46.316    2.530   18.309    0.000   46.316    0.973
    ##    .y0_QI            14.079    0.870   16.186    0.000   14.079    0.899
    ##    .y0_pbmi          46.404    4.188   11.080    0.000   46.404    0.954
    ##    .y0_CATINC         1.102    0.043   25.847    0.000    1.102    0.916

Anxiety Psych Pruned Model - WAIST MIN
--------------------------------------

``` r
mod_psych_anx_pruned_wm <-
  '
    #################
    ## Regressions ##
    #################  
     
    y10_WAISTMIN ~ y0_CATINC + y0_QI + y0_pbmi + y0_RACE  

    y10_WAISTMIN ~ y5_AWARE + y5_DISTRST + y5_INEFCT + y5_SELWT 

    y5_AWARE + y5_DISTRST + y5_INEFCT + y5_SELWT ~ y3_ANXR 

    y5_AWARE + y5_DISTRST + y5_INEFCT + y3_ANXR ~ y0_CATINC + y0_pbmi

    y5_AWARE + y5_DISTRST + y5_INEFCT ~ y0_RACE

    y5_AWARE + y5_INEFCT + y5_SELWT ~ y0_QI 

    y5_SELWT ~ y0_pbmi + y0_CATINC + y0_RACE

    y0_QI ~ y0_pbmi + 0*y0_CATINC + y0_RACE

    y0_pbmi + y0_CATINC ~ y0_RACE

    
    #################
    ## Covariances ##
    #################    

    y5_AWARE ~~ y5_DISTRST + y5_INEFCT + y5_SELWT 

    y5_DISTRST ~~ y5_INEFCT + y5_SELWT 

    y5_INEFCT ~~ y5_SELWT 

    y3_ANXR ~~ 0*y0_QI + 0*y0_RACE   

    y0_pbmi ~~ 0*y0_CATINC

  '


fit_psych_anx_pruned_wm <-
  sem(
    mod_psych_anx_pruned_wm, 
    data = semTestingData, 
    estimator = "MLM"
  )

summary(
  fit_psych_anx_pruned_wm, 
  standardized = TRUE, 
  fit.measures = TRUE
)
```

    ## lavaan 0.6-3 ended normally after 139 iterations
    ## 
    ##   Optimization method                           NLMINB
    ##   Number of free parameters                         49
    ## 
    ##   Number of observations                           793
    ## 
    ##   Estimator                                         ML      Robust
    ##   Model Fit Test Statistic                      10.212       9.557
    ##   Degrees of freedom                                 6           6
    ##   P-value (Chi-square)                           0.116       0.145
    ##   Scaling correction factor                                  1.069
    ##     for the Satorra-Bentler correction
    ## 
    ## Model test baseline model:
    ## 
    ##   Minimum Function Test Statistic             2172.600    1737.013
    ##   Degrees of freedom                                45          45
    ##   P-value                                        0.000       0.000
    ## 
    ## User model versus baseline model:
    ## 
    ##   Comparative Fit Index (CFI)                    0.998       0.998
    ##   Tucker-Lewis Index (TLI)                       0.985       0.984
    ## 
    ##   Robust Comparative Fit Index (CFI)                         0.998
    ##   Robust Tucker-Lewis Index (TLI)                            0.987
    ## 
    ## Loglikelihood and Information Criteria:
    ## 
    ##   Loglikelihood user model (H0)             -18767.537  -18767.537
    ##   Loglikelihood unrestricted model (H1)     -18762.431  -18762.431
    ## 
    ##   Number of free parameters                         49          49
    ##   Akaike (AIC)                               37633.073   37633.073
    ##   Bayesian (BIC)                             37862.188   37862.188
    ##   Sample-size adjusted Bayesian (BIC)        37706.587   37706.587
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.030       0.027
    ##   90 Percent Confidence Interval          0.000  0.060       0.000  0.057
    ##   P-value RMSEA <= 0.05                          0.845       0.881
    ## 
    ##   Robust RMSEA                                               0.028
    ##   90 Percent Confidence Interval                             0.000  0.060
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.016       0.016
    ## 
    ## Parameter Estimates:
    ## 
    ##   Information                                 Expected
    ##   Information saturated (h1) model          Structured
    ##   Standard Errors                           Robust.sem
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   y10_WAISTMIN ~                                                        
    ##     y0_CATINC        -0.628    0.291   -2.160    0.031   -0.628   -0.051
    ##     y0_QI             2.488    0.095   26.291    0.000    2.488    0.736
    ##     y0_pbmi           0.210    0.044    4.738    0.000    0.210    0.109
    ##     y0_RACE          -0.934    0.682   -1.370    0.171   -0.934   -0.035
    ##     y5_AWARE          0.063    0.078    0.813    0.416    0.063    0.022
    ##     y5_DISTRST        0.293    0.120    2.442    0.015    0.293    0.073
    ##     y5_INEFCT        -0.044    0.124   -0.353    0.724   -0.044   -0.012
    ##     y5_SELWT         -0.708    0.556   -1.273    0.203   -0.708   -0.034
    ##   y5_AWARE ~                                                            
    ##     y3_ANXR           0.253    0.025   10.087    0.000    0.253    0.351
    ##   y5_DISTRST ~                                                          
    ##     y3_ANXR           0.118    0.017    6.794    0.000    0.118    0.227
    ##   y5_INEFCT ~                                                           
    ##     y3_ANXR           0.204    0.021    9.603    0.000    0.204    0.346
    ##   y5_SELWT ~                                                            
    ##     y3_ANXR          -0.040    0.003  -11.968    0.000   -0.040   -0.398
    ##   y5_AWARE ~                                                            
    ##     y0_CATINC        -0.458    0.148   -3.099    0.002   -0.458   -0.108
    ##     y0_pbmi          -0.001    0.024   -0.047    0.963   -0.001   -0.002
    ##   y5_DISTRST ~                                                          
    ##     y0_CATINC        -0.278    0.105   -2.632    0.008   -0.278   -0.091
    ##     y0_pbmi           0.029    0.015    1.925    0.054    0.029    0.060
    ##   y5_INEFCT ~                                                           
    ##     y0_CATINC        -0.364    0.124   -2.944    0.003   -0.364   -0.105
    ##     y0_pbmi           0.038    0.021    1.792    0.073    0.038    0.070
    ##   y3_ANXR ~                                                             
    ##     y0_CATINC        -0.618    0.202   -3.061    0.002   -0.618   -0.105
    ##     y0_pbmi           0.110    0.029    3.773    0.000    0.110    0.119
    ##   y5_AWARE ~                                                            
    ##     y0_RACE           0.984    0.307    3.208    0.001    0.984    0.105
    ##   y5_DISTRST ~                                                          
    ##     y0_RACE           1.477    0.234    6.301    0.000    1.477    0.220
    ##   y5_INEFCT ~                                                           
    ##     y0_RACE          -0.148    0.279   -0.530    0.596   -0.148   -0.019
    ##   y5_AWARE ~                                                            
    ##     y0_QI             0.110    0.043    2.588    0.010    0.110    0.094
    ##   y5_INEFCT ~                                                           
    ##     y0_QI             0.079    0.038    2.048    0.041    0.079    0.082
    ##   y5_SELWT ~                                                            
    ##     y0_QI            -0.014    0.006   -2.267    0.023   -0.014   -0.084
    ##     y0_pbmi          -0.005    0.003   -1.482    0.138   -0.005   -0.054
    ##     y0_CATINC         0.017    0.019    0.881    0.378    0.017    0.029
    ##     y0_RACE           0.165    0.044    3.728    0.000    0.165    0.127
    ##   y0_QI ~                                                               
    ##     y0_pbmi           0.164    0.027    6.068    0.000    0.164    0.289
    ##     y0_CATINC         0.000                               0.000    0.000
    ##     y0_RACE           0.684    0.270    2.535    0.011    0.684    0.086
    ##   y0_pbmi ~                                                             
    ##     y0_RACE           2.981    0.480    6.205    0.000    2.981    0.213
    ##   y0_CATINC ~                                                           
    ##     y0_RACE          -0.637    0.074   -8.618    0.000   -0.637   -0.290
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##  .y5_AWARE ~~                                                           
    ##    .y5_DISTRST        2.530    0.562    4.499    0.000    2.530    0.193
    ##    .y5_INEFCT         6.813    0.948    7.185    0.000    6.813    0.462
    ##    .y5_SELWT         -0.469    0.100   -4.679    0.000   -0.469   -0.190
    ##  .y5_DISTRST ~~                                                         
    ##    .y5_INEFCT         4.432    0.603    7.355    0.000    4.432    0.409
    ##    .y5_SELWT         -0.434    0.080   -5.428    0.000   -0.434   -0.239
    ##  .y5_INEFCT ~~                                                          
    ##    .y5_SELWT         -1.040    0.105   -9.859    0.000   -1.040   -0.508
    ##  .y3_ANXR ~~                                                            
    ##    .y0_QI             0.000                               0.000    0.000
    ##     y0_RACE           0.000                               0.000    0.000
    ##  .y0_pbmi ~~                                                            
    ##    .y0_CATINC         0.000                               0.000    0.000
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .y10_WAISTMIN     67.120    4.491   14.944    0.000   67.120    0.374
    ##    .y5_AWARE         17.820    1.563   11.401    0.000   17.820    0.820
    ##    .y5_DISTRST        9.617    0.688   13.979    0.000    9.617    0.856
    ##    .y5_INEFCT        12.215    1.075   11.368    0.000   12.215    0.839
    ##    .y5_SELWT          0.343    0.018   19.269    0.000    0.343    0.814
    ##    .y3_ANXR          40.720    1.682   24.203    0.000   40.720    0.973
    ##    .y0_QI            14.079    0.870   16.186    0.000   14.079    0.899
    ##    .y0_pbmi          46.404    4.188   11.080    0.000   46.404    0.954
    ##    .y0_CATINC         1.102    0.043   25.847    0.000    1.102    0.916
    ##     y0_RACE           0.249    0.001  285.949    0.000    0.249    1.000

Stress Psych Pruned Model - WAIST UMB
-------------------------------------

``` r
mod_pysch_stress_pruned_wu <-
  '
    ######################
    ## Latent Variables ##
    ######################

    #################
    ## Regressions ##
    #################  
     
    y10_WAISTUMB ~ y0_CATINC + y0_QI + y0_pbmi + y0_RACE  

    y10_WAISTUMB ~ y5_AWARE + y5_DISTRST + y5_INEFCT + y5_SELWT

    y5_AWARE + y5_DISTRST + y5_INEFCT + y5_SELWT ~ y2_STRESS 

    y5_AWARE + y5_DISTRST + y5_INEFCT + y2_STRESS ~ y0_CATINC + y0_RACE + y0_pbmi

    y5_AWARE + y5_INEFCT + y5_SELWT ~ y0_QI 

    y5_SELWT ~ y0_pbmi + y0_CATINC + y0_RACE

    y0_QI ~ y0_pbmi + 0*y0_CATINC + y0_RACE

    y0_pbmi + y0_CATINC ~ y0_RACE

    
    #################
    ## Covariances ##
    #################    

    y5_AWARE ~~ y5_DISTRST + y5_INEFCT + y5_SELWT 

    y5_DISTRST ~~ y5_INEFCT + y5_SELWT 

    y5_INEFCT ~~ y5_SELWT 

    y2_STRESS ~~ 0*y0_QI   

    y0_pbmi ~~ 0*y0_CATINC
  
  '

fit_psych_stress_pruned_wu <- 
  sem(
    mod_pysch_stress_pruned_wu, 
    data = semTestingData, 
    estimator = "MLM"
  )

summary(
  fit_psych_stress_pruned_wu, 
  standardized = TRUE, 
  fit.measures = TRUE
)
```

    ## lavaan 0.6-3 ended normally after 162 iterations
    ## 
    ##   Optimization method                           NLMINB
    ##   Number of free parameters                         49
    ## 
    ##   Number of observations                           793
    ## 
    ##   Estimator                                         ML      Robust
    ##   Model Fit Test Statistic                       6.540       6.078
    ##   Degrees of freedom                                 5           5
    ##   P-value (Chi-square)                           0.257       0.299
    ##   Scaling correction factor                                  1.076
    ##     for the Satorra-Bentler correction
    ## 
    ## Model test baseline model:
    ## 
    ##   Minimum Function Test Statistic             1988.521    1577.256
    ##   Degrees of freedom                                45          45
    ##   P-value                                        0.000       0.000
    ## 
    ## User model versus baseline model:
    ## 
    ##   Comparative Fit Index (CFI)                    0.999       0.999
    ##   Tucker-Lewis Index (TLI)                       0.993       0.994
    ## 
    ##   Robust Comparative Fit Index (CFI)                         0.999
    ##   Robust Tucker-Lewis Index (TLI)                            0.995
    ## 
    ## Loglikelihood and Information Criteria:
    ## 
    ##   Loglikelihood user model (H0)             -18445.072  -18445.072
    ##   Loglikelihood unrestricted model (H1)     -18441.801  -18441.801
    ## 
    ##   Number of free parameters                         49          49
    ##   Akaike (AIC)                               36988.143   36988.143
    ##   Bayesian (BIC)                             37217.258   37217.258
    ##   Sample-size adjusted Bayesian (BIC)        37061.657   37061.657
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.020       0.016
    ##   90 Percent Confidence Interval          0.000  0.056       0.000  0.053
    ##   P-value RMSEA <= 0.05                          0.905       0.931
    ## 
    ##   Robust RMSEA                                               0.017
    ##   90 Percent Confidence Interval                             0.000  0.056
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.011       0.011
    ## 
    ## Parameter Estimates:
    ## 
    ##   Information                                 Expected
    ##   Information saturated (h1) model          Structured
    ##   Standard Errors                           Robust.sem
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   y10_WAISTUMB ~                                                        
    ##     y0_CATINC        -0.893    0.353   -2.530    0.011   -0.893   -0.064
    ##     y0_QI             2.865    0.108   26.411    0.000    2.865    0.736
    ##     y0_pbmi           0.161    0.054    2.988    0.003    0.161    0.073
    ##     y0_RACE          -1.756    0.804   -2.185    0.029   -1.756   -0.057
    ##     y5_AWARE          0.089    0.095    0.938    0.348    0.089    0.027
    ##     y5_DISTRST        0.358    0.138    2.590    0.010    0.358    0.078
    ##     y5_INEFCT        -0.078    0.159   -0.489    0.625   -0.078   -0.019
    ##     y5_SELWT         -1.135    0.667   -1.702    0.089   -1.135   -0.048
    ##   y5_AWARE ~                                                            
    ##     y2_STRESS         0.108    0.022    4.865    0.000    0.108    0.160
    ##   y5_DISTRST ~                                                          
    ##     y2_STRESS         0.059    0.019    3.200    0.001    0.059    0.122
    ##   y5_INEFCT ~                                                           
    ##     y2_STRESS         0.132    0.021    6.275    0.000    0.132    0.238
    ##   y5_SELWT ~                                                            
    ##     y2_STRESS        -0.023    0.003   -7.063    0.000   -0.023   -0.247
    ##   y5_AWARE ~                                                            
    ##     y0_CATINC        -0.543    0.163   -3.330    0.001   -0.543   -0.128
    ##     y0_RACE           0.910    0.330    2.755    0.006    0.910    0.098
    ##     y0_pbmi           0.016    0.024    0.654    0.513    0.016    0.023
    ##   y5_DISTRST ~                                                          
    ##     y0_CATINC        -0.309    0.112   -2.766    0.006   -0.309   -0.101
    ##     y0_RACE           1.458    0.242    6.028    0.000    1.458    0.218
    ##     y0_pbmi           0.037    0.016    2.321    0.020    0.037    0.078
    ##   y5_INEFCT ~                                                           
    ##     y0_CATINC        -0.394    0.130   -3.022    0.003   -0.394   -0.113
    ##     y0_RACE          -0.159    0.285   -0.556    0.578   -0.159   -0.021
    ##     y0_pbmi           0.048    0.023    2.092    0.036    0.048    0.088
    ##   y2_STRESS ~                                                           
    ##     y0_CATINC        -0.865    0.237   -3.641    0.000   -0.865   -0.137
    ##     y0_RACE          -1.014    0.522   -1.943    0.052   -1.014   -0.073
    ##     y0_pbmi           0.099    0.035    2.779    0.005    0.099    0.100
    ##   y5_AWARE ~                                                            
    ##     y0_QI             0.126    0.046    2.771    0.006    0.126    0.107
    ##   y5_INEFCT ~                                                           
    ##     y0_QI             0.088    0.040    2.218    0.027    0.088    0.091
    ##   y5_SELWT ~                                                            
    ##     y0_QI            -0.016    0.006   -2.483    0.013   -0.016   -0.097
    ##     y0_pbmi          -0.007    0.004   -1.875    0.061   -0.007   -0.077
    ##     y0_CATINC         0.025    0.020    1.231    0.218    0.025    0.042
    ##     y0_RACE           0.171    0.047    3.629    0.000    0.171    0.131
    ##   y0_QI ~                                                               
    ##     y0_pbmi           0.164    0.027    6.068    0.000    0.164    0.289
    ##     y0_CATINC         0.000                               0.000    0.000
    ##     y0_RACE           0.684    0.270    2.535    0.011    0.684    0.086
    ##   y0_pbmi ~                                                             
    ##     y0_RACE           2.981    0.480    6.205    0.000    2.981    0.213
    ##   y0_CATINC ~                                                           
    ##     y0_RACE          -0.637    0.074   -8.618    0.000   -0.637   -0.290
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##  .y5_AWARE ~~                                                           
    ##    .y5_DISTRST        3.443    0.608    5.658    0.000    3.443    0.244
    ##    .y5_INEFCT         8.246    1.100    7.500    0.000    8.246    0.511
    ##    .y5_SELWT         -0.762    0.113   -6.739    0.000   -0.762   -0.276
    ##  .y5_DISTRST ~~                                                         
    ##    .y5_INEFCT         5.046    0.646    7.811    0.000    5.046    0.440
    ##    .y5_SELWT         -0.561    0.082   -6.831    0.000   -0.561   -0.286
    ##  .y5_INEFCT ~~                                                          
    ##    .y5_SELWT         -1.229    0.120  -10.223    0.000   -1.229   -0.549
    ##  .y2_STRESS ~~                                                          
    ##    .y0_QI             0.000                               0.000    0.000
    ##  .y0_pbmi ~~                                                            
    ##    .y0_CATINC         0.000                               0.000    0.000
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .y10_WAISTUMB     93.587    6.068   15.424    0.000   93.587    0.394
    ##    .y5_AWARE         19.873    1.742   11.408    0.000   19.873    0.916
    ##    .y5_DISTRST       10.018    0.713   14.059    0.000   10.018    0.896
    ##    .y5_INEFCT        13.099    1.169   11.205    0.000   13.099    0.898
    ##    .y5_SELWT          0.383    0.020   18.853    0.000    0.383    0.903
    ##    .y2_STRESS        46.316    2.530   18.309    0.000   46.316    0.973
    ##    .y0_QI            14.079    0.870   16.186    0.000   14.079    0.899
    ##    .y0_pbmi          46.404    4.188   11.080    0.000   46.404    0.954
    ##    .y0_CATINC         1.102    0.043   25.847    0.000    1.102    0.916

Anxiety Psych Pruned Model - WAIST UMB
--------------------------------------

``` r
mod_psych_anx_pruned_wu <-
  '
    #################
    ## Regressions ##
    #################  
     
    y10_WAISTUMB ~ y0_CATINC + y0_QI + y0_pbmi + y0_RACE  

    y10_WAISTUMB ~ y5_AWARE + y5_DISTRST + y5_INEFCT + y5_SELWT 

    y5_AWARE + y5_DISTRST + y5_INEFCT + y5_SELWT ~ y3_ANXR 

    y5_AWARE + y5_DISTRST + y5_INEFCT + y3_ANXR ~ y0_CATINC + y0_pbmi

    y5_AWARE + y5_DISTRST + y5_INEFCT ~ y0_RACE

    y5_AWARE + y5_INEFCT + y5_SELWT ~ y0_QI 

    y5_SELWT ~ y0_pbmi + y0_CATINC + y0_RACE

    y0_QI ~ y0_pbmi + 0*y0_CATINC + y0_RACE

    y0_pbmi + y0_CATINC ~ y0_RACE

    
    #################
    ## Covariances ##
    #################    

    y5_AWARE ~~ y5_DISTRST + y5_INEFCT + y5_SELWT 

    y5_DISTRST ~~ y5_INEFCT + y5_SELWT 

    y5_INEFCT ~~ y5_SELWT 

    y3_ANXR ~~ 0*y0_QI + 0*y0_RACE   

    y0_pbmi ~~ 0*y0_CATINC

  '


fit_psych_anx_pruned_wu <-
  sem(
    mod_psych_anx_pruned_wu, 
    data = semTestingData, 
    estimator = "MLM"
  )

summary(
  fit_psych_anx_pruned_wu, 
  standardized = TRUE, 
  fit.measures = TRUE
)
```

    ## lavaan 0.6-3 ended normally after 157 iterations
    ## 
    ##   Optimization method                           NLMINB
    ##   Number of free parameters                         49
    ## 
    ##   Number of observations                           793
    ## 
    ##   Estimator                                         ML      Robust
    ##   Model Fit Test Statistic                      10.163       9.436
    ##   Degrees of freedom                                 6           6
    ##   P-value (Chi-square)                           0.118       0.150
    ##   Scaling correction factor                                  1.077
    ##     for the Satorra-Bentler correction
    ## 
    ## Model test baseline model:
    ## 
    ##   Minimum Function Test Statistic             2130.299    1711.308
    ##   Degrees of freedom                                45          45
    ##   P-value                                        0.000       0.000
    ## 
    ## User model versus baseline model:
    ## 
    ##   Comparative Fit Index (CFI)                    0.998       0.998
    ##   Tucker-Lewis Index (TLI)                       0.985       0.985
    ## 
    ##   Robust Comparative Fit Index (CFI)                         0.998
    ##   Robust Tucker-Lewis Index (TLI)                            0.987
    ## 
    ## Loglikelihood and Information Criteria:
    ## 
    ##   Loglikelihood user model (H0)             -18899.338  -18899.338
    ##   Loglikelihood unrestricted model (H1)     -18894.257  -18894.257
    ## 
    ##   Number of free parameters                         49          49
    ##   Akaike (AIC)                               37896.677   37896.677
    ##   Bayesian (BIC)                             38125.792   38125.792
    ##   Sample-size adjusted Bayesian (BIC)        37970.191   37970.191
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.030       0.027
    ##   90 Percent Confidence Interval          0.000  0.060       0.000  0.057
    ##   P-value RMSEA <= 0.05                          0.847       0.887
    ## 
    ##   Robust RMSEA                                               0.028
    ##   90 Percent Confidence Interval                             0.000  0.060
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.016       0.016
    ## 
    ## Parameter Estimates:
    ## 
    ##   Information                                 Expected
    ##   Information saturated (h1) model          Structured
    ##   Standard Errors                           Robust.sem
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   y10_WAISTUMB ~                                                        
    ##     y0_CATINC        -0.893    0.353   -2.534    0.011   -0.893   -0.064
    ##     y0_QI             2.865    0.108   26.510    0.000    2.865    0.736
    ##     y0_pbmi           0.161    0.054    2.991    0.003    0.161    0.073
    ##     y0_RACE          -1.756    0.802   -2.189    0.029   -1.756   -0.057
    ##     y5_AWARE          0.089    0.095    0.939    0.348    0.089    0.027
    ##     y5_DISTRST        0.358    0.138    2.590    0.010    0.358    0.078
    ##     y5_INEFCT        -0.078    0.159   -0.489    0.625   -0.078   -0.019
    ##     y5_SELWT         -1.135    0.666   -1.706    0.088   -1.135   -0.048
    ##   y5_AWARE ~                                                            
    ##     y3_ANXR           0.253    0.025   10.087    0.000    0.253    0.351
    ##   y5_DISTRST ~                                                          
    ##     y3_ANXR           0.118    0.017    6.794    0.000    0.118    0.227
    ##   y5_INEFCT ~                                                           
    ##     y3_ANXR           0.204    0.021    9.603    0.000    0.204    0.346
    ##   y5_SELWT ~                                                            
    ##     y3_ANXR          -0.040    0.003  -11.968    0.000   -0.040   -0.398
    ##   y5_AWARE ~                                                            
    ##     y0_CATINC        -0.458    0.148   -3.099    0.002   -0.458   -0.108
    ##     y0_pbmi          -0.001    0.024   -0.047    0.963   -0.001   -0.002
    ##   y5_DISTRST ~                                                          
    ##     y0_CATINC        -0.278    0.105   -2.632    0.008   -0.278   -0.091
    ##     y0_pbmi           0.029    0.015    1.925    0.054    0.029    0.060
    ##   y5_INEFCT ~                                                           
    ##     y0_CATINC        -0.364    0.124   -2.944    0.003   -0.364   -0.105
    ##     y0_pbmi           0.038    0.021    1.792    0.073    0.038    0.070
    ##   y3_ANXR ~                                                             
    ##     y0_CATINC        -0.618    0.202   -3.061    0.002   -0.618   -0.105
    ##     y0_pbmi           0.110    0.029    3.773    0.000    0.110    0.119
    ##   y5_AWARE ~                                                            
    ##     y0_RACE           0.984    0.307    3.208    0.001    0.984    0.105
    ##   y5_DISTRST ~                                                          
    ##     y0_RACE           1.477    0.234    6.301    0.000    1.477    0.220
    ##   y5_INEFCT ~                                                           
    ##     y0_RACE          -0.148    0.279   -0.530    0.596   -0.148   -0.019
    ##   y5_AWARE ~                                                            
    ##     y0_QI             0.110    0.043    2.588    0.010    0.110    0.094
    ##   y5_INEFCT ~                                                           
    ##     y0_QI             0.079    0.038    2.048    0.041    0.079    0.082
    ##   y5_SELWT ~                                                            
    ##     y0_QI            -0.014    0.006   -2.267    0.023   -0.014   -0.084
    ##     y0_pbmi          -0.005    0.003   -1.482    0.138   -0.005   -0.054
    ##     y0_CATINC         0.017    0.019    0.881    0.378    0.017    0.029
    ##     y0_RACE           0.165    0.044    3.728    0.000    0.165    0.127
    ##   y0_QI ~                                                               
    ##     y0_pbmi           0.164    0.027    6.068    0.000    0.164    0.289
    ##     y0_CATINC         0.000                               0.000    0.000
    ##     y0_RACE           0.684    0.270    2.535    0.011    0.684    0.086
    ##   y0_pbmi ~                                                             
    ##     y0_RACE           2.981    0.480    6.205    0.000    2.981    0.213
    ##   y0_CATINC ~                                                           
    ##     y0_RACE          -0.637    0.074   -8.618    0.000   -0.637   -0.290
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##  .y5_AWARE ~~                                                           
    ##    .y5_DISTRST        2.530    0.562    4.499    0.000    2.530    0.193
    ##    .y5_INEFCT         6.813    0.948    7.185    0.000    6.813    0.462
    ##    .y5_SELWT         -0.469    0.100   -4.679    0.000   -0.469   -0.190
    ##  .y5_DISTRST ~~                                                         
    ##    .y5_INEFCT         4.432    0.603    7.355    0.000    4.432    0.409
    ##    .y5_SELWT         -0.434    0.080   -5.428    0.000   -0.434   -0.239
    ##  .y5_INEFCT ~~                                                          
    ##    .y5_SELWT         -1.040    0.105   -9.859    0.000   -1.040   -0.508
    ##  .y3_ANXR ~~                                                            
    ##    .y0_QI             0.000                               0.000    0.000
    ##     y0_RACE           0.000                               0.000    0.000
    ##  .y0_pbmi ~~                                                            
    ##    .y0_CATINC         0.000                               0.000    0.000
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .y10_WAISTUMB     93.587    6.068   15.424    0.000   93.587    0.395
    ##    .y5_AWARE         17.820    1.563   11.401    0.000   17.820    0.820
    ##    .y5_DISTRST        9.617    0.688   13.979    0.000    9.617    0.856
    ##    .y5_INEFCT        12.215    1.075   11.368    0.000   12.215    0.839
    ##    .y5_SELWT          0.343    0.018   19.269    0.000    0.343    0.814
    ##    .y3_ANXR          40.720    1.682   24.203    0.000   40.720    0.973
    ##    .y0_QI            14.079    0.870   16.186    0.000   14.079    0.899
    ##    .y0_pbmi          46.404    4.188   11.080    0.000   46.404    0.954
    ##    .y0_CATINC         1.102    0.043   25.847    0.000    1.102    0.916
    ##     y0_RACE           0.249    0.001  285.949    0.000    0.249    1.000

Stress Psych Pruned Model - SUMSKIN
-----------------------------------

``` r
mod_pysch_stress_pruned_ss <-
  '
    ######################
    ## Latent Variables ##
    ######################

    #################
    ## Regressions ##
    #################  
     
    y10_SUMSKIN ~ y0_CATINC + y0_SUMSKIN + y0_pbmi + y0_RACE  

    y10_SUMSKIN ~ y5_AWARE + y5_DISTRST + y5_INEFCT + y5_SELWT

    y5_AWARE + y5_DISTRST + y5_INEFCT + y5_SELWT ~ y2_STRESS 

    y5_AWARE + y5_DISTRST + y5_INEFCT + y2_STRESS ~ y0_CATINC + y0_RACE + y0_pbmi

    y5_AWARE + y5_INEFCT + y5_SELWT ~ y0_SUMSKIN 

    y5_SELWT ~ y0_pbmi + y0_CATINC + y0_RACE

    y0_SUMSKIN ~ y0_pbmi + 0*y0_CATINC + y0_RACE

    y0_pbmi + y0_CATINC ~ y0_RACE

    
    #################
    ## Covariances ##
    #################    

    y5_AWARE ~~ y5_DISTRST + y5_INEFCT + y5_SELWT 

    y5_DISTRST ~~ y5_INEFCT + y5_SELWT 

    y5_INEFCT ~~ y5_SELWT 

    y2_STRESS ~~ 0*y0_SUMSKIN   

    y0_pbmi ~~ 0*y0_CATINC
  
  '

fit_psych_stress_pruned_ss <- 
  sem(
    mod_pysch_stress_pruned_ss, 
    data = semTestingData, 
    estimator = "MLM"
  )
```

    ## Warning in lav_data_full(data = data, group = group, cluster = cluster, :
    ## lavaan WARNING: some observed variances are (at least) a factor 1000 times
    ## larger than others; use varTable(fit) to investigate

``` r
summary(
  fit_psych_stress_pruned_ss, 
  standardized = TRUE, 
  fit.measures = TRUE
)
```

    ## lavaan 0.6-3 ended normally after 185 iterations
    ## 
    ##   Optimization method                           NLMINB
    ##   Number of free parameters                         49
    ## 
    ##   Number of observations                           793
    ## 
    ##   Estimator                                         ML      Robust
    ##   Model Fit Test Statistic                       8.574       7.882
    ##   Degrees of freedom                                 5           5
    ##   P-value (Chi-square)                           0.127       0.163
    ##   Scaling correction factor                                  1.088
    ##     for the Satorra-Bentler correction
    ## 
    ## Model test baseline model:
    ## 
    ##   Minimum Function Test Statistic             1913.007    1545.750
    ##   Degrees of freedom                                45          45
    ##   P-value                                        0.000       0.000
    ## 
    ## User model versus baseline model:
    ## 
    ##   Comparative Fit Index (CFI)                    0.998       0.998
    ##   Tucker-Lewis Index (TLI)                       0.983       0.983
    ## 
    ##   Robust Comparative Fit Index (CFI)                         0.998
    ##   Robust Tucker-Lewis Index (TLI)                            0.985
    ## 
    ## Loglikelihood and Information Criteria:
    ## 
    ##   Loglikelihood user model (H0)             -20341.731  -20341.731
    ##   Loglikelihood unrestricted model (H1)     -20337.444  -20337.444
    ## 
    ##   Number of free parameters                         49          49
    ##   Akaike (AIC)                               40781.461   40781.461
    ##   Bayesian (BIC)                             41010.577   41010.577
    ##   Sample-size adjusted Bayesian (BIC)        40854.975   40854.975
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.030       0.027
    ##   90 Percent Confidence Interval          0.000  0.063       0.000  0.060
    ##   P-value RMSEA <= 0.05                          0.813       0.861
    ## 
    ##   Robust RMSEA                                               0.028
    ##   90 Percent Confidence Interval                             0.000  0.064
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.015       0.015
    ## 
    ## Parameter Estimates:
    ## 
    ##   Information                                 Expected
    ##   Information saturated (h1) model          Structured
    ##   Standard Errors                           Robust.sem
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   y10_SUMSKIN ~                                                         
    ##     y0_CATINC        -2.366    0.746   -3.171    0.002   -2.366   -0.081
    ##     y0_SUMSKIN        1.119    0.045   25.084    0.000    1.119    0.693
    ##     y0_pbmi           0.628    0.111    5.629    0.000    0.628    0.137
    ##     y0_RACE           1.639    1.713    0.957    0.339    1.639    0.026
    ##     y5_AWARE          0.046    0.212    0.217    0.828    0.046    0.007
    ##     y5_DISTRST        0.771    0.334    2.310    0.021    0.771    0.081
    ##     y5_INEFCT        -0.179    0.352   -0.508    0.611   -0.179   -0.021
    ##     y5_SELWT         -0.747    1.456   -0.513    0.608   -0.747   -0.015
    ##   y5_AWARE ~                                                            
    ##     y2_STRESS         0.107    0.022    4.812    0.000    0.107    0.159
    ##   y5_DISTRST ~                                                          
    ##     y2_STRESS         0.059    0.019    3.200    0.001    0.059    0.122
    ##   y5_INEFCT ~                                                           
    ##     y2_STRESS         0.131    0.021    6.239    0.000    0.131    0.237
    ##   y5_SELWT ~                                                            
    ##     y2_STRESS        -0.023    0.003   -6.990    0.000   -0.023   -0.244
    ##   y5_AWARE ~                                                            
    ##     y0_CATINC        -0.542    0.163   -3.322    0.001   -0.542   -0.128
    ##     y0_RACE           1.014    0.330    3.076    0.002    1.014    0.109
    ##     y0_pbmi           0.021    0.023    0.885    0.376    0.021    0.031
    ##   y5_DISTRST ~                                                          
    ##     y0_CATINC        -0.309    0.112   -2.766    0.006   -0.309   -0.101
    ##     y0_RACE           1.458    0.242    6.028    0.000    1.458    0.218
    ##     y0_pbmi           0.037    0.016    2.321    0.020    0.037    0.078
    ##   y5_INEFCT ~                                                           
    ##     y0_CATINC        -0.392    0.130   -3.013    0.003   -0.392   -0.113
    ##     y0_RACE          -0.086    0.282   -0.306    0.759   -0.086   -0.011
    ##     y0_pbmi           0.052    0.023    2.264    0.024    0.052    0.095
    ##   y2_STRESS ~                                                           
    ##     y0_CATINC        -0.865    0.237   -3.641    0.000   -0.865   -0.137
    ##     y0_RACE          -1.014    0.522   -1.943    0.052   -1.014   -0.073
    ##     y0_pbmi           0.099    0.035    2.779    0.005    0.099    0.100
    ##   y5_AWARE ~                                                            
    ##     y0_SUMSKIN        0.021    0.009    2.343    0.019    0.021    0.088
    ##   y5_INEFCT ~                                                           
    ##     y0_SUMSKIN        0.014    0.007    1.907    0.057    0.014    0.073
    ##   y5_SELWT ~                                                            
    ##     y0_SUMSKIN       -0.004    0.001   -2.877    0.004   -0.004   -0.109
    ##     y0_pbmi          -0.007    0.004   -1.907    0.057   -0.007   -0.076
    ##     y0_CATINC         0.026    0.020    1.266    0.205    0.026    0.043
    ##     y0_RACE           0.157    0.047    3.342    0.001    0.157    0.121
    ##   y0_SUMSKIN ~                                                          
    ##     y0_pbmi           0.762    0.121    6.275    0.000    0.762    0.268
    ##     y0_CATINC         0.000                               0.000    0.000
    ##     y0_RACE          -0.848    1.385   -0.613    0.540   -0.848   -0.021
    ##   y0_pbmi ~                                                             
    ##     y0_RACE           2.981    0.480    6.205    0.000    2.981    0.213
    ##   y0_CATINC ~                                                           
    ##     y0_RACE          -0.637    0.074   -8.618    0.000   -0.637   -0.290
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##  .y5_AWARE ~~                                                           
    ##    .y5_DISTRST        3.419    0.611    5.600    0.000    3.419    0.242
    ##    .y5_INEFCT         8.279    1.103    7.504    0.000    8.279    0.512
    ##    .y5_SELWT         -0.761    0.114   -6.694    0.000   -0.761   -0.276
    ##  .y5_DISTRST ~~                                                         
    ##    .y5_INEFCT         5.030    0.646    7.788    0.000    5.030    0.439
    ##    .y5_SELWT         -0.556    0.082   -6.748    0.000   -0.556   -0.285
    ##  .y5_INEFCT ~~                                                          
    ##    .y5_SELWT         -1.227    0.120  -10.253    0.000   -1.227   -0.549
    ##  .y2_STRESS ~~                                                          
    ##    .y0_SUMSKIN        0.000                               0.000    0.000
    ##  .y0_pbmi ~~                                                            
    ##    .y0_CATINC         0.000                               0.000    0.000
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .y10_SUMSKIN     430.798   33.382   12.905    0.000  430.798    0.421
    ##    .y5_AWARE         19.927    1.750   11.386    0.000   19.927    0.919
    ##    .y5_DISTRST       10.018    0.713   14.059    0.000   10.018    0.896
    ##    .y5_INEFCT        13.119    1.172   11.199    0.000   13.119    0.901
    ##    .y5_SELWT          0.381    0.020   18.878    0.000    0.381    0.901
    ##    .y2_STRESS        46.316    2.530   18.309    0.000   46.316    0.973
    ##    .y0_SUMSKIN      365.162   22.111   16.515    0.000  365.162    0.930
    ##    .y0_pbmi          46.404    4.188   11.080    0.000   46.404    0.954
    ##    .y0_CATINC         1.102    0.043   25.847    0.000    1.102    0.916

Anxiety Psych Pruned Model - SUMSKIN
------------------------------------

``` r
mod_psych_anx_pruned_ss <-
  '
    #################
    ## Regressions ##
    #################  
     
    y10_SUMSKIN ~ y0_CATINC + y0_SUMSKIN + y0_pbmi + y0_RACE  

    y10_SUMSKIN ~ y5_AWARE + y5_DISTRST + y5_INEFCT + y5_SELWT 

    y5_AWARE + y5_DISTRST + y5_INEFCT + y5_SELWT ~ y3_ANXR 

    y5_AWARE + y5_DISTRST + y5_INEFCT + y3_ANXR ~ y0_CATINC + y0_pbmi

    y5_AWARE + y5_DISTRST + y5_INEFCT ~ y0_RACE

    y5_AWARE + y5_INEFCT + y5_SELWT ~ y0_SUMSKIN 

    y5_SELWT ~ y0_pbmi + y0_CATINC + y0_RACE

    y0_SUMSKIN ~ y0_pbmi + 0*y0_CATINC + y0_RACE

    y0_pbmi + y0_CATINC ~ y0_RACE

    
    #################
    ## Covariances ##
    #################    

    y5_AWARE ~~ y5_DISTRST + y5_INEFCT + y5_SELWT 

    y5_DISTRST ~~ y5_INEFCT + y5_SELWT 

    y5_INEFCT ~~ y5_SELWT 

    y3_ANXR ~~ 0*y0_SUMSKIN + 0*y0_RACE   

    y0_pbmi ~~ 0*y0_CATINC

  '


fit_psych_anx_pruned_ss <-
  sem(
    mod_psych_anx_pruned_ss, 
    data = semTestingData, 
    estimator = "MLM"
  )
```

    ## Warning in lav_data_full(data = data, group = group, cluster = cluster, :
    ## lavaan WARNING: some observed variances are (at least) a factor 1000 times
    ## larger than others; use varTable(fit) to investigate

``` r
summary(
  fit_psych_anx_pruned_ss, 
  standardized = TRUE, 
  fit.measures = TRUE
)
```

    ## lavaan 0.6-3 ended normally after 176 iterations
    ## 
    ##   Optimization method                           NLMINB
    ##   Number of free parameters                         49
    ## 
    ##   Number of observations                           793
    ## 
    ##   Estimator                                         ML      Robust
    ##   Model Fit Test Statistic                      14.525      13.428
    ##   Degrees of freedom                                 6           6
    ##   P-value (Chi-square)                           0.024       0.037
    ##   Scaling correction factor                                  1.082
    ##     for the Satorra-Bentler correction
    ## 
    ## Model test baseline model:
    ## 
    ##   Minimum Function Test Statistic             2055.588    1683.678
    ##   Degrees of freedom                                45          45
    ##   P-value                                        0.000       0.000
    ## 
    ## User model versus baseline model:
    ## 
    ##   Comparative Fit Index (CFI)                    0.996       0.995
    ##   Tucker-Lewis Index (TLI)                       0.968       0.966
    ## 
    ##   Robust Comparative Fit Index (CFI)                         0.996
    ##   Robust Tucker-Lewis Index (TLI)                            0.970
    ## 
    ## Loglikelihood and Information Criteria:
    ## 
    ##   Loglikelihood user model (H0)             -20796.759  -20796.759
    ##   Loglikelihood unrestricted model (H1)     -20789.497  -20789.497
    ## 
    ##   Number of free parameters                         49          49
    ##   Akaike (AIC)                               41691.519   41691.519
    ##   Bayesian (BIC)                             41920.634   41920.634
    ##   Sample-size adjusted Bayesian (BIC)        41765.033   41765.033
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.042       0.040
    ##   90 Percent Confidence Interval          0.014  0.071       0.011  0.067
    ##   P-value RMSEA <= 0.05                          0.631       0.701
    ## 
    ##   Robust RMSEA                                               0.041
    ##   90 Percent Confidence Interval                             0.010  0.071
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.021       0.021
    ## 
    ## Parameter Estimates:
    ## 
    ##   Information                                 Expected
    ##   Information saturated (h1) model          Structured
    ##   Standard Errors                           Robust.sem
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   y10_SUMSKIN ~                                                         
    ##     y0_CATINC        -2.366    0.745   -3.176    0.001   -2.366   -0.081
    ##     y0_SUMSKIN        1.119    0.044   25.279    0.000    1.119    0.693
    ##     y0_pbmi           0.628    0.111    5.633    0.000    0.628    0.137
    ##     y0_RACE           1.639    1.710    0.959    0.338    1.639    0.026
    ##     y5_AWARE          0.046    0.211    0.217    0.828    0.046    0.007
    ##     y5_DISTRST        0.771    0.333    2.313    0.021    0.771    0.081
    ##     y5_INEFCT        -0.179    0.352   -0.508    0.611   -0.179   -0.021
    ##     y5_SELWT         -0.747    1.449   -0.515    0.606   -0.747   -0.015
    ##   y5_AWARE ~                                                            
    ##     y3_ANXR           0.252    0.025   10.008    0.000    0.252    0.350
    ##   y5_DISTRST ~                                                          
    ##     y3_ANXR           0.118    0.017    6.794    0.000    0.118    0.227
    ##   y5_INEFCT ~                                                           
    ##     y3_ANXR           0.204    0.021    9.553    0.000    0.204    0.345
    ##   y5_SELWT ~                                                            
    ##     y3_ANXR          -0.040    0.003  -11.863    0.000   -0.040   -0.396
    ##   y5_AWARE ~                                                            
    ##     y0_CATINC        -0.455    0.147   -3.084    0.002   -0.455   -0.107
    ##     y0_pbmi           0.005    0.023    0.212    0.832    0.005    0.007
    ##   y5_DISTRST ~                                                          
    ##     y0_CATINC        -0.278    0.105   -2.632    0.008   -0.278   -0.091
    ##     y0_pbmi           0.029    0.015    1.925    0.054    0.029    0.060
    ##   y5_INEFCT ~                                                           
    ##     y0_CATINC        -0.362    0.123   -2.933    0.003   -0.362   -0.104
    ##     y0_pbmi           0.042    0.021    1.973    0.048    0.042    0.077
    ##   y3_ANXR ~                                                             
    ##     y0_CATINC        -0.618    0.202   -3.061    0.002   -0.618   -0.105
    ##     y0_pbmi           0.110    0.029    3.773    0.000    0.110    0.119
    ##   y5_AWARE ~                                                            
    ##     y0_RACE           1.074    0.305    3.519    0.000    1.074    0.115
    ##   y5_DISTRST ~                                                          
    ##     y0_RACE           1.477    0.234    6.301    0.000    1.477    0.220
    ##   y5_INEFCT ~                                                           
    ##     y0_RACE          -0.083    0.275   -0.302    0.763   -0.083   -0.011
    ##   y5_AWARE ~                                                            
    ##     y0_SUMSKIN        0.016    0.008    1.897    0.058    0.016    0.068
    ##   y5_INEFCT ~                                                           
    ##     y0_SUMSKIN        0.012    0.007    1.611    0.107    0.012    0.061
    ##   y5_SELWT ~                                                            
    ##     y0_SUMSKIN       -0.003    0.001   -2.460    0.014   -0.003   -0.090
    ##     y0_pbmi          -0.005    0.003   -1.525    0.127   -0.005   -0.055
    ##     y0_CATINC         0.018    0.019    0.912    0.362    0.018    0.030
    ##     y0_RACE           0.154    0.044    3.474    0.001    0.154    0.119
    ##   y0_SUMSKIN ~                                                          
    ##     y0_pbmi           0.762    0.121    6.275    0.000    0.762    0.268
    ##     y0_CATINC         0.000                               0.000    0.000
    ##     y0_RACE          -0.848    1.385   -0.613    0.540   -0.848   -0.021
    ##   y0_pbmi ~                                                             
    ##     y0_RACE           2.981    0.480    6.205    0.000    2.981    0.213
    ##   y0_CATINC ~                                                           
    ##     y0_RACE          -0.637    0.074   -8.618    0.000   -0.637   -0.290
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##  .y5_AWARE ~~                                                           
    ##    .y5_DISTRST        2.514    0.565    4.449    0.000    2.514    0.192
    ##    .y5_INEFCT         6.857    0.950    7.218    0.000    6.857    0.463
    ##    .y5_SELWT         -0.472    0.101   -4.690    0.000   -0.472   -0.191
    ##  .y5_DISTRST ~~                                                         
    ##    .y5_INEFCT         4.420    0.603    7.329    0.000    4.420    0.407
    ##    .y5_SELWT         -0.431    0.080   -5.373    0.000   -0.431   -0.238
    ##  .y5_INEFCT ~~                                                          
    ##    .y5_SELWT         -1.041    0.105   -9.934    0.000   -1.041   -0.509
    ##  .y3_ANXR ~~                                                            
    ##    .y0_SUMSKIN        0.000                               0.000    0.000
    ##     y0_RACE           0.000                               0.000    0.000
    ##  .y0_pbmi ~~                                                            
    ##    .y0_CATINC         0.000                               0.000    0.000
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .y10_SUMSKIN     430.798   33.382   12.905    0.000  430.798    0.421
    ##    .y5_AWARE         17.889    1.570   11.395    0.000   17.889    0.824
    ##    .y5_DISTRST        9.617    0.688   13.979    0.000    9.617    0.856
    ##    .y5_INEFCT        12.242    1.079   11.348    0.000   12.242    0.842
    ##    .y5_SELWT          0.342    0.018   19.342    0.000    0.342    0.815
    ##    .y3_ANXR          40.720    1.682   24.203    0.000   40.720    0.973
    ##    .y0_SUMSKIN      365.162   22.111   16.515    0.000  365.162    0.930
    ##    .y0_pbmi          46.404    4.188   11.080    0.000   46.404    0.954
    ##    .y0_CATINC         1.102    0.043   25.847    0.000    1.102    0.916
    ##     y0_RACE           0.249    0.001  285.949    0.000    0.249    1.000

Stress Psych Pruned Model - ARMCIRAV
------------------------------------

``` r
mod_pysch_stress_pruned_ac <-
  '
    ######################
    ## Latent Variables ##
    ######################

    #################
    ## Regressions ##
    #################  
     
    y10_ARMCIRAV ~ y0_CATINC + y0_ARMCIRAV + y0_pbmi + y0_RACE  

    y10_ARMCIRAV ~ y5_AWARE + y5_DISTRST + y5_INEFCT + y5_SELWT

    y5_AWARE + y5_DISTRST + y5_INEFCT + y5_SELWT ~ y2_STRESS 

    y5_AWARE + y5_DISTRST + y5_INEFCT + y2_STRESS ~ y0_CATINC + y0_RACE + y0_pbmi

    y5_AWARE + y5_INEFCT + y5_SELWT ~ y0_ARMCIRAV 

    y5_SELWT ~ y0_pbmi + y0_CATINC + y0_RACE

    y0_ARMCIRAV ~ y0_pbmi + 0*y0_CATINC + y0_RACE

    y0_pbmi + y0_CATINC ~ y0_RACE

    
    #################
    ## Covariances ##
    #################    

    y5_AWARE ~~ y5_DISTRST + y5_INEFCT + y5_SELWT 

    y5_DISTRST ~~ y5_INEFCT + y5_SELWT 

    y5_INEFCT ~~ y5_SELWT 

    y2_STRESS ~~ 0*y0_ARMCIRAV   

    y0_pbmi ~~ 0*y0_CATINC
  
  '

fit_psych_stress_pruned_ac <- 
  sem(
    mod_pysch_stress_pruned_ac, 
    data = semTestingData, 
    estimator = "MLM"
  )

summary(
  fit_psych_stress_pruned_ac, 
  standardized = TRUE, 
  fit.measures = TRUE
)
```

    ## lavaan 0.6-3 ended normally after 141 iterations
    ## 
    ##   Optimization method                           NLMINB
    ##   Number of free parameters                         49
    ## 
    ##   Number of observations                           793
    ## 
    ##   Estimator                                         ML      Robust
    ##   Model Fit Test Statistic                       6.587       6.185
    ##   Degrees of freedom                                 5           5
    ##   P-value (Chi-square)                           0.253       0.289
    ##   Scaling correction factor                                  1.065
    ##     for the Satorra-Bentler correction
    ## 
    ## Model test baseline model:
    ## 
    ##   Minimum Function Test Statistic             2010.089    1630.677
    ##   Degrees of freedom                                45          45
    ##   P-value                                        0.000       0.000
    ## 
    ## User model versus baseline model:
    ## 
    ##   Comparative Fit Index (CFI)                    0.999       0.999
    ##   Tucker-Lewis Index (TLI)                       0.993       0.993
    ## 
    ##   Robust Comparative Fit Index (CFI)                         0.999
    ##   Robust Tucker-Lewis Index (TLI)                            0.994
    ## 
    ## Loglikelihood and Information Criteria:
    ## 
    ##   Loglikelihood user model (H0)             -17568.445  -17568.445
    ##   Loglikelihood unrestricted model (H1)     -17565.151  -17565.151
    ## 
    ##   Number of free parameters                         49          49
    ##   Akaike (AIC)                               35234.889   35234.889
    ##   Bayesian (BIC)                             35464.005   35464.005
    ##   Sample-size adjusted Bayesian (BIC)        35308.404   35308.404
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.020       0.017
    ##   90 Percent Confidence Interval          0.000  0.056       0.000  0.053
    ##   P-value RMSEA <= 0.05                          0.903       0.926
    ## 
    ##   Robust RMSEA                                               0.018
    ##   90 Percent Confidence Interval                             0.000  0.056
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.011       0.011
    ## 
    ## Parameter Estimates:
    ## 
    ##   Information                                 Expected
    ##   Information saturated (h1) model          Structured
    ##   Standard Errors                           Robust.sem
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   y10_ARMCIRAV ~                                                        
    ##     y0_CATINC        -0.169    0.125   -1.346    0.178   -0.169   -0.033
    ##     y0_ARMCIRAV       1.116    0.038   29.175    0.000    1.116    0.729
    ##     y0_pbmi           0.101    0.020    4.975    0.000    0.101    0.125
    ##     y0_RACE          -0.117    0.280   -0.416    0.677   -0.117   -0.010
    ##     y5_AWARE          0.031    0.033    0.948    0.343    0.031    0.026
    ##     y5_DISTRST        0.132    0.052    2.550    0.011    0.132    0.079
    ##     y5_INEFCT        -0.046    0.053   -0.865    0.387   -0.046   -0.031
    ##     y5_SELWT         -0.279    0.236   -1.183    0.237   -0.279   -0.032
    ##   y5_AWARE ~                                                            
    ##     y2_STRESS         0.109    0.022    4.885    0.000    0.109    0.161
    ##   y5_DISTRST ~                                                          
    ##     y2_STRESS         0.059    0.019    3.200    0.001    0.059    0.122
    ##   y5_INEFCT ~                                                           
    ##     y2_STRESS         0.132    0.021    6.285    0.000    0.132    0.239
    ##   y5_SELWT ~                                                            
    ##     y2_STRESS        -0.023    0.003   -7.084    0.000   -0.023   -0.247
    ##   y5_AWARE ~                                                            
    ##     y0_CATINC        -0.547    0.163   -3.355    0.001   -0.547   -0.129
    ##     y0_RACE           0.945    0.329    2.873    0.004    0.945    0.101
    ##     y0_pbmi           0.017    0.024    0.697    0.486    0.017    0.025
    ##   y5_DISTRST ~                                                          
    ##     y0_CATINC        -0.309    0.112   -2.766    0.006   -0.309   -0.101
    ##     y0_RACE           1.458    0.242    6.028    0.000    1.458    0.218
    ##     y0_pbmi           0.037    0.016    2.321    0.020    0.037    0.078
    ##   y5_INEFCT ~                                                           
    ##     y0_CATINC        -0.394    0.130   -3.019    0.003   -0.394   -0.113
    ##     y0_RACE          -0.128    0.284   -0.453    0.651   -0.128   -0.017
    ##     y0_pbmi           0.050    0.023    2.181    0.029    0.050    0.092
    ##   y2_STRESS ~                                                           
    ##     y0_CATINC        -0.865    0.237   -3.641    0.000   -0.865   -0.137
    ##     y0_RACE          -1.014    0.522   -1.943    0.052   -1.014   -0.073
    ##     y0_pbmi           0.099    0.035    2.779    0.005    0.099    0.100
    ##   y5_AWARE ~                                                            
    ##     y0_ARMCIRAV       0.136    0.047    2.893    0.004    0.136    0.106
    ##   y5_INEFCT ~                                                           
    ##     y0_ARMCIRAV       0.082    0.040    2.063    0.039    0.082    0.079
    ##   y5_SELWT ~                                                            
    ##     y0_ARMCIRAV      -0.019    0.007   -2.827    0.005   -0.019   -0.109
    ##     y0_pbmi          -0.007    0.004   -1.838    0.066   -0.007   -0.074
    ##     y0_CATINC         0.026    0.020    1.270    0.204    0.026    0.044
    ##     y0_RACE           0.167    0.047    3.565    0.000    0.167    0.128
    ##   y0_ARMCIRAV ~                                                         
    ##     y0_pbmi           0.145    0.023    6.263    0.000    0.145    0.276
    ##     y0_CATINC         0.000                               0.000    0.000
    ##     y0_RACE           0.364    0.253    1.440    0.150    0.364    0.050
    ##   y0_pbmi ~                                                             
    ##     y0_RACE           2.981    0.480    6.205    0.000    2.981    0.213
    ##   y0_CATINC ~                                                           
    ##     y0_RACE          -0.637    0.074   -8.618    0.000   -0.637   -0.290
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##  .y5_AWARE ~~                                                           
    ##    .y5_DISTRST        3.462    0.609    5.684    0.000    3.462    0.245
    ##    .y5_INEFCT         8.280    1.097    7.546    0.000    8.280    0.512
    ##    .y5_SELWT         -0.761    0.113   -6.713    0.000   -0.761   -0.276
    ##  .y5_DISTRST ~~                                                         
    ##    .y5_INEFCT         5.058    0.646    7.827    0.000    5.058    0.441
    ##    .y5_SELWT         -0.563    0.082   -6.862    0.000   -0.563   -0.288
    ##  .y5_INEFCT ~~                                                          
    ##    .y5_SELWT         -1.231    0.120  -10.272    0.000   -1.231   -0.549
    ##  .y2_STRESS ~~                                                          
    ##    .y0_ARMCIRAV       0.000                               0.000    0.000
    ##  .y0_pbmi ~~                                                            
    ##    .y0_CATINC         0.000                               0.000    0.000
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .y10_ARMCIRAV     11.815    0.898   13.162    0.000   11.815    0.377
    ##    .y5_AWARE         19.886    1.733   11.475    0.000   19.886    0.915
    ##    .y5_DISTRST       10.018    0.713   14.059    0.000   10.018    0.896
    ##    .y5_INEFCT        13.137    1.171   11.222    0.000   13.137    0.899
    ##    .y5_SELWT          0.382    0.020   18.889    0.000    0.382    0.900
    ##    .y2_STRESS        46.316    2.530   18.309    0.000   46.316    0.973
    ##    .y0_ARMCIRAV      12.244    0.660   18.562    0.000   12.244    0.915
    ##    .y0_pbmi          46.404    4.188   11.080    0.000   46.404    0.954
    ##    .y0_CATINC         1.102    0.043   25.847    0.000    1.102    0.916

Anxiety Psych Pruned Model - ARMCIRAV
-------------------------------------

``` r
mod_psych_anx_pruned_ac <-
  '
    #################
    ## Regressions ##
    #################  
     
    y10_ARMCIRAV ~ y0_CATINC + y0_ARMCIRAV + y0_pbmi + y0_RACE  

    y10_ARMCIRAV ~ y5_AWARE + y5_DISTRST + y5_INEFCT + y5_SELWT 

    y5_AWARE + y5_DISTRST + y5_INEFCT + y5_SELWT ~ y3_ANXR 

    y5_AWARE + y5_DISTRST + y5_INEFCT + y3_ANXR ~ y0_CATINC + y0_pbmi

    y5_AWARE + y5_DISTRST + y5_INEFCT ~ y0_RACE

    y5_AWARE + y5_INEFCT + y5_SELWT ~ y0_ARMCIRAV 

    y5_SELWT ~ y0_pbmi + y0_CATINC + y0_RACE

    y0_ARMCIRAV ~ y0_pbmi + 0*y0_CATINC + y0_RACE

    y0_pbmi + y0_CATINC ~ y0_RACE

    
    #################
    ## Covariances ##
    #################    

    y5_AWARE ~~ y5_DISTRST + y5_INEFCT + y5_SELWT 

    y5_DISTRST ~~ y5_INEFCT + y5_SELWT 

    y5_INEFCT ~~ y5_SELWT 

    y3_ANXR ~~ 0*y0_ARMCIRAV + 0*y0_RACE   

    y0_pbmi ~~ 0*y0_CATINC

  '


fit_psych_anx_pruned_ac <-
  sem(
    mod_psych_anx_pruned_ac, 
    data = semTestingData, 
    estimator = "MLM"
  )

summary(
  fit_psych_anx_pruned_ac, 
  standardized = TRUE, 
  fit.measures = TRUE
)
```

    ## lavaan 0.6-3 ended normally after 125 iterations
    ## 
    ##   Optimization method                           NLMINB
    ##   Number of free parameters                         49
    ## 
    ##   Number of observations                           793
    ## 
    ##   Estimator                                         ML      Robust
    ##   Model Fit Test Statistic                      11.276      10.569
    ##   Degrees of freedom                                 6           6
    ##   P-value (Chi-square)                           0.080       0.103
    ##   Scaling correction factor                                  1.067
    ##     for the Satorra-Bentler correction
    ## 
    ## Model test baseline model:
    ## 
    ##   Minimum Function Test Statistic             2152.201    1766.740
    ##   Degrees of freedom                                45          45
    ##   P-value                                        0.000       0.000
    ## 
    ## User model versus baseline model:
    ## 
    ##   Comparative Fit Index (CFI)                    0.997       0.997
    ##   Tucker-Lewis Index (TLI)                       0.981       0.980
    ## 
    ##   Robust Comparative Fit Index (CFI)                         0.998
    ##   Robust Tucker-Lewis Index (TLI)                            0.983
    ## 
    ## Loglikelihood and Information Criteria:
    ## 
    ##   Loglikelihood user model (H0)             -18023.077  -18023.077
    ##   Loglikelihood unrestricted model (H1)     -18017.439  -18017.439
    ## 
    ##   Number of free parameters                         49          49
    ##   Akaike (AIC)                               36144.153   36144.153
    ##   Bayesian (BIC)                             36373.269   36373.269
    ##   Sample-size adjusted Bayesian (BIC)        36217.668   36217.668
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.033       0.031
    ##   90 Percent Confidence Interval          0.000  0.063       0.000  0.060
    ##   P-value RMSEA <= 0.05                          0.798       0.840
    ## 
    ##   Robust RMSEA                                               0.032
    ##   90 Percent Confidence Interval                             0.000  0.063
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.016       0.016
    ## 
    ## Parameter Estimates:
    ## 
    ##   Information                                 Expected
    ##   Information saturated (h1) model          Structured
    ##   Standard Errors                           Robust.sem
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   y10_ARMCIRAV ~                                                        
    ##     y0_CATINC        -0.169    0.125   -1.348    0.178   -0.169   -0.033
    ##     y0_ARMCIRAV       1.116    0.038   29.343    0.000    1.116    0.729
    ##     y0_pbmi           0.101    0.020    4.979    0.000    0.101    0.125
    ##     y0_RACE          -0.117    0.279   -0.417    0.676   -0.117   -0.010
    ##     y5_AWARE          0.031    0.033    0.949    0.343    0.031    0.026
    ##     y5_DISTRST        0.132    0.052    2.552    0.011    0.132    0.079
    ##     y5_INEFCT        -0.046    0.053   -0.865    0.387   -0.046   -0.031
    ##     y5_SELWT         -0.279    0.236   -1.185    0.236   -0.279   -0.032
    ##   y5_AWARE ~                                                            
    ##     y3_ANXR           0.253    0.025   10.057    0.000    0.253    0.351
    ##   y5_DISTRST ~                                                          
    ##     y3_ANXR           0.118    0.017    6.794    0.000    0.118    0.227
    ##   y5_INEFCT ~                                                           
    ##     y3_ANXR           0.205    0.021    9.593    0.000    0.205    0.347
    ##   y5_SELWT ~                                                            
    ##     y3_ANXR          -0.040    0.003  -11.941    0.000   -0.040   -0.398
    ##   y5_AWARE ~                                                            
    ##     y0_CATINC        -0.460    0.147   -3.122    0.002   -0.460   -0.108
    ##     y0_pbmi           0.000    0.023    0.009    0.993    0.000    0.000
    ##   y5_DISTRST ~                                                          
    ##     y0_CATINC        -0.278    0.105   -2.632    0.008   -0.278   -0.091
    ##     y0_pbmi           0.029    0.015    1.925    0.054    0.029    0.060
    ##   y5_INEFCT ~                                                           
    ##     y0_CATINC        -0.363    0.124   -2.939    0.003   -0.363   -0.104
    ##     y0_pbmi           0.041    0.022    1.894    0.058    0.041    0.075
    ##   y3_ANXR ~                                                             
    ##     y0_CATINC        -0.618    0.202   -3.061    0.002   -0.618   -0.105
    ##     y0_pbmi           0.110    0.029    3.773    0.000    0.110    0.119
    ##   y5_AWARE ~                                                            
    ##     y0_RACE           1.015    0.305    3.327    0.001    1.015    0.109
    ##   y5_DISTRST ~                                                          
    ##     y0_RACE           1.477    0.234    6.301    0.000    1.477    0.220
    ##   y5_INEFCT ~                                                           
    ##     y0_RACE          -0.119    0.277   -0.430    0.667   -0.119   -0.016
    ##   y5_AWARE ~                                                            
    ##     y0_ARMCIRAV       0.116    0.045    2.580    0.010    0.116    0.091
    ##   y5_INEFCT ~                                                           
    ##     y0_ARMCIRAV       0.070    0.039    1.812    0.070    0.070    0.067
    ##   y5_SELWT ~                                                            
    ##     y0_ARMCIRAV      -0.016    0.006   -2.543    0.011   -0.016   -0.093
    ##     y0_pbmi          -0.005    0.003   -1.451    0.147   -0.005   -0.053
    ##     y0_CATINC         0.018    0.019    0.918    0.358    0.018    0.030
    ##     y0_RACE           0.163    0.044    3.673    0.000    0.163    0.125
    ##   y0_ARMCIRAV ~                                                         
    ##     y0_pbmi           0.145    0.023    6.263    0.000    0.145    0.276
    ##     y0_CATINC         0.000                               0.000    0.000
    ##     y0_RACE           0.364    0.253    1.440    0.150    0.364    0.050
    ##   y0_pbmi ~                                                             
    ##     y0_RACE           2.981    0.480    6.205    0.000    2.981    0.213
    ##   y0_CATINC ~                                                           
    ##     y0_RACE          -0.637    0.074   -8.618    0.000   -0.637   -0.290
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##  .y5_AWARE ~~                                                           
    ##    .y5_DISTRST        2.548    0.562    4.532    0.000    2.548    0.195
    ##    .y5_INEFCT         6.847    0.945    7.248    0.000    6.847    0.463
    ##    .y5_SELWT         -0.469    0.100   -4.665    0.000   -0.469   -0.190
    ##  .y5_DISTRST ~~                                                         
    ##    .y5_INEFCT         4.442    0.603    7.372    0.000    4.442    0.409
    ##    .y5_SELWT         -0.436    0.080   -5.466    0.000   -0.436   -0.240
    ##  .y5_INEFCT ~~                                                          
    ##    .y5_SELWT         -1.043    0.105   -9.910    0.000   -1.043   -0.509
    ##  .y3_ANXR ~~                                                            
    ##    .y0_ARMCIRAV       0.000                               0.000    0.000
    ##     y0_RACE           0.000                               0.000    0.000
    ##  .y0_pbmi ~~                                                            
    ##    .y0_CATINC         0.000                               0.000    0.000
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .y10_ARMCIRAV     11.815    0.898   13.162    0.000   11.815    0.377
    ##    .y5_AWARE         17.838    1.553   11.483    0.000   17.838    0.820
    ##    .y5_DISTRST        9.617    0.688   13.979    0.000    9.617    0.856
    ##    .y5_INEFCT        12.251    1.078   11.368    0.000   12.251    0.840
    ##    .y5_SELWT          0.343    0.018   19.280    0.000    0.343    0.813
    ##    .y3_ANXR          40.720    1.682   24.203    0.000   40.720    0.973
    ##    .y0_ARMCIRAV      12.244    0.660   18.562    0.000   12.244    0.915
    ##    .y0_pbmi          46.404    4.188   11.080    0.000   46.404    0.954
    ##    .y0_CATINC         1.102    0.043   25.847    0.000    1.102    0.916
    ##     y0_RACE           0.249    0.001  285.949    0.000    0.249    1.000
