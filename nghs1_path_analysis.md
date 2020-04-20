NGHS1 Path Analysis
================
Daniel O’Leary
November 14, 2018

  - [Setup](#setup)
      - [Load Packages](#load-packages)
      - [Load Data](#load-data)
      - [Check Dimensions](#check-dimensions)
  - [Path Models](#path-models)
      - [Stress Psych Initial Model](#stress-psych-initial-model)
      - [Stress Psych Pruned Model](#stress-psych-pruned-model)
      - [Stress Psych Pruned Model - Parameter
        Export](#stress-psych-pruned-model---parameter-export)
      - [Stress Psych Pruned Model - All
        Data](#stress-psych-pruned-model---all-data)
  - [Anxiety Psych](#anxiety-psych)
      - [Anxiety Psych Initial Model](#anxiety-psych-initial-model)
      - [Anxiety Psych Pruned Model](#anxiety-psych-pruned-model)
      - [Anxiety Psych Pruned Model - Parameter
        Export](#anxiety-psych-pruned-model---parameter-export)
      - [Anxiety Psych Pruned Model - All
        Data](#anxiety-psych-pruned-model---all-data)
  - [Stress Behavior](#stress-behavior)
      - [Stress Behavior Initial Model](#stress-behavior-initial-model)
      - [Stress Behavior Pruned Model](#stress-behavior-pruned-model)
      - [Stress Behavior Pruned Model - Parameter
        Export](#stress-behavior-pruned-model---parameter-export)
      - [Stress Behavior Pruned Model - All
        Data](#stress-behavior-pruned-model---all-data)
  - [Anxiety Behavior](#anxiety-behavior)
      - [Anxiety Behavior Initial
        Model](#anxiety-behavior-initial-model)
      - [Anxiety Behavior Pruned Model](#anxiety-behavior-pruned-model)
      - [Anxiety Behavior Pruned Model - Parameter
        Export](#anxiety-behavior-pruned-model---parameter-export)
      - [Anxiety Behavior Pruned Model - All
        Data](#anxiety-behavior-pruned-model---all-data)

# Setup

## Load Packages

## Load Data

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

## Check Dimensions

``` r
dim(semTrainingData)
```

    ## [1]  793 1628

``` r
dim(semTestingData)
```

    ## [1]  793 1628

# Path Models

Good fit for models:

  - Chi-square p-val \> 0.05 \[not necc. w/ large sample size\]
  - TLI \>= 0.95
  - CFI \>= 0.90
  - RMSEA \< 0.08
  - SRMR \< 0.08

## Stress Psych Initial Model

``` r
mod_psych_stress_initial <-
  '
    #################
    ## Regressions ##
    #################  
     
    y10_QI ~ y0_CATINC + y0_QI + y0_pbmi + y0_RACE  

    y10_QI ~ y5_AWARE + y5_DISTRST + y5_INEFCT + y5_SELWT + y2_STRESS

    y5_AWARE + y5_DISTRST + y5_INEFCT + y5_SELWT ~ y2_STRESS

    y5_AWARE + y5_DISTRST + y5_INEFCT + y5_SELWT + y2_STRESS ~ y0_CATINC + y0_QI + y0_RACE + y0_pbmi

    y0_QI ~ y0_pbmi + y0_CATINC + y0_RACE

    y0_pbmi + y0_CATINC ~ y0_RACE

    #################
    ## Covariances ##
    #################    

    y5_AWARE ~~ y5_DISTRST + y5_INEFCT + y5_SELWT

    y5_DISTRST ~~ y5_INEFCT + y5_SELWT

    y5_INEFCT ~~ y5_SELWT

    y0_pbmi ~~ y0_CATINC


  '

fit_psych_stress_initial <- 
  sem(
    mod_psych_stress_initial, 
    data = semTrainingData, 
    estimator = "MLM"
  )

summary(
  fit_psych_stress_initial, 
  standardized = TRUE, 
  fit.measures = TRUE
)
```

    ## lavaan 0.6-5 ended normally after 159 iterations
    ## 
    ##   Estimator                                         ML
    ##   Optimization method                           NLMINB
    ##   Number of free parameters                         54
    ##                                                       
    ##   Number of observations                           793
    ##                                                       
    ## Model Test User Model:
    ##                                               Standard      Robust
    ##   Test Statistic                                 0.000       0.000
    ##   Degrees of freedom                                 0           0
    ## 
    ## Model Test Baseline Model:
    ## 
    ##   Test statistic                              2020.749    1590.195
    ##   Degrees of freedom                                45          45
    ##   P-value                                        0.000       0.000
    ##   Scaling correction factor                                  1.271
    ## 
    ## User Model versus Baseline Model:
    ## 
    ##   Comparative Fit Index (CFI)                    1.000       1.000
    ##   Tucker-Lewis Index (TLI)                       1.000       1.000
    ##                                                                   
    ##   Robust Comparative Fit Index (CFI)                            NA
    ##   Robust Tucker-Lewis Index (TLI)                               NA
    ## 
    ## Loglikelihood and Information Criteria:
    ## 
    ##   Loglikelihood user model (H0)             -17483.046  -17483.046
    ##   Loglikelihood unrestricted model (H1)     -17483.046  -17483.046
    ##                                                                   
    ##   Akaike (AIC)                               35074.092   35074.092
    ##   Bayesian (BIC)                             35326.586   35326.586
    ##   Sample-size adjusted Bayesian (BIC)        35155.107   35155.107
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.000       0.000
    ##   90 Percent confidence interval - lower         0.000       0.000
    ##   90 Percent confidence interval - upper         0.000       0.000
    ##   P-value RMSEA <= 0.05                             NA          NA
    ##                                                                   
    ##   Robust RMSEA                                               0.000
    ##   90 Percent confidence interval - lower                     0.000
    ##   90 Percent confidence interval - upper                     0.000
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.000       0.000
    ## 
    ## Parameter Estimates:
    ## 
    ##   Information                                 Expected
    ##   Information saturated (h1) model          Structured
    ##   Standard errors                           Robust.sem
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   y10_QI ~                                                              
    ##     y0_CATINC        -0.388    0.135   -2.881    0.004   -0.388   -0.067
    ##     y0_QI             1.278    0.048   26.414    0.000    1.278    0.743
    ##     y0_pbmi           0.081    0.025    3.239    0.001    0.081    0.091
    ##     y0_RACE           0.585    0.305    1.916    0.055    0.585    0.046
    ##     y5_AWARE          0.055    0.042    1.320    0.187    0.055    0.039
    ##     y5_DISTRST        0.071    0.057    1.249    0.212    0.071    0.037
    ##     y5_INEFCT         0.099    0.057    1.747    0.081    0.099    0.057
    ##     y5_SELWT          0.278    0.259    1.076    0.282    0.278    0.027
    ##     y2_STRESS         0.008    0.019    0.435    0.663    0.008    0.009
    ##   y5_AWARE ~                                                            
    ##     y2_STRESS         0.067    0.021    3.237    0.001    0.067    0.100
    ##   y5_DISTRST ~                                                          
    ##     y2_STRESS         0.063    0.016    3.949    0.000    0.063    0.128
    ##   y5_INEFCT ~                                                           
    ##     y2_STRESS         0.115    0.020    5.713    0.000    0.115    0.209
    ##   y5_SELWT ~                                                            
    ##     y2_STRESS        -0.015    0.003   -4.712    0.000   -0.015   -0.160
    ##   y5_AWARE ~                                                            
    ##     y0_CATINC        -0.441    0.158   -2.797    0.005   -0.441   -0.108
    ##     y0_QI             0.077    0.047    1.649    0.099    0.077    0.063
    ##     y0_RACE           1.086    0.314    3.453    0.001    1.086    0.122
    ##     y0_pbmi           0.068    0.027    2.568    0.010    0.068    0.109
    ##   y5_DISTRST ~                                                          
    ##     y0_CATINC        -0.499    0.108   -4.641    0.000   -0.499   -0.166
    ##     y0_QI            -0.002    0.030   -0.068    0.946   -0.002   -0.002
    ##     y0_RACE           1.720    0.232    7.417    0.000    1.720    0.263
    ##     y0_pbmi           0.014    0.015    0.957    0.339    0.014    0.031
    ##   y5_INEFCT ~                                                           
    ##     y0_CATINC        -0.388    0.131   -2.956    0.003   -0.388   -0.116
    ##     y0_QI             0.093    0.035    2.655    0.008    0.093    0.094
    ##     y0_RACE          -0.630    0.285   -2.213    0.027   -0.630   -0.086
    ##     y0_pbmi           0.048    0.021    2.231    0.026    0.048    0.092
    ##   y5_SELWT ~                                                            
    ##     y0_CATINC         0.047    0.021    2.260    0.024    0.047    0.084
    ##     y0_QI            -0.016    0.006   -2.738    0.006   -0.016   -0.099
    ##     y0_RACE           0.271    0.046    5.824    0.000    0.271    0.222
    ##     y0_pbmi          -0.008    0.004   -2.372    0.018   -0.008   -0.098
    ##   y2_STRESS ~                                                           
    ##     y0_CATINC        -0.209    0.237   -0.882    0.378   -0.209   -0.034
    ##     y0_QI             0.029    0.074    0.388    0.698    0.029    0.016
    ##     y0_RACE          -0.513    0.532   -0.964    0.335   -0.513   -0.039
    ##     y0_pbmi           0.063    0.033    1.922    0.055    0.063    0.067
    ##   y0_QI ~                                                               
    ##     y0_pbmi           0.121    0.020    6.074    0.000    0.121    0.234
    ##     y0_CATINC         0.146    0.130    1.119    0.263    0.146    0.043
    ##     y0_RACE           1.125    0.275    4.084    0.000    1.125    0.154
    ##   y0_pbmi ~                                                             
    ##     y0_RACE           2.994    0.491    6.098    0.000    2.994    0.211
    ##   y0_CATINC ~                                                           
    ##     y0_RACE          -0.709    0.073   -9.707    0.000   -0.709   -0.326
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##  .y5_AWARE ~~                                                           
    ##    .y5_DISTRST        3.380    0.682    4.957    0.000    3.380    0.262
    ##    .y5_INEFCT         8.129    1.187    6.848    0.000    8.129    0.544
    ##    .y5_SELWT         -0.809    0.113   -7.152    0.000   -0.809   -0.324
    ##  .y5_DISTRST ~~                                                         
    ##    .y5_INEFCT         3.876    0.597    6.487    0.000    3.876    0.367
    ##    .y5_SELWT         -0.545    0.067   -8.167    0.000   -0.545   -0.309
    ##  .y5_INEFCT ~~                                                          
    ##    .y5_SELWT         -1.008    0.109   -9.257    0.000   -1.008   -0.493
    ##  .y0_pbmi ~~                                                            
    ##    .y0_CATINC        -0.104    0.259   -0.403    0.687   -0.104   -0.015
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .y10_QI           13.637    0.938   14.535    0.000   13.637    0.343
    ##    .y5_AWARE         18.228    1.521   11.985    0.000   18.228    0.922
    ##    .y5_DISTRST        9.106    0.689   13.210    0.000    9.106    0.852
    ##    .y5_INEFCT        12.243    1.410    8.681    0.000   12.243    0.919
    ##    .y5_SELWT          0.342    0.018   19.414    0.000    0.342    0.917
    ##    .y2_STRESS        43.774    2.728   16.045    0.000   43.774    0.994
    ##    .y0_QI            12.214    0.829   14.740    0.000   12.214    0.910
    ##    .y0_pbmi          47.862    4.660   10.271    0.000   47.862    0.955
    ##    .y0_CATINC         1.058    0.043   24.364    0.000    1.058    0.894

## Stress Psych Pruned Model

``` r
mod_psych_stress_pruned <-
  '
    ######################
    ## Latent Variables ##
    ######################

    #################
    ## Regressions ##
    #################  
     
    y10_QI ~ y0_CATINC + y0_QI + y0_pbmi + y0_RACE  

    y10_QI ~ y5_AWARE + y5_DISTRST + y5_INEFCT + y5_SELWT

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

fit_psych_stress_pruned <- 
  sem(
    mod_psych_stress_pruned, 
    data = semTestingData, 
    estimator = "MLM"
  )

summary(
  fit_psych_stress_pruned, 
  standardized = TRUE, 
  fit.measures = TRUE,
  rsq = TRUE
)
```

    ## lavaan 0.6-5 ended normally after 143 iterations
    ## 
    ##   Estimator                                         ML
    ##   Optimization method                           NLMINB
    ##   Number of free parameters                         49
    ##                                                       
    ##   Number of observations                           793
    ##                                                       
    ## Model Test User Model:
    ##                                               Standard      Robust
    ##   Test Statistic                                 6.243       5.675
    ##   Degrees of freedom                                 5           5
    ##   P-value (Chi-square)                           0.283       0.339
    ##   Scaling correction factor                                  1.100
    ##     for the Satorra-Bentler correction 
    ## 
    ## Model Test Baseline Model:
    ## 
    ##   Test statistic                              2117.043    1665.443
    ##   Degrees of freedom                                45          45
    ##   P-value                                        0.000       0.000
    ##   Scaling correction factor                                  1.271
    ## 
    ## User Model versus Baseline Model:
    ## 
    ##   Comparative Fit Index (CFI)                    0.999       1.000
    ##   Tucker-Lewis Index (TLI)                       0.995       0.996
    ##                                                                   
    ##   Robust Comparative Fit Index (CFI)                         1.000
    ##   Robust Tucker-Lewis Index (TLI)                            0.997
    ## 
    ## Loglikelihood and Information Criteria:
    ## 
    ##   Loglikelihood user model (H0)             -17743.813  -17743.813
    ##   Loglikelihood unrestricted model (H1)     -17740.692  -17740.692
    ##                                                                   
    ##   Akaike (AIC)                               35585.626   35585.626
    ##   Bayesian (BIC)                             35814.741   35814.741
    ##   Sample-size adjusted Bayesian (BIC)        35659.140   35659.140
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.018       0.013
    ##   90 Percent confidence interval - lower         0.000       0.000
    ##   90 Percent confidence interval - upper         0.055       0.051
    ##   P-value RMSEA <= 0.05                          0.916       0.945
    ##                                                                   
    ##   Robust RMSEA                                               0.014
    ##   90 Percent confidence interval - lower                     0.000
    ##   90 Percent confidence interval - upper                     0.055
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.011       0.011
    ## 
    ## Parameter Estimates:
    ## 
    ##   Information                                 Expected
    ##   Information saturated (h1) model          Structured
    ##   Standard errors                           Robust.sem
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   y10_QI ~                                                              
    ##     y0_CATINC        -0.326    0.144   -2.266    0.023   -0.326   -0.052
    ##     y0_QI             1.316    0.043   30.877    0.000    1.316    0.755
    ##     y0_pbmi           0.096    0.022    4.303    0.000    0.096    0.097
    ##     y0_RACE           0.231    0.322    0.718    0.473    0.231    0.017
    ##     y5_AWARE          0.047    0.038    1.250    0.211    0.047    0.032
    ##     y5_DISTRST        0.146    0.063    2.329    0.020    0.146    0.071
    ##     y5_INEFCT        -0.050    0.066   -0.748    0.454   -0.050   -0.027
    ##     y5_SELWT         -0.449    0.278   -1.615    0.106   -0.449   -0.042
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
    ##    .y10_QI           15.963    1.285   12.419    0.000   15.963    0.335
    ##    .y5_AWARE         19.873    1.742   11.408    0.000   19.873    0.916
    ##    .y5_DISTRST       10.018    0.713   14.059    0.000   10.018    0.896
    ##    .y5_INEFCT        13.099    1.169   11.205    0.000   13.099    0.898
    ##    .y5_SELWT          0.383    0.020   18.853    0.000    0.383    0.903
    ##    .y2_STRESS        46.316    2.530   18.309    0.000   46.316    0.973
    ##    .y0_QI            14.079    0.870   16.186    0.000   14.079    0.899
    ##    .y0_pbmi          46.404    4.188   11.080    0.000   46.404    0.954
    ##    .y0_CATINC         1.102    0.043   25.847    0.000    1.102    0.916
    ## 
    ## R-Square:
    ##                    Estimate
    ##     y10_QI            0.665
    ##     y5_AWARE          0.084
    ##     y5_DISTRST        0.104
    ##     y5_INEFCT         0.102
    ##     y5_SELWT          0.097
    ##     y2_STRESS         0.027
    ##     y0_QI             0.101
    ##     y0_pbmi           0.046
    ##     y0_CATINC         0.084

## Stress Psych Pruned Model - Parameter Export

``` r
standardizedSolution(
  fit_psych_stress_pruned,
  type = "std.all",
  se = TRUE,
  zstat = TRUE,
  pvalue = TRUE,
  ci = TRUE,
  level = 0.95
) %>%
  mutate_if(
    ~any(is.numeric(.)),
    ~round(., 3)
  ) %>%
  write.table(
    "G:/My Drive/research/projects/phsr1/nghs1/nghs1_plots/stress_psych_table_std.csv",
    sep =","
  )
```

## Stress Psych Pruned Model - All Data

``` r
fit_psych_stress_pruned_ad <-
  sem(
    mod_psych_stress_pruned, 
    data = semAllData, 
    estimator = "MLM"
  )

summary(
  fit_psych_stress_pruned_ad, 
  standardized = TRUE, 
  fit.measures = TRUE
)
```

    ## lavaan 0.6-5 ended normally after 146 iterations
    ## 
    ##   Estimator                                         ML
    ##   Optimization method                           NLMINB
    ##   Number of free parameters                         49
    ##                                                       
    ##   Number of observations                          2379
    ##                                                       
    ## Model Test User Model:
    ##                                               Standard      Robust
    ##   Test Statistic                                 3.517       3.298
    ##   Degrees of freedom                                 5           5
    ##   P-value (Chi-square)                           0.621       0.654
    ##   Scaling correction factor                                  1.067
    ##     for the Satorra-Bentler correction 
    ## 
    ## Model Test Baseline Model:
    ## 
    ##   Test statistic                              6012.148    4786.830
    ##   Degrees of freedom                                45          45
    ##   P-value                                        0.000       0.000
    ##   Scaling correction factor                                  1.256
    ## 
    ## User Model versus Baseline Model:
    ## 
    ##   Comparative Fit Index (CFI)                    1.000       1.000
    ##   Tucker-Lewis Index (TLI)                       1.002       1.003
    ##                                                                   
    ##   Robust Comparative Fit Index (CFI)                         1.000
    ##   Robust Tucker-Lewis Index (TLI)                            1.003
    ## 
    ## Loglikelihood and Information Criteria:
    ## 
    ##   Loglikelihood user model (H0)             -53072.867  -53072.867
    ##   Loglikelihood unrestricted model (H1)     -53071.109  -53071.109
    ##                                                                   
    ##   Akaike (AIC)                              106243.734  106243.734
    ##   Bayesian (BIC)                            106526.682  106526.682
    ##   Sample-size adjusted Bayesian (BIC)       106370.998  106370.998
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.000       0.000
    ##   90 Percent confidence interval - lower         0.000       0.000
    ##   90 Percent confidence interval - upper         0.024       0.022
    ##   P-value RMSEA <= 0.05                          1.000       1.000
    ##                                                                   
    ##   Robust RMSEA                                               0.000
    ##   90 Percent confidence interval - lower                     0.000
    ##   90 Percent confidence interval - upper                     0.024
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.007       0.007
    ## 
    ## Parameter Estimates:
    ## 
    ##   Information                                 Expected
    ##   Information saturated (h1) model          Structured
    ##   Standard errors                           Robust.sem
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   y10_QI ~                                                              
    ##     y0_CATINC        -0.421    0.082   -5.125    0.000   -0.421   -0.070
    ##     y0_QI             1.292    0.027   47.646    0.000    1.292    0.741
    ##     y0_pbmi           0.090    0.014    6.385    0.000    0.090    0.093
    ##     y0_RACE           0.477    0.187    2.554    0.011    0.477    0.036
    ##     y5_AWARE          0.050    0.024    2.127    0.033    0.050    0.035
    ##     y5_DISTRST        0.077    0.032    2.404    0.016    0.077    0.039
    ##     y5_INEFCT         0.037    0.035    1.072    0.284    0.037    0.021
    ##     y5_SELWT         -0.212    0.154   -1.378    0.168   -0.212   -0.020
    ##   y5_AWARE ~                                                            
    ##     y2_STRESS         0.118    0.013    9.165    0.000    0.118    0.175
    ##   y5_DISTRST ~                                                          
    ##     y2_STRESS         0.065    0.010    6.413    0.000    0.065    0.134
    ##   y5_INEFCT ~                                                           
    ##     y2_STRESS         0.126    0.013   10.041    0.000    0.126    0.224
    ##   y5_SELWT ~                                                            
    ##     y2_STRESS        -0.020    0.002  -11.059    0.000   -0.020   -0.219
    ##   y5_AWARE ~                                                            
    ##     y0_CATINC        -0.568    0.092   -6.198    0.000   -0.568   -0.136
    ##     y0_RACE           0.971    0.190    5.110    0.000    0.971    0.105
    ##     y0_pbmi           0.035    0.014    2.411    0.016    0.035    0.052
    ##   y5_DISTRST ~                                                          
    ##     y0_CATINC        -0.314    0.063   -4.994    0.000   -0.314   -0.105
    ##     y0_RACE           1.701    0.138   12.327    0.000    1.701    0.257
    ##     y0_pbmi           0.013    0.009    1.489    0.136    0.013    0.028
    ##   y5_INEFCT ~                                                           
    ##     y0_CATINC        -0.358    0.073   -4.913    0.000   -0.358   -0.103
    ##     y0_RACE          -0.335    0.167   -2.006    0.045   -0.335   -0.044
    ##     y0_pbmi           0.043    0.013    3.404    0.001    0.043    0.078
    ##   y2_STRESS ~                                                           
    ##     y0_CATINC        -0.556    0.135   -4.125    0.000   -0.556   -0.090
    ##     y0_RACE          -0.535    0.304   -1.764    0.078   -0.535   -0.039
    ##     y0_pbmi           0.065    0.020    3.231    0.001    0.065    0.065
    ##   y5_AWARE ~                                                            
    ##     y0_QI             0.098    0.027    3.626    0.000    0.098    0.081
    ##   y5_INEFCT ~                                                           
    ##     y0_QI             0.086    0.022    3.944    0.000    0.086    0.086
    ##   y5_SELWT ~                                                            
    ##     y0_QI            -0.020    0.004   -5.693    0.000   -0.020   -0.121
    ##     y0_pbmi          -0.005    0.002   -2.471    0.013   -0.005   -0.056
    ##     y0_CATINC         0.025    0.012    2.051    0.040    0.025    0.043
    ##     y0_RACE           0.229    0.027    8.405    0.000    0.229    0.180
    ##   y0_QI ~                                                               
    ##     y0_pbmi           0.150    0.014   10.717    0.000    0.150    0.270
    ##     y0_CATINC         0.000                               0.000    0.000
    ##     y0_RACE           0.811    0.150    5.418    0.000    0.811    0.106
    ##   y0_pbmi ~                                                             
    ##     y0_RACE           2.839    0.274   10.361    0.000    2.839    0.207
    ##   y0_CATINC ~                                                           
    ##     y0_RACE          -0.721    0.043  -16.915    0.000   -0.721   -0.326
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##  .y5_AWARE ~~                                                           
    ##    .y5_DISTRST        2.892    0.372    7.775    0.000    2.892    0.212
    ##    .y5_INEFCT         8.249    0.656   12.581    0.000    8.249    0.511
    ##    .y5_SELWT         -0.778    0.066  -11.867    0.000   -0.778   -0.293
    ##  .y5_DISTRST ~~                                                         
    ##    .y5_INEFCT         4.446    0.378   11.774    0.000    4.446    0.389
    ##    .y5_SELWT         -0.496    0.044  -11.187    0.000   -0.496   -0.264
    ##  .y5_INEFCT ~~                                                          
    ##    .y5_SELWT         -1.148    0.072  -16.001    0.000   -1.148   -0.518
    ##  .y2_STRESS ~~                                                          
    ##    .y0_QI             0.000                               0.000    0.000
    ##  .y0_pbmi ~~                                                            
    ##    .y0_CATINC         0.000                               0.000    0.000
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .y10_QI           15.496    0.682   22.733    0.000   15.496    0.349
    ##    .y5_AWARE         19.313    0.935   20.654    0.000   19.313    0.906
    ##    .y5_DISTRST        9.663    0.420   23.017    0.000    9.663    0.880
    ##    .y5_INEFCT        13.484    0.850   15.862    0.000   13.484    0.917
    ##    .y5_SELWT          0.364    0.011   32.918    0.000    0.364    0.908
    ##    .y2_STRESS        46.110    1.531   30.123    0.000   46.110    0.989
    ##    .y0_QI            13.199    0.496   26.594    0.000   13.199    0.904
    ##    .y0_pbmi          44.981    2.313   19.444    0.000   44.981    0.957
    ##    .y0_CATINC         1.090    0.025   44.306    0.000    1.090    0.894

# Anxiety Psych

## Anxiety Psych Initial Model

``` r
mod_psych_anx_initial <-
  '
    #################
    ## Regressions ##
    #################  
     
    y10_QI ~ y0_CATINC + y0_QI + y0_pbmi + y0_RACE  

    y10_QI ~ y5_AWARE + y5_DISTRST + y5_INEFCT + y5_SELWT + y3_ANXR

    y5_AWARE + y5_DISTRST + y5_INEFCT + y5_SELWT ~ y3_ANXR

    y5_AWARE + y5_DISTRST + y5_INEFCT + y5_SELWT + y3_ANXR ~ y0_CATINC + y0_QI + y0_RACE + y0_pbmi

    y0_QI ~ y0_pbmi + y0_CATINC + y0_RACE

    y0_pbmi + y0_CATINC ~ y0_RACE

    #################
    ## Covariances ##
    #################    

    y5_AWARE ~~ y5_DISTRST + y5_INEFCT + y5_SELWT 

    y5_DISTRST ~~ y5_INEFCT + y5_SELWT 

    y5_INEFCT ~~ y5_SELWT 

    y0_pbmi ~~ y0_CATINC
  '

fit_psych_anx_initial <- 
  sem(
    mod_psych_anx_initial, 
    data = semTrainingData, 
    estimator = "MLM"
  )

summary(
  fit_psych_anx_initial, 
  standardized = TRUE, 
  fit.measures = TRUE
)
```

    ## lavaan 0.6-5 ended normally after 147 iterations
    ## 
    ##   Estimator                                         ML
    ##   Optimization method                           NLMINB
    ##   Number of free parameters                         54
    ##                                                       
    ##   Number of observations                           793
    ##                                                       
    ## Model Test User Model:
    ##                                               Standard      Robust
    ##   Test Statistic                                 0.000       0.000
    ##   Degrees of freedom                                 0           0
    ## 
    ## Model Test Baseline Model:
    ## 
    ##   Test statistic                              2194.294    1712.958
    ##   Degrees of freedom                                45          45
    ##   P-value                                        0.000       0.000
    ##   Scaling correction factor                                  1.281
    ## 
    ## User Model versus Baseline Model:
    ## 
    ##   Comparative Fit Index (CFI)                    1.000       1.000
    ##   Tucker-Lewis Index (TLI)                       1.000       1.000
    ##                                                                   
    ##   Robust Comparative Fit Index (CFI)                            NA
    ##   Robust Tucker-Lewis Index (TLI)                               NA
    ## 
    ## Loglikelihood and Information Criteria:
    ## 
    ##   Loglikelihood user model (H0)             -17362.597  -17362.597
    ##   Loglikelihood unrestricted model (H1)     -17362.597  -17362.597
    ##                                                                   
    ##   Akaike (AIC)                               34833.193   34833.193
    ##   Bayesian (BIC)                             35085.688   35085.688
    ##   Sample-size adjusted Bayesian (BIC)        34914.209   34914.209
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.000       0.000
    ##   90 Percent confidence interval - lower         0.000       0.000
    ##   90 Percent confidence interval - upper         0.000       0.000
    ##   P-value RMSEA <= 0.05                             NA          NA
    ##                                                                   
    ##   Robust RMSEA                                               0.000
    ##   90 Percent confidence interval - lower                     0.000
    ##   90 Percent confidence interval - upper                     0.000
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.000       0.000
    ## 
    ## Parameter Estimates:
    ## 
    ##   Information                                 Expected
    ##   Information saturated (h1) model          Structured
    ##   Standard errors                           Robust.sem
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   y10_QI ~                                                              
    ##     y0_CATINC        -0.384    0.135   -2.849    0.004   -0.384   -0.066
    ##     y0_QI             1.280    0.049   26.378    0.000    1.280    0.744
    ##     y0_pbmi           0.081    0.025    3.210    0.001    0.081    0.091
    ##     y0_RACE           0.577    0.306    1.889    0.059    0.577    0.046
    ##     y5_AWARE          0.050    0.042    1.198    0.231    0.050    0.035
    ##     y5_DISTRST        0.070    0.058    1.220    0.223    0.070    0.036
    ##     y5_INEFCT         0.098    0.056    1.746    0.081    0.098    0.057
    ##     y5_SELWT          0.322    0.267    1.206    0.228    0.322    0.031
    ##     y3_ANXR           0.017    0.023    0.756    0.450    0.017    0.018
    ##   y5_AWARE ~                                                            
    ##     y3_ANXR           0.239    0.025    9.681    0.000    0.239    0.342
    ##   y5_DISTRST ~                                                          
    ##     y3_ANXR           0.112    0.017    6.803    0.000    0.112    0.219
    ##   y5_INEFCT ~                                                           
    ##     y3_ANXR           0.208    0.020   10.186    0.000    0.208    0.363
    ##   y5_SELWT ~                                                            
    ##     y3_ANXR          -0.038    0.003  -12.239    0.000   -0.038   -0.397
    ##   y5_AWARE ~                                                            
    ##     y0_CATINC        -0.309    0.146   -2.117    0.034   -0.309   -0.076
    ##     y0_QI             0.088    0.042    2.098    0.036    0.088    0.073
    ##     y0_RACE           1.082    0.301    3.594    0.000    1.082    0.122
    ##     y0_pbmi           0.049    0.025    1.975    0.048    0.049    0.078
    ##   y5_DISTRST ~                                                          
    ##     y0_CATINC        -0.444    0.107   -4.152    0.000   -0.444   -0.148
    ##     y0_QI             0.004    0.030    0.145    0.885    0.004    0.005
    ##     y0_RACE           1.702    0.229    7.438    0.000    1.702    0.260
    ##     y0_pbmi           0.007    0.015    0.487    0.626    0.007    0.016
    ##   y5_INEFCT ~                                                           
    ##     y0_CATINC        -0.285    0.123   -2.326    0.020   -0.285   -0.085
    ##     y0_QI             0.105    0.033    3.174    0.002    0.105    0.106
    ##     y0_RACE          -0.663    0.278   -2.381    0.017   -0.663   -0.091
    ##     y0_pbmi           0.034    0.020    1.700    0.089    0.034    0.066
    ##   y5_SELWT ~                                                            
    ##     y0_CATINC         0.027    0.019    1.402    0.161    0.027    0.048
    ##     y0_QI            -0.018    0.006   -3.265    0.001   -0.018   -0.110
    ##     y0_RACE           0.273    0.043    6.427    0.000    0.273    0.224
    ##     y0_pbmi          -0.006    0.003   -1.754    0.079   -0.006   -0.065
    ##   y3_ANXR ~                                                             
    ##     y0_CATINC        -0.612    0.220   -2.783    0.005   -0.612   -0.105
    ##     y0_QI            -0.041    0.066   -0.618    0.537   -0.041   -0.023
    ##     y0_RACE          -0.126    0.496   -0.254    0.800   -0.126   -0.010
    ##     y0_pbmi           0.099    0.037    2.685    0.007    0.099    0.110
    ##   y0_QI ~                                                               
    ##     y0_pbmi           0.121    0.020    6.074    0.000    0.121    0.234
    ##     y0_CATINC         0.146    0.130    1.119    0.263    0.146    0.043
    ##     y0_RACE           1.125    0.275    4.084    0.000    1.125    0.154
    ##   y0_pbmi ~                                                             
    ##     y0_RACE           2.994    0.491    6.098    0.000    2.994    0.211
    ##   y0_CATINC ~                                                           
    ##     y0_RACE          -0.709    0.073   -9.707    0.000   -0.709   -0.326
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##  .y5_AWARE ~~                                                           
    ##    .y5_DISTRST        2.504    0.597    4.195    0.000    2.504    0.210
    ##    .y5_INEFCT         6.497    1.017    6.386    0.000    6.497    0.485
    ##    .y5_SELWT         -0.492    0.093   -5.295    0.000   -0.492   -0.226
    ##  .y5_DISTRST ~~                                                         
    ##    .y5_INEFCT         3.270    0.553    5.913    0.000    3.270    0.331
    ##    .y5_SELWT         -0.417    0.060   -6.910    0.000   -0.417   -0.259
    ##  .y5_INEFCT ~~                                                          
    ##    .y5_SELWT         -0.768    0.090   -8.572    0.000   -0.768   -0.425
    ##  .y0_pbmi ~~                                                            
    ##    .y0_CATINC        -0.104    0.259   -0.403    0.687   -0.104   -0.015
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .y10_QI           13.631    0.939   14.518    0.000   13.631    0.343
    ##    .y5_AWARE         16.164    1.325   12.203    0.000   16.164    0.818
    ##    .y5_DISTRST        8.782    0.665   13.213    0.000    8.782    0.822
    ##    .y5_INEFCT        11.109    1.287    8.630    0.000   11.109    0.834
    ##    .y5_SELWT          0.294    0.016   18.582    0.000    0.294    0.789
    ##    .y3_ANXR          39.528    1.574   25.118    0.000   39.528    0.977
    ##    .y0_QI            12.214    0.829   14.740    0.000   12.214    0.910
    ##    .y0_pbmi          47.862    4.660   10.271    0.000   47.862    0.955
    ##    .y0_CATINC         1.058    0.043   24.364    0.000    1.058    0.894

## Anxiety Psych Pruned Model

``` r
mod_psych_anx_pruned <-
  '
    #################
    ## Regressions ##
    #################  
     
    y10_QI ~ y0_CATINC + y0_QI + y0_pbmi + y0_RACE  

    y10_QI ~ y5_AWARE + y5_DISTRST + y5_INEFCT + y5_SELWT 

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


fit_psych_anx_pruned <-
  sem(
    mod_psych_anx_pruned, 
    data = semTestingData, 
    estimator = "MLM"
  )

summary(
  fit_psych_anx_pruned, 
  standardized = TRUE, 
  fit.measures = TRUE,
  rsq = TRUE
)
```

    ## lavaan 0.6-5 ended normally after 125 iterations
    ## 
    ##   Estimator                                         ML
    ##   Optimization method                           NLMINB
    ##   Number of free parameters                         49
    ##                                                       
    ##   Number of observations                           793
    ##                                                       
    ## Model Test User Model:
    ##                                               Standard      Robust
    ##   Test Statistic                                10.113       9.453
    ##   Degrees of freedom                                 6           6
    ##   P-value (Chi-square)                           0.120       0.150
    ##   Scaling correction factor                                  1.070
    ##     for the Satorra-Bentler correction 
    ## 
    ## Model Test Baseline Model:
    ## 
    ##   Test statistic                              2259.067    1799.403
    ##   Degrees of freedom                                45          45
    ##   P-value                                        0.000       0.000
    ##   Scaling correction factor                                  1.255
    ## 
    ## User Model versus Baseline Model:
    ## 
    ##   Comparative Fit Index (CFI)                    0.998       0.998
    ##   Tucker-Lewis Index (TLI)                       0.986       0.985
    ##                                                                   
    ##   Robust Comparative Fit Index (CFI)                         0.998
    ##   Robust Tucker-Lewis Index (TLI)                            0.987
    ## 
    ## Loglikelihood and Information Criteria:
    ## 
    ##   Loglikelihood user model (H0)             -18198.080  -18198.080
    ##   Loglikelihood unrestricted model (H1)     -18193.023  -18193.023
    ##                                                                   
    ##   Akaike (AIC)                               36494.159   36494.159
    ##   Bayesian (BIC)                             36723.275   36723.275
    ##   Sample-size adjusted Bayesian (BIC)        36567.674   36567.674
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.029       0.027
    ##   90 Percent confidence interval - lower         0.000       0.000
    ##   90 Percent confidence interval - upper         0.060       0.057
    ##   P-value RMSEA <= 0.05                          0.850       0.885
    ##                                                                   
    ##   Robust RMSEA                                               0.028
    ##   90 Percent confidence interval - lower                     0.000
    ##   90 Percent confidence interval - upper                     0.060
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.016       0.016
    ## 
    ## Parameter Estimates:
    ## 
    ##   Information                                 Expected
    ##   Information saturated (h1) model          Structured
    ##   Standard errors                           Robust.sem
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   y10_QI ~                                                              
    ##     y0_CATINC        -0.326    0.144   -2.268    0.023   -0.326   -0.052
    ##     y0_QI             1.316    0.042   30.981    0.000    1.316    0.755
    ##     y0_pbmi           0.096    0.022    4.307    0.000    0.096    0.097
    ##     y0_RACE           0.231    0.321    0.719    0.472    0.231    0.017
    ##     y5_AWARE          0.047    0.038    1.251    0.211    0.047    0.032
    ##     y5_DISTRST        0.146    0.063    2.331    0.020    0.146    0.071
    ##     y5_INEFCT        -0.050    0.066   -0.748    0.455   -0.050   -0.027
    ##     y5_SELWT         -0.449    0.278   -1.617    0.106   -0.449   -0.042
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
    ##    .y10_QI           15.963    1.285   12.419    0.000   15.963    0.336
    ##    .y5_AWARE         17.820    1.563   11.401    0.000   17.820    0.820
    ##    .y5_DISTRST        9.617    0.688   13.979    0.000    9.617    0.856
    ##    .y5_INEFCT        12.215    1.075   11.368    0.000   12.215    0.839
    ##    .y5_SELWT          0.343    0.018   19.269    0.000    0.343    0.814
    ##    .y3_ANXR          40.720    1.682   24.203    0.000   40.720    0.973
    ##    .y0_QI            14.079    0.870   16.186    0.000   14.079    0.899
    ##    .y0_pbmi          46.404    4.188   11.080    0.000   46.404    0.954
    ##    .y0_CATINC         1.102    0.043   25.847    0.000    1.102    0.916
    ##     y0_RACE           0.249    0.001  285.950    0.000    0.249    1.000
    ## 
    ## R-Square:
    ##                    Estimate
    ##     y10_QI            0.664
    ##     y5_AWARE          0.180
    ##     y5_DISTRST        0.144
    ##     y5_INEFCT         0.161
    ##     y5_SELWT          0.186
    ##     y3_ANXR           0.027
    ##     y0_QI             0.101
    ##     y0_pbmi           0.046
    ##     y0_CATINC         0.084

## Anxiety Psych Pruned Model - Parameter Export

``` r
standardizedSolution(
  fit_psych_anx_pruned, 
  type = "std.all", 
  se = TRUE, 
  zstat = TRUE, 
  pvalue = TRUE, 
  ci = TRUE, 
  level = 0.95
) %>%
  mutate_if(
    ~any(is.numeric(.)), 
    ~round(., 3)
  ) %>%
  write.table(
    "G:/My Drive/research/projects/phsr1/nghs1/nghs1_plots/anx_psych_table_std.csv", 
    sep =","
  )
```

## Anxiety Psych Pruned Model - All Data

``` r
fit_psych_anx_pruned_ad <-
  sem(
    mod_psych_anx_pruned, 
    data = semAllData, 
    estimator = "MLM"
  )

summary(
  fit_psych_anx_pruned_ad, 
  standardized = TRUE, 
  fit.measures = TRUE, 
  rsq = TRUE
)
```

    ## lavaan 0.6-5 ended normally after 128 iterations
    ## 
    ##   Estimator                                         ML
    ##   Optimization method                           NLMINB
    ##   Number of free parameters                         49
    ##                                                       
    ##   Number of observations                          2379
    ##                                                       
    ## Model Test User Model:
    ##                                               Standard      Robust
    ##   Test Statistic                                 8.264       7.916
    ##   Degrees of freedom                                 6           6
    ##   P-value (Chi-square)                           0.219       0.244
    ##   Scaling correction factor                                  1.044
    ##     for the Satorra-Bentler correction 
    ## 
    ## Model Test Baseline Model:
    ## 
    ##   Test statistic                              6472.018    5157.046
    ##   Degrees of freedom                                45          45
    ##   P-value                                        0.000       0.000
    ##   Scaling correction factor                                  1.255
    ## 
    ## User Model versus Baseline Model:
    ## 
    ##   Comparative Fit Index (CFI)                    1.000       1.000
    ##   Tucker-Lewis Index (TLI)                       0.997       0.997
    ##                                                                   
    ##   Robust Comparative Fit Index (CFI)                         1.000
    ##   Robust Tucker-Lewis Index (TLI)                            0.998
    ## 
    ## Loglikelihood and Information Criteria:
    ## 
    ##   Loglikelihood user model (H0)             -54433.855  -54433.855
    ##   Loglikelihood unrestricted model (H1)     -54429.723  -54429.723
    ##                                                                   
    ##   Akaike (AIC)                              108965.711  108965.711
    ##   Bayesian (BIC)                            109248.658  109248.658
    ##   Sample-size adjusted Bayesian (BIC)       109092.975  109092.975
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.013       0.012
    ##   90 Percent confidence interval - lower         0.000       0.000
    ##   90 Percent confidence interval - upper         0.031       0.030
    ##   P-value RMSEA <= 0.05                          1.000       1.000
    ##                                                                   
    ##   Robust RMSEA                                               0.012
    ##   90 Percent confidence interval - lower                     0.000
    ##   90 Percent confidence interval - upper                     0.031
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.009       0.009
    ## 
    ## Parameter Estimates:
    ## 
    ##   Information                                 Expected
    ##   Information saturated (h1) model          Structured
    ##   Standard errors                           Robust.sem
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   y10_QI ~                                                              
    ##     y0_CATINC        -0.421    0.082   -5.127    0.000   -0.421   -0.070
    ##     y0_QI             1.292    0.027   47.749    0.000    1.292    0.741
    ##     y0_pbmi           0.090    0.014    6.384    0.000    0.090    0.093
    ##     y0_RACE           0.477    0.187    2.554    0.011    0.477    0.036
    ##     y5_AWARE          0.050    0.024    2.127    0.033    0.050    0.035
    ##     y5_DISTRST        0.077    0.032    2.405    0.016    0.077    0.039
    ##     y5_INEFCT         0.037    0.035    1.072    0.284    0.037    0.021
    ##     y5_SELWT         -0.212    0.154   -1.377    0.168   -0.212   -0.020
    ##   y5_AWARE ~                                                            
    ##     y3_ANXR           0.254    0.015   16.988    0.000    0.254    0.355
    ##   y5_DISTRST ~                                                          
    ##     y3_ANXR           0.109    0.010   10.665    0.000    0.109    0.211
    ##   y5_INEFCT ~                                                           
    ##     y3_ANXR           0.208    0.013   15.891    0.000    0.208    0.349
    ##   y5_SELWT ~                                                            
    ##     y3_ANXR          -0.038    0.002  -20.008    0.000   -0.038   -0.389
    ##   y5_AWARE ~                                                            
    ##     y0_CATINC        -0.468    0.086   -5.455    0.000   -0.468   -0.112
    ##     y0_pbmi           0.019    0.014    1.373    0.170    0.019    0.028
    ##   y5_DISTRST ~                                                          
    ##     y0_CATINC        -0.279    0.062   -4.522    0.000   -0.279   -0.093
    ##     y0_pbmi           0.007    0.009    0.805    0.421    0.007    0.015
    ##   y5_INEFCT ~                                                           
    ##     y0_CATINC        -0.292    0.070   -4.190    0.000   -0.292   -0.084
    ##     y0_pbmi           0.032    0.012    2.686    0.007    0.032    0.057
    ##   y3_ANXR ~                                                             
    ##     y0_CATINC        -0.633    0.117   -5.418    0.000   -0.633   -0.109
    ##     y0_pbmi           0.096    0.019    5.049    0.000    0.096    0.102
    ##   y5_AWARE ~                                                            
    ##     y0_RACE           0.955    0.180    5.290    0.000    0.955    0.103
    ##   y5_DISTRST ~                                                          
    ##     y0_RACE           1.683    0.136   12.420    0.000    1.683    0.254
    ##   y5_INEFCT ~                                                           
    ##     y0_RACE          -0.366    0.162   -2.254    0.024   -0.366   -0.048
    ##   y5_AWARE ~                                                            
    ##     y0_QI             0.089    0.025    3.512    0.000    0.089    0.074
    ##   y5_INEFCT ~                                                           
    ##     y0_QI             0.081    0.021    3.841    0.000    0.081    0.081
    ##   y5_SELWT ~                                                            
    ##     y0_QI            -0.019    0.003   -5.649    0.000   -0.019   -0.114
    ##     y0_pbmi          -0.003    0.002   -1.526    0.127   -0.003   -0.031
    ##     y0_CATINC         0.011    0.011    0.982    0.326    0.011    0.019
    ##     y0_RACE           0.232    0.025    9.143    0.000    0.232    0.184
    ##   y0_QI ~                                                               
    ##     y0_pbmi           0.150    0.014   10.717    0.000    0.150    0.270
    ##     y0_CATINC         0.000                               0.000    0.000
    ##     y0_RACE           0.811    0.150    5.418    0.000    0.811    0.106
    ##   y0_pbmi ~                                                             
    ##     y0_RACE           2.839    0.274   10.361    0.000    2.839    0.207
    ##   y0_CATINC ~                                                           
    ##     y0_RACE          -0.721    0.043  -16.915    0.000   -0.721   -0.326
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##  .y5_AWARE ~~                                                           
    ##    .y5_DISTRST        2.127    0.336    6.322    0.000    2.127    0.167
    ##    .y5_INEFCT         6.797    0.573   11.853    0.000    6.797    0.462
    ##    .y5_SELWT         -0.496    0.057   -8.721    0.000   -0.496   -0.209
    ##  .y5_DISTRST ~~                                                         
    ##    .y5_INEFCT         3.909    0.346   11.281    0.000    3.909    0.361
    ##    .y5_SELWT         -0.388    0.041   -9.457    0.000   -0.388   -0.223
    ##  .y5_INEFCT ~~                                                          
    ##    .y5_SELWT         -0.945    0.062  -15.176    0.000   -0.945   -0.470
    ##  .y3_ANXR ~~                                                            
    ##    .y0_QI             0.000                               0.000    0.000
    ##     y0_RACE           0.000                               0.000    0.000
    ##  .y0_pbmi ~~                                                            
    ##    .y0_CATINC         0.000                               0.000    0.000
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .y10_QI           15.496    0.682   22.733    0.000   15.496    0.349
    ##    .y5_AWARE         17.340    0.840   20.648    0.000   17.340    0.814
    ##    .y5_DISTRST        9.379    0.406   23.110    0.000    9.379    0.854
    ##    .y5_INEFCT        12.468    0.772   16.141    0.000   12.468    0.849
    ##    .y5_SELWT          0.324    0.010   32.212    0.000    0.324    0.810
    ##    .y3_ANXR          40.550    0.947   42.820    0.000   40.550    0.976
    ##    .y0_QI            13.199    0.496   26.594    0.000   13.199    0.904
    ##    .y0_pbmi          44.981    2.313   19.444    0.000   44.981    0.957
    ##    .y0_CATINC         1.090    0.025   44.306    0.000    1.090    0.894
    ##     y0_RACE           0.250    0.000 1234.182    0.000    0.250    1.000
    ## 
    ## R-Square:
    ##                    Estimate
    ##     y10_QI            0.651
    ##     y5_AWARE          0.186
    ##     y5_DISTRST        0.146
    ##     y5_INEFCT         0.151
    ##     y5_SELWT          0.190
    ##     y3_ANXR           0.024
    ##     y0_QI             0.096
    ##     y0_pbmi           0.043
    ##     y0_CATINC         0.106

# Stress Behavior

## Stress Behavior Initial Model

``` r
mod_behav_stress_initial <-
  '
    #################
    ## Regressions ##
    #################  
     
    y10_QI ~ y0_CATINC + y0_QI + y0_pbmi + y0_RACE    

    y10_QI ~ y6_eat_SNKFOOD2 + y5_eat_STOPEAT + y4_eat_VITAMINS + y7_SECRET + y2_STRESS

    y6_eat_SNKFOOD2 + y5_eat_STOPEAT + y4_eat_VITAMINS + y7_SECRET ~ y2_STRESS

    y6_eat_SNKFOOD2 + y5_eat_STOPEAT + y4_eat_VITAMINS + y7_SECRET + y2_STRESS ~ y0_CATINC + y0_QI + y0_RACE + y0_pbmi

    y0_QI ~ y0_pbmi + y0_CATINC + y0_RACE

    y0_pbmi + y0_CATINC ~ y0_RACE

    #################
    ## Covariances ##
    #################  
  
    y6_eat_SNKFOOD2 ~~ y5_eat_STOPEAT + y4_eat_VITAMINS + y7_SECRET 

    y5_eat_STOPEAT ~~ y4_eat_VITAMINS + y7_SECRET 

    y4_eat_VITAMINS ~~ y7_SECRET 

    y0_pbmi ~~ y0_CATINC
    
  '
fit_behav_stress_initial <- 
  sem(
    mod_behav_stress_initial, 
    data = semTestingData %>% mutate(y5_eat_STOPEAT = as.factor(y5_eat_STOPEAT)), 
    estimator = "DWLS", 
    ordered = c("y5_eat_STOPEAT")
  )

summary(
  fit_behav_stress_initial, 
  standardized = TRUE, 
  fit.measures = TRUE
)
```

    ## lavaan 0.6-5 ended normally after 190 iterations
    ## 
    ##   Estimator                                       DWLS
    ##   Optimization method                           NLMINB
    ##   Number of free parameters                         62
    ##                                                       
    ##   Number of observations                           793
    ##                                                       
    ## Model Test User Model:
    ##                                                       
    ##   Test statistic                                 0.000
    ##   Degrees of freedom                                 0
    ## 
    ## Model Test Baseline Model:
    ## 
    ##   Test statistic                               880.175
    ##   Degrees of freedom                                36
    ##   P-value                                        0.000
    ## 
    ## User Model versus Baseline Model:
    ## 
    ##   Comparative Fit Index (CFI)                    1.000
    ##   Tucker-Lewis Index (TLI)                       1.000
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.000
    ##   90 Percent confidence interval - lower         0.000
    ##   90 Percent confidence interval - upper         0.000
    ##   P-value RMSEA <= 0.05                             NA
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.002
    ## 
    ## Parameter Estimates:
    ## 
    ##   Information                                 Expected
    ##   Information saturated (h1) model        Unstructured
    ##   Standard errors                             Standard
    ## 
    ## Regressions:
    ##                     Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   y10_QI ~                                                               
    ##     y0_CATINC         -0.395    0.288   -1.370    0.171   -0.395   -0.063
    ##     y0_QI              1.272    0.143    8.882    0.000    1.272    0.730
    ##     y0_pbmi            0.100    0.046    2.186    0.029    0.100    0.101
    ##     y0_RACE            0.511    0.684    0.747    0.455    0.511    0.037
    ##     y6_et_SNKFOOD2    -0.739    0.541   -1.367    0.172   -0.739   -0.066
    ##     y5_eat_STOPEAT    -0.420    0.490   -0.857    0.391   -0.420   -0.062
    ##     y4_et_VITAMINS     0.214    0.489    0.438    0.661    0.214    0.021
    ##     y7_SECRET          0.434    0.807    0.537    0.591    0.434    0.023
    ##     y2_STRESS         -0.000    0.046   -0.005    0.996   -0.000   -0.000
    ##   y6_eat_SNKFOOD2 ~                                                      
    ##     y2_STRESS         -0.010    0.003   -3.176    0.001   -0.010   -0.116
    ##   y5_eat_STOPEAT ~                                                       
    ##     y2_STRESS         -0.012    0.007   -1.651    0.099   -0.012   -0.084
    ##   y4_eat_VITAMINS ~                                                      
    ##     y2_STRESS          0.005    0.004    1.358    0.174    0.005    0.052
    ##   y7_SECRET ~                                                            
    ##     y2_STRESS          0.006    0.002    3.134    0.002    0.006    0.107
    ##   y6_eat_SNKFOOD2 ~                                                      
    ##     y0_CATINC         -0.032    0.023   -1.413    0.158   -0.032   -0.057
    ##     y0_QI             -0.017    0.006   -2.668    0.008   -0.017   -0.110
    ##     y0_RACE            0.201    0.047    4.251    0.000    0.201    0.163
    ##     y0_pbmi           -0.004    0.004   -0.988    0.323   -0.004   -0.042
    ##   y5_eat_STOPEAT ~                                                       
    ##     y0_CATINC          0.037    0.048    0.760    0.447    0.037    0.040
    ##     y0_QI             -0.105    0.013   -7.836    0.000   -0.105   -0.413
    ##     y0_RACE           -0.150    0.106   -1.415    0.157   -0.150   -0.074
    ##     y0_pbmi           -0.001    0.008   -0.181    0.857   -0.001   -0.010
    ##   y4_eat_VITAMINS ~                                                      
    ##     y0_CATINC         -0.010    0.024   -0.412    0.680   -0.010   -0.016
    ##     y0_QI             -0.018    0.007   -2.497    0.013   -0.018   -0.106
    ##     y0_RACE           -0.073    0.051   -1.409    0.159   -0.073   -0.054
    ##     y0_pbmi           -0.002    0.004   -0.499    0.618   -0.002   -0.022
    ##   y7_SECRET ~                                                            
    ##     y0_CATINC         -0.003    0.013   -0.272    0.785   -0.003   -0.010
    ##     y0_QI              0.001    0.004    0.208    0.835    0.001    0.009
    ##     y0_RACE            0.012    0.028    0.425    0.671    0.012    0.016
    ##     y0_pbmi           -0.000    0.002   -0.045    0.964   -0.000   -0.002
    ##   y2_STRESS ~                                                            
    ##     y0_CATINC         -0.871    0.253   -3.441    0.001   -0.871   -0.138
    ##     y0_QI              0.050    0.069    0.727    0.467    0.050    0.029
    ##     y0_RACE           -1.052    0.531   -1.983    0.047   -1.052   -0.076
    ##     y0_pbmi            0.090    0.041    2.205    0.027    0.090    0.091
    ##   y0_QI ~                                                                
    ##     y0_pbmi            0.165    0.018    9.250    0.000    0.165    0.291
    ##     y0_CATINC          0.135    0.137    0.982    0.326    0.135    0.037
    ##     y0_RACE            0.766    0.317    2.412    0.016    0.766    0.097
    ##   y0_pbmi ~                                                              
    ##     y0_RACE            2.981    0.499    5.976    0.000    2.981    0.214
    ##   y0_CATINC ~                                                            
    ##     y0_RACE           -0.637    0.084   -7.581    0.000   -0.637   -0.290
    ## 
    ## Covariances:
    ##                      Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##  .y6_eat_SNKFOOD2 ~~                                                      
    ##    .y5_eat_STOPEAT     -0.035    0.030   -1.145    0.252   -0.035   -0.065
    ##    .y4_et_VITAMINS     -0.024    0.014   -1.690    0.091   -0.024   -0.059
    ##    .y7_SECRET          -0.022    0.008   -2.866    0.004   -0.022   -0.101
    ##  .y5_eat_STOPEAT ~~                                                       
    ##    .y4_et_VITAMINS      0.001    0.034    0.026    0.979    0.001    0.001
    ##    .y7_SECRET           0.047    0.018    2.665    0.008    0.047    0.145
    ##  .y4_eat_VITAMINS ~~                                                      
    ##    .y7_SECRET           0.009    0.009    1.009    0.313    0.009    0.037
    ##  .y0_pbmi ~~                                                              
    ##    .y0_CATINC          -0.538    0.251   -2.141    0.032   -0.538   -0.075
    ## 
    ## Intercepts:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .y10_QI           -0.806    3.479   -0.232    0.817   -0.806   -0.117
    ##    .y6_et_SNKFOOD2    2.824    0.177   15.932    0.000    2.824    4.593
    ##    .y5_eat_STOPEAT    0.000                               0.000    0.000
    ##    .y4_et_VITAMINS    1.961    0.195   10.072    0.000    1.961    2.916
    ##    .y7_SECRET         0.953    0.105    9.042    0.000    0.953    2.614
    ##    .y2_STRESS        25.752    1.710   15.063    0.000   25.752    3.729
    ##    .y0_QI            11.979    0.932   12.857    0.000   11.979    3.026
    ##    .y0_pbmi          26.669    0.838   31.814    0.000   26.669    3.825
    ##    .y0_CATINC         3.897    0.151   25.781    0.000    3.897    3.552
    ## 
    ## Thresholds:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##     y5_t_STOPEAT|1   -2.994    0.387   -7.731    0.000   -2.994   -2.961
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .y10_QI           15.919    4.056    3.925    0.000   15.919    0.334
    ##    .y6_et_SNKFOOD2    0.356    0.024   14.766    0.000    0.356    0.943
    ##    .y5_eat_STOPEAT    0.814                               0.814    0.796
    ##    .y4_et_VITAMINS    0.443    0.035   12.661    0.000    0.443    0.980
    ##    .y7_SECRET         0.131    0.007   19.272    0.000    0.131    0.988
    ##    .y2_STRESS        46.281    2.233   20.728    0.000   46.281    0.970
    ##    .y0_QI            14.059    0.810   17.348    0.000   14.059    0.897
    ##    .y0_pbmi          46.404    1.647   28.177    0.000   46.404    0.954
    ##    .y0_CATINC         1.102    0.089   12.331    0.000    1.102    0.916
    ## 
    ## Scales y*:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##     y5_eat_STOPEAT    1.000                               1.000    1.000

## Stress Behavior Pruned Model

``` r
mod_behav_stress_pruned <-
  '
    #################
    ## Regressions ##
    #################  
     
    y10_QI ~ y0_CATINC + y0_QI + y0_pbmi + y0_RACE    

    y10_QI ~ y6_eat_SNKFOOD2 + y5_eat_STOPEAT + y7_SECRET 

    y6_eat_SNKFOOD2 ~ y2_STRESS + y0_CATINC + y0_QI + y0_RACE + y0_pbmi
    
    y5_eat_STOPEAT ~ y2_STRESS + y0_QI + y0_RACE
    
    y7_SECRET ~ y2_STRESS 

    y2_STRESS ~ y0_CATINC + y0_pbmi + y0_RACE

    y0_QI ~ y0_pbmi + 0*y0_CATINC + y0_RACE

    y0_pbmi + y0_CATINC ~ y0_RACE

    #################
    ## Covariances ##
    #################  
  
    y6_eat_SNKFOOD2 ~~ 0*y5_eat_STOPEAT + y7_SECRET 

    y5_eat_STOPEAT ~~ 0*y7_SECRET 

    y0_pbmi ~~ 0*y0_CATINC
    
  '

fit_behav_stress_pruned <- 
  sem(
    mod_behav_stress_pruned, 
    data = semTestingData %>% mutate(y5_eat_STOPEAT = as.factor(y5_eat_STOPEAT)), 
    estimator = "DWLS", 
    ordered = c("y5_eat_STOPEAT")
  )

summary(
  fit_behav_stress_pruned, 
  standardized = TRUE, 
  fit.measures = TRUE
)
```

    ## lavaan 0.6-5 ended normally after 134 iterations
    ## 
    ##   Estimator                                       DWLS
    ##   Optimization method                           NLMINB
    ##   Number of free parameters                         39
    ##                                                       
    ##   Number of observations                           793
    ##                                                       
    ## Model Test User Model:
    ##                                                       
    ##   Test statistic                                15.081
    ##   Degrees of freedom                                12
    ##   P-value (Chi-square)                           0.237
    ## 
    ## Model Test Baseline Model:
    ## 
    ##   Test statistic                               861.211
    ##   Degrees of freedom                                28
    ##   P-value                                        0.000
    ## 
    ## User Model versus Baseline Model:
    ## 
    ##   Comparative Fit Index (CFI)                    0.996
    ##   Tucker-Lewis Index (TLI)                       0.991
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.018
    ##   90 Percent confidence interval - lower         0.000
    ##   90 Percent confidence interval - upper         0.043
    ##   P-value RMSEA <= 0.05                          0.988
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.027
    ## 
    ## Parameter Estimates:
    ## 
    ##   Information                                 Expected
    ##   Information saturated (h1) model        Unstructured
    ##   Standard errors                             Standard
    ## 
    ## Regressions:
    ##                     Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   y10_QI ~                                                               
    ##     y0_CATINC         -0.405    0.216   -1.875    0.061   -0.405   -0.064
    ##     y0_QI              1.259    0.136    9.250    0.000    1.259    0.723
    ##     y0_pbmi            0.104    0.045    2.337    0.019    0.104    0.105
    ##     y0_RACE            0.511    0.674    0.758    0.448    0.511    0.037
    ##     y6_et_SNKFOOD2    -0.846    0.513   -1.649    0.099   -0.846   -0.075
    ##     y5_eat_STOPEAT    -0.431    0.457   -0.944    0.345   -0.431   -0.063
    ##     y7_SECRET          0.445    0.575    0.775    0.438    0.445    0.024
    ##   y6_eat_SNKFOOD2 ~                                                      
    ##     y2_STRESS         -0.010    0.003   -3.196    0.001   -0.010   -0.117
    ##     y0_CATINC         -0.031    0.022   -1.412    0.158   -0.031   -0.056
    ##     y0_QI             -0.016    0.006   -2.565    0.010   -0.016   -0.100
    ##     y0_RACE            0.198    0.047    4.185    0.000    0.198    0.161
    ##     y0_pbmi           -0.003    0.004   -0.920    0.358   -0.003   -0.039
    ##   y5_eat_STOPEAT ~                                                       
    ##     y2_STRESS         -0.012    0.007   -1.866    0.062   -0.012   -0.085
    ##     y0_QI             -0.105    0.011   -9.342    0.000   -0.105   -0.413
    ##     y0_RACE           -0.178    0.101   -1.764    0.078   -0.178   -0.088
    ##   y7_SECRET ~                                                            
    ##     y2_STRESS          0.005    0.002    3.244    0.001    0.005    0.104
    ##   y2_STRESS ~                                                            
    ##     y0_CATINC         -0.937    0.249   -3.763    0.000   -0.937   -0.149
    ##     y0_pbmi            0.119    0.033    3.644    0.000    0.119    0.120
    ##     y0_RACE           -1.094    0.531   -2.059    0.039   -1.094   -0.079
    ##   y0_QI ~                                                                
    ##     y0_pbmi            0.166    0.017    9.680    0.000    0.166    0.292
    ##     y0_CATINC          0.000                               0.000    0.000
    ##     y0_RACE            0.678    0.307    2.207    0.027    0.678    0.085
    ##   y0_pbmi ~                                                              
    ##     y0_RACE            2.981    0.499    5.976    0.000    2.981    0.214
    ##   y0_CATINC ~                                                            
    ##     y0_RACE           -0.637    0.084   -7.581    0.000   -0.637   -0.290
    ## 
    ## Covariances:
    ##                      Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##  .y6_eat_SNKFOOD2 ~~                                                      
    ##    .y5_eat_STOPEAT      0.000                               0.000    0.000
    ##    .y7_SECRET          -0.022    0.008   -2.924    0.003   -0.022   -0.102
    ##  .y5_eat_STOPEAT ~~                                                       
    ##    .y7_SECRET           0.000                               0.000    0.000
    ##  .y0_pbmi ~~                                                              
    ##    .y0_CATINC           0.000                               0.000    0.000
    ## 
    ## Intercepts:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .y10_QI           -0.160    2.710   -0.059    0.953   -0.160   -0.023
    ##    .y6_et_SNKFOOD2    2.790    0.171   16.291    0.000    2.790    4.538
    ##    .y5_eat_STOPEAT    0.000                               0.000    0.000
    ##    .y7_SECRET         0.954    0.066   14.361    0.000    0.954    2.619
    ##    .y2_STRESS        26.083    1.539   16.945    0.000   26.083    3.775
    ##    .y0_QI            12.488    0.693   18.018    0.000   12.488    3.151
    ##    .y0_pbmi          26.669    0.838   31.814    0.000   26.669    3.827
    ##    .y0_CATINC         3.897    0.151   25.781    0.000    3.897    3.553
    ## 
    ## Thresholds:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##     y5_t_STOPEAT|1   -3.102    0.294  -10.565    0.000   -3.102   -3.068
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .y10_QI           15.977    4.024    3.971    0.000   15.977    0.336
    ##    .y6_et_SNKFOOD2    0.357    0.024   14.842    0.000    0.357    0.946
    ##    .y5_eat_STOPEAT    0.820                               0.820    0.802
    ##    .y7_SECRET         0.131    0.007   19.312    0.000    0.131    0.989
    ##    .y2_STRESS        46.104    2.242   20.560    0.000   46.104    0.966
    ##    .y0_QI            14.080    0.793   17.760    0.000   14.080    0.897
    ##    .y0_pbmi          46.350    1.645   28.174    0.000   46.350    0.954
    ##    .y0_CATINC         1.102    0.089   12.326    0.000    1.102    0.916
    ## 
    ## Scales y*:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##     y5_eat_STOPEAT    1.000                               1.000    1.000

## Stress Behavior Pruned Model - Parameter Export

``` r
standardizedSolution(
  fit_behav_stress_pruned, 
  type = "std.all", 
  se = TRUE, 
  zstat = TRUE, 
  pvalue = TRUE, 
  ci = TRUE, 
  level = 0.95
) %>%
  mutate_if(
    ~any(is.numeric(.)), 
    ~round(., 3)
  ) %>%
  write.table(
    "G:/My Drive/research/projects/phsr1/nghs1/nghs1_plots/stress_behav_table_std.csv", 
    sep =","
  )
```

## Stress Behavior Pruned Model - All Data

``` r
fit_behav_stress_pruned_ad <-
  sem(
    mod_behav_stress_pruned, 
    data = semAllData %>% mutate(y5_eat_STOPEAT = as.factor(y5_eat_STOPEAT)), 
    estimator = "DWLS", 
    ordered = c("y5_eat_STOPEAT")
  )

summary(
  fit_behav_stress_pruned_ad,
  standardized = TRUE, 
  fit.measures = TRUE
)
```

    ## lavaan 0.6-5 ended normally after 141 iterations
    ## 
    ##   Estimator                                       DWLS
    ##   Optimization method                           NLMINB
    ##   Number of free parameters                         39
    ##                                                       
    ##   Number of observations                          2379
    ##                                                       
    ## Model Test User Model:
    ##                                                       
    ##   Test statistic                                23.717
    ##   Degrees of freedom                                12
    ##   P-value (Chi-square)                           0.022
    ## 
    ## Model Test Baseline Model:
    ## 
    ##   Test statistic                              2160.714
    ##   Degrees of freedom                                28
    ##   P-value                                        0.000
    ## 
    ## User Model versus Baseline Model:
    ## 
    ##   Comparative Fit Index (CFI)                    0.995
    ##   Tucker-Lewis Index (TLI)                       0.987
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.020
    ##   90 Percent confidence interval - lower         0.007
    ##   90 Percent confidence interval - upper         0.032
    ##   P-value RMSEA <= 0.05                          1.000
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.020
    ## 
    ## Parameter Estimates:
    ## 
    ##   Information                                 Expected
    ##   Information saturated (h1) model        Unstructured
    ##   Standard errors                             Standard
    ## 
    ## Regressions:
    ##                     Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   y10_QI ~                                                               
    ##     y0_CATINC         -0.593    0.124   -4.765    0.000   -0.593   -0.098
    ##     y0_QI              1.254    0.071   17.740    0.000    1.254    0.717
    ##     y0_pbmi            0.092    0.025    3.757    0.000    0.092    0.095
    ##     y0_RACE            0.622    0.376    1.655    0.098    0.622    0.047
    ##     y6_et_SNKFOOD2    -0.767    0.291   -2.638    0.008   -0.767   -0.071
    ##     y5_eat_STOPEAT    -0.455    0.232   -1.959    0.050   -0.455   -0.069
    ##     y7_SECRET          0.951    0.338    2.814    0.005    0.951    0.052
    ##   y6_eat_SNKFOOD2 ~                                                      
    ##     y2_STRESS         -0.006    0.002   -3.491    0.000   -0.006   -0.072
    ##     y0_CATINC         -0.016    0.012   -1.262    0.207   -0.016   -0.028
    ##     y0_QI             -0.021    0.004   -5.503    0.000   -0.021   -0.128
    ##     y0_RACE            0.200    0.027    7.291    0.000    0.200    0.163
    ##     y0_pbmi           -0.000    0.002   -0.056    0.955   -0.000   -0.001
    ##   y5_eat_STOPEAT ~                                                       
    ##     y2_STRESS         -0.015    0.004   -3.849    0.000   -0.015   -0.104
    ##     y0_QI             -0.086    0.007  -12.894    0.000   -0.086   -0.324
    ##     y0_RACE           -0.146    0.058   -2.522    0.012   -0.146   -0.073
    ##   y7_SECRET ~                                                            
    ##     y2_STRESS          0.005    0.001    5.764    0.000    0.005    0.102
    ##   y2_STRESS ~                                                            
    ##     y0_CATINC         -0.623    0.137   -4.548    0.000   -0.623   -0.101
    ##     y0_pbmi            0.085    0.020    4.320    0.000    0.085    0.085
    ##     y0_RACE           -0.614    0.304   -2.022    0.043   -0.614   -0.045
    ##   y0_QI ~                                                                
    ##     y0_pbmi            0.155    0.010   15.683    0.000    0.155    0.278
    ##     y0_CATINC          0.000                               0.000    0.000
    ##     y0_RACE            0.799    0.169    4.732    0.000    0.799    0.105
    ##   y0_pbmi ~                                                              
    ##     y0_RACE            2.839    0.284   10.006    0.000    2.839    0.207
    ##   y0_CATINC ~                                                            
    ##     y0_RACE           -0.721    0.049  -14.691    0.000   -0.721   -0.327
    ## 
    ## Covariances:
    ##                      Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##  .y6_eat_SNKFOOD2 ~~                                                      
    ##    .y5_eat_STOPEAT      0.000                               0.000    0.000
    ##    .y7_SECRET          -0.013    0.004   -2.960    0.003   -0.013   -0.060
    ##  .y5_eat_STOPEAT ~~                                                       
    ##    .y7_SECRET           0.000                               0.000    0.000
    ##  .y0_pbmi ~~                                                              
    ##    .y0_CATINC           0.000                               0.000    0.000
    ## 
    ## Intercepts:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .y10_QI           -0.048    1.516   -0.032    0.975   -0.048   -0.007
    ##    .y6_et_SNKFOOD2    2.637    0.098   26.836    0.000    2.637    4.293
    ##    .y5_eat_STOPEAT    0.000                               0.000    0.000
    ##    .y7_SECRET         0.966    0.037   26.381    0.000    0.966    2.656
    ##    .y2_STRESS        25.104    0.889   28.251    0.000   25.104    3.678
    ##    .y0_QI            12.543    0.389   32.206    0.000   12.543    3.288
    ##    .y0_pbmi          26.821    0.476   56.391    0.000   26.821    3.914
    ##    .y0_CATINC         4.006    0.088   45.506    0.000    4.006    3.629
    ## 
    ## Thresholds:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##     y5_t_STOPEAT|1   -2.823    0.172  -16.435    0.000   -2.823   -2.801
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .y10_QI           15.153    2.124    7.133    0.000   15.153    0.341
    ##    .y6_et_SNKFOOD2    0.361    0.014   25.940    0.000    0.361    0.955
    ##    .y5_eat_STOPEAT    0.883                               0.883    0.869
    ##    .y7_SECRET         0.131    0.004   32.150    0.000    0.131    0.990
    ##    .y2_STRESS        45.841    1.199   38.245    0.000   45.841    0.984
    ##    .y0_QI            13.097    0.416   31.450    0.000   13.097    0.900
    ##    .y0_pbmi          44.937    0.914   49.182    0.000   44.937    0.957
    ##    .y0_CATINC         1.089    0.050   21.673    0.000    1.089    0.893
    ## 
    ## Scales y*:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##     y5_eat_STOPEAT    1.000                               1.000    1.000

# Anxiety Behavior

## Anxiety Behavior Initial Model

``` r
mod_behav_anx_initial <-
  '
    #################
    ## Regressions ##
    #################  
     
    y10_QI ~ y0_CATINC + y0_QI + y0_pbmi + y0_RACE    

    y10_QI ~ y6_eat_SNKFOOD2 + y5_eat_STOPEAT + y4_eat_VITAMINS + y7_SECRET + y3_ANXR

    y6_eat_SNKFOOD2 + y5_eat_STOPEAT + y4_eat_VITAMINS + y7_SECRET ~ y3_ANXR

    y6_eat_SNKFOOD2 + y5_eat_STOPEAT + y4_eat_VITAMINS + y7_SECRET + y3_ANXR ~ y0_CATINC + y0_QI + y0_RACE + y0_pbmi

    y0_QI ~ y0_pbmi + y0_CATINC + y0_RACE

    y0_pbmi + y0_CATINC ~ y0_RACE

    #################
    ## Covariances ##
    #################  
  
    y6_eat_SNKFOOD2 ~~ y5_eat_STOPEAT + y4_eat_VITAMINS + y7_SECRET 

    y5_eat_STOPEAT ~~ y4_eat_VITAMINS + y7_SECRET 

    y4_eat_VITAMINS ~~ y7_SECRET 

    y0_pbmi ~~ y0_CATINC
    
  '

fit_behav_anx_initial <- 
  sem(
    mod_behav_anx_initial, 
    data = semTrainingData %>% mutate(y5_eat_STOPEAT = as.factor(y5_eat_STOPEAT)), 
    estimator = "DWLS", 
    ordered = c("y5_eat_STOPEAT")
  )

summary(
  fit_behav_anx_initial, 
  standardized = TRUE, 
  fit.measures = TRUE
)
```

    ## lavaan 0.6-5 ended normally after 189 iterations
    ## 
    ##   Estimator                                       DWLS
    ##   Optimization method                           NLMINB
    ##   Number of free parameters                         62
    ##                                                       
    ##   Number of observations                           793
    ##                                                       
    ## Model Test User Model:
    ##                                                       
    ##   Test statistic                                 0.000
    ##   Degrees of freedom                                 0
    ## 
    ## Model Test Baseline Model:
    ## 
    ##   Test statistic                               751.170
    ##   Degrees of freedom                                36
    ##   P-value                                        0.000
    ## 
    ## User Model versus Baseline Model:
    ## 
    ##   Comparative Fit Index (CFI)                    1.000
    ##   Tucker-Lewis Index (TLI)                       1.000
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.000
    ##   90 Percent confidence interval - lower         0.000
    ##   90 Percent confidence interval - upper         0.000
    ##   P-value RMSEA <= 0.05                             NA
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.002
    ## 
    ## Parameter Estimates:
    ## 
    ##   Information                                 Expected
    ##   Information saturated (h1) model        Unstructured
    ##   Standard errors                             Standard
    ## 
    ## Regressions:
    ##                     Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   y10_QI ~                                                               
    ##     y0_CATINC         -0.423    0.255   -1.655    0.098   -0.423   -0.073
    ##     y0_QI              1.251    0.119   10.483    0.000    1.251    0.727
    ##     y0_pbmi            0.080    0.037    2.164    0.030    0.080    0.090
    ##     y0_RACE            0.907    0.629    1.442    0.149    0.907    0.072
    ##     y6_et_SNKFOOD2    -0.527    0.470   -1.122    0.262   -0.527   -0.052
    ##     y5_eat_STOPEAT    -0.381    0.447   -0.854    0.393   -0.381   -0.061
    ##     y4_et_VITAMINS     0.103    0.420    0.245    0.807    0.103    0.011
    ##     y7_SECRET          0.554    0.817    0.678    0.498    0.554    0.032
    ##     y3_ANXR            0.025    0.053    0.464    0.643    0.025    0.025
    ##   y6_eat_SNKFOOD2 ~                                                      
    ##     y3_ANXR            0.001    0.004    0.180    0.857    0.001    0.007
    ##   y5_eat_STOPEAT ~                                                       
    ##     y3_ANXR           -0.043    0.009   -4.985    0.000   -0.043   -0.271
    ##   y4_eat_VITAMINS ~                                                      
    ##     y3_ANXR            0.002    0.004    0.377    0.706    0.002    0.014
    ##   y7_SECRET ~                                                            
    ##     y3_ANXR            0.009    0.002    3.604    0.000    0.009    0.149
    ##   y6_eat_SNKFOOD2 ~                                                      
    ##     y0_CATINC         -0.029    0.022   -1.307    0.191   -0.029   -0.051
    ##     y0_QI             -0.017    0.007   -2.413    0.016   -0.017   -0.097
    ##     y0_RACE            0.157    0.048    3.246    0.001    0.157    0.126
    ##     y0_pbmi            0.000    0.003    0.076    0.939    0.000    0.003
    ##   y5_eat_STOPEAT ~                                                       
    ##     y0_CATINC          0.118    0.050    2.358    0.018    0.118    0.127
    ##     y0_QI             -0.081    0.014   -5.842    0.000   -0.081   -0.297
    ##     y0_RACE            0.087    0.111    0.783    0.434    0.087    0.043
    ##     y0_pbmi           -0.014    0.007   -1.866    0.062   -0.014   -0.096
    ##   y4_eat_VITAMINS ~                                                      
    ##     y0_CATINC         -0.016    0.024   -0.678    0.498   -0.016   -0.026
    ##     y0_QI             -0.016    0.007   -2.258    0.024   -0.016   -0.090
    ##     y0_RACE           -0.069    0.052   -1.320    0.187   -0.069   -0.051
    ##     y0_pbmi            0.003    0.003    0.969    0.332    0.003    0.034
    ##   y7_SECRET ~                                                            
    ##     y0_CATINC          0.006    0.015    0.413    0.680    0.006    0.018
    ##     y0_QI              0.000    0.004    0.070    0.944    0.000    0.003
    ##     y0_RACE           -0.017    0.030   -0.558    0.577   -0.017   -0.022
    ##     y0_pbmi           -0.000    0.002   -0.079    0.937   -0.000   -0.003
    ##   y3_ANXR ~                                                              
    ##     y0_CATINC         -0.612    0.226   -2.706    0.007   -0.612   -0.105
    ##     y0_QI             -0.041    0.067   -0.606    0.545   -0.041   -0.023
    ##     y0_RACE           -0.126    0.499   -0.252    0.801   -0.126   -0.010
    ##     y0_pbmi            0.099    0.032    3.138    0.002    0.099    0.110
    ##   y0_QI ~                                                                
    ##     y0_pbmi            0.121    0.016    7.789    0.000    0.121    0.234
    ##     y0_CATINC          0.146    0.115    1.270    0.204    0.146    0.043
    ##     y0_RACE            1.125    0.287    3.919    0.000    1.125    0.154
    ##   y0_pbmi ~                                                              
    ##     y0_RACE            2.994    0.512    5.842    0.000    2.994    0.212
    ##   y0_CATINC ~                                                            
    ##     y0_RACE           -0.709    0.083   -8.559    0.000   -0.709   -0.326
    ## 
    ## Covariances:
    ##                      Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##  .y6_eat_SNKFOOD2 ~~                                                      
    ##    .y5_eat_STOPEAT     -0.013    0.033   -0.401    0.689   -0.013   -0.024
    ##    .y4_et_VITAMINS     -0.041    0.014   -2.827    0.005   -0.041   -0.099
    ##    .y7_SECRET          -0.013    0.008   -1.756    0.079   -0.013   -0.060
    ##  .y5_eat_STOPEAT ~~                                                       
    ##    .y4_et_VITAMINS     -0.020    0.035   -0.574    0.566   -0.020   -0.034
    ##    .y7_SECRET           0.009    0.019    0.451    0.652    0.009    0.026
    ##  .y4_eat_VITAMINS ~~                                                      
    ##    .y7_SECRET           0.020    0.009    2.279    0.023    0.020    0.082
    ##  .y0_pbmi ~~                                                              
    ##    .y0_CATINC          -0.104    0.246   -0.424    0.672   -0.104   -0.015
    ## 
    ## Intercepts:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .y10_QI           -0.997    2.818   -0.354    0.723   -0.997   -0.158
    ##    .y6_et_SNKFOOD2    2.490    0.159   15.690    0.000    2.490    3.982
    ##    .y5_eat_STOPEAT    0.000                               0.000    0.000
    ##    .y4_et_VITAMINS    1.871    0.169   11.053    0.000    1.871    2.779
    ##    .y7_SECRET         1.050    0.103   10.227    0.000    1.050    2.849
    ##    .y3_ANXR          10.097    1.529    6.604    0.000   10.097    1.587
    ##    .y0_QI            12.535    0.784   15.986    0.000   12.535    3.422
    ##    .y0_pbmi          26.473    0.863   30.672    0.000   26.473    3.740
    ##    .y0_CATINC         4.038    0.150   26.982    0.000    4.038    3.711
    ## 
    ## Thresholds:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##     y5_t_STOPEAT|1   -2.595    0.349   -7.437    0.000   -2.595   -2.586
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .y10_QI           13.593    3.180    4.274    0.000   13.593    0.343
    ##    .y6_et_SNKFOOD2    0.381    0.026   14.867    0.000    0.381    0.973
    ##    .y5_eat_STOPEAT    0.795                               0.795    0.789
    ##    .y4_et_VITAMINS    0.448    0.036   12.610    0.000    0.448    0.989
    ##    .y7_SECRET         0.133    0.008   16.822    0.000    0.133    0.978
    ##    .y3_ANXR          39.528    2.788   14.177    0.000   39.528    0.977
    ##    .y0_QI            12.214    0.650   18.802    0.000   12.214    0.910
    ##    .y0_pbmi          47.862    1.583   30.230    0.000   47.862    0.955
    ##    .y0_CATINC         1.058    0.083   12.703    0.000    1.058    0.894
    ## 
    ## Scales y*:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##     y5_eat_STOPEAT    1.000                               1.000    1.000

## Anxiety Behavior Pruned Model

``` r
mod_behav_anx_pruned <-

  '
    #################
    ## Regressions ##
    #################  
     
    y10_QI ~ y0_CATINC + y0_QI + y0_pbmi + y0_RACE    

    y10_QI ~ y6_eat_SNKFOOD2 + y5_eat_STOPEAT + y7_SECRET 

    y6_eat_SNKFOOD2 ~ y0_CATINC + y0_QI + y0_RACE
    
    y5_eat_STOPEAT ~ y3_ANXR + y0_CATINC + y0_QI + y0_pbmi
    
    y7_SECRET ~ y3_ANXR 

    y3_ANXR ~ y0_CATINC + y0_pbmi

    y0_QI ~ y0_pbmi + 0*y0_CATINC + y0_RACE

    y0_pbmi + y0_CATINC ~ y0_RACE

    #################
    ## Covariances ##
    #################  
  
    y6_eat_SNKFOOD2 ~~ 0*y5_eat_STOPEAT + y7_SECRET 

    y5_eat_STOPEAT ~~ 0*y7_SECRET 

    y0_pbmi ~~ 0*y0_CATINC
    
  '

fit_behav_anx_pruned <- 
  sem(
    mod_behav_anx_pruned, 
    data = semTestingData %>% mutate(y5_eat_STOPEAT = as.factor(y5_eat_STOPEAT)), 
    estimator = "DWLS", 
    ordered = c("y5_eat_STOPEAT")
  )

summary(
  fit_behav_anx_pruned, 
  standardized = TRUE, 
  fit.measures = TRUE
)
```

    ## lavaan 0.6-5 ended normally after 124 iterations
    ## 
    ##   Estimator                                       DWLS
    ##   Optimization method                           NLMINB
    ##   Number of free parameters                         37
    ##                                                       
    ##   Number of observations                           793
    ##                                                       
    ## Model Test User Model:
    ##                                                       
    ##   Test statistic                                32.855
    ##   Degrees of freedom                                14
    ##   P-value (Chi-square)                           0.003
    ## 
    ## Model Test Baseline Model:
    ## 
    ##   Test statistic                               896.673
    ##   Degrees of freedom                                28
    ##   P-value                                        0.000
    ## 
    ## User Model versus Baseline Model:
    ## 
    ##   Comparative Fit Index (CFI)                    0.978
    ##   Tucker-Lewis Index (TLI)                       0.957
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.041
    ##   90 Percent confidence interval - lower         0.023
    ##   90 Percent confidence interval - upper         0.060
    ##   P-value RMSEA <= 0.05                          0.764
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.036
    ## 
    ## Parameter Estimates:
    ## 
    ##   Information                                 Expected
    ##   Information saturated (h1) model        Unstructured
    ##   Standard errors                             Standard
    ## 
    ## Regressions:
    ##                     Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   y10_QI ~                                                               
    ##     y0_CATINC         -0.385    0.219   -1.761    0.078   -0.385   -0.061
    ##     y0_QI              1.248    0.134    9.276    0.000    1.248    0.717
    ##     y0_pbmi            0.108    0.045    2.425    0.015    0.108    0.109
    ##     y0_RACE            0.411    0.670    0.614    0.539    0.411    0.030
    ##     y6_et_SNKFOOD2    -0.715    0.517   -1.382    0.167   -0.715   -0.064
    ##     y5_eat_STOPEAT    -0.475    0.440   -1.079    0.281   -0.475   -0.069
    ##     y7_SECRET          0.436    0.577    0.756    0.450    0.436    0.023
    ##   y6_eat_SNKFOOD2 ~                                                      
    ##     y0_CATINC         -0.016    0.021   -0.772    0.440   -0.016   -0.029
    ##     y0_QI             -0.021    0.005   -4.036    0.000   -0.021   -0.135
    ##     y0_RACE            0.208    0.046    4.508    0.000    0.208    0.169
    ##   y5_eat_STOPEAT ~                                                       
    ##     y3_ANXR           -0.029    0.008   -3.778    0.000   -0.029   -0.190
    ##     y0_CATINC          0.039    0.044    0.898    0.369    0.039    0.043
    ##     y0_QI             -0.103    0.013   -7.856    0.000   -0.103   -0.407
    ##     y0_pbmi            0.000    0.008    0.016    0.987    0.000    0.001
    ##   y7_SECRET ~                                                            
    ##     y3_ANXR            0.009    0.002    4.290    0.000    0.009    0.159
    ##   y3_ANXR ~                                                              
    ##     y0_CATINC         -0.659    0.217   -3.030    0.002   -0.659   -0.111
    ##     y0_pbmi            0.147    0.033    4.464    0.000    0.147    0.157
    ##   y0_QI ~                                                                
    ##     y0_pbmi            0.169    0.018    9.646    0.000    0.169    0.296
    ##     y0_CATINC          0.000                               0.000    0.000
    ##     y0_RACE            0.801    0.296    2.711    0.007    0.801    0.101
    ##   y0_pbmi ~                                                              
    ##     y0_RACE            2.876    0.493    5.830    0.000    2.876    0.207
    ##   y0_CATINC ~                                                            
    ##     y0_RACE           -0.628    0.083   -7.521    0.000   -0.628   -0.285
    ## 
    ## Covariances:
    ##                      Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##  .y6_eat_SNKFOOD2 ~~                                                      
    ##    .y5_eat_STOPEAT      0.000                               0.000    0.000
    ##    .y7_SECRET          -0.025    0.007   -3.300    0.001   -0.025   -0.114
    ##  .y5_eat_STOPEAT ~~                                                       
    ##    .y7_SECRET           0.000                               0.000    0.000
    ##  .y0_pbmi ~~                                                              
    ##    .y0_CATINC           0.000                               0.000    0.000
    ## 
    ## Intercepts:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .y10_QI           -0.405    2.720   -0.149    0.882   -0.405   -0.059
    ##    .y6_et_SNKFOOD2    2.464    0.139   17.689    0.000    2.464    4.007
    ##    .y5_eat_STOPEAT    0.000                               0.000    0.000
    ##    .y7_SECRET         1.002    0.055   18.233    0.000    1.002    2.751
    ##    .y3_ANXR           9.063    1.477    6.138    0.000    9.063    1.389
    ##    .y0_QI            12.407    0.700   17.720    0.000   12.407    3.127
    ##    .y0_pbmi          26.669    0.838   31.814    0.000   26.669    3.836
    ##    .y0_CATINC         3.897    0.151   25.781    0.000    3.897    3.547
    ## 
    ## Thresholds:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##     y5_t_STOPEAT|1   -2.892    0.324   -8.937    0.000   -2.892   -2.880
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .y10_QI           16.210    3.993    4.060    0.000   16.210    0.341
    ##    .y6_et_SNKFOOD2    0.362    0.024   15.124    0.000    0.362    0.957
    ##    .y5_eat_STOPEAT    0.791                               0.791    0.785
    ##    .y7_SECRET         0.129    0.007   18.727    0.000    0.129    0.975
    ##    .y3_ANXR          40.886    2.894   14.130    0.000   40.886    0.961
    ##    .y0_QI            14.005    0.804   17.411    0.000   14.005    0.890
    ##    .y0_pbmi          46.260    1.644   28.147    0.000   46.260    0.957
    ##    .y0_CATINC         1.109    0.089   12.434    0.000    1.109    0.918
    ## 
    ## Scales y*:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##     y5_eat_STOPEAT    1.000                               1.000    1.000

## Anxiety Behavior Pruned Model - Parameter Export

``` r
standardizedSolution(
  fit_behav_anx_pruned, 
  type = "std.all", 
  se = TRUE, 
  zstat = TRUE, 
  pvalue = TRUE, 
  ci = TRUE, 
  level = 0.95
) %>%
  mutate_if(
    ~any(is.numeric(.)), 
    ~round(., 3)
    ) %>%
  write.table(
    "G:/My Drive/research/projects/phsr1/nghs1/nghs1_plots/anx_behav_table_std.csv", 
    sep = ","
  )
```

## Anxiety Behavior Pruned Model - All Data

``` r
fit_behav_anx_pruned_ad <-
  sem(
    mod_behav_anx_pruned, 
    data = semAllData%>% mutate(y5_eat_STOPEAT = as.factor(y5_eat_STOPEAT)), 
    estimator = "DWLS", 
    ordered = c("y5_eat_STOPEAT")
  )

summary(
  fit_behav_anx_pruned_ad, 
  standardized = TRUE, 
  fit.measures = TRUE
)
```

    ## lavaan 0.6-5 ended normally after 124 iterations
    ## 
    ##   Estimator                                       DWLS
    ##   Optimization method                           NLMINB
    ##   Number of free parameters                         37
    ##                                                       
    ##   Number of observations                          2379
    ##                                                       
    ## Model Test User Model:
    ##                                                       
    ##   Test statistic                                25.907
    ##   Degrees of freedom                                14
    ##   P-value (Chi-square)                           0.027
    ## 
    ## Model Test Baseline Model:
    ## 
    ##   Test statistic                              2313.755
    ##   Degrees of freedom                                28
    ##   P-value                                        0.000
    ## 
    ## User Model versus Baseline Model:
    ## 
    ##   Comparative Fit Index (CFI)                    0.995
    ##   Tucker-Lewis Index (TLI)                       0.990
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.019
    ##   90 Percent confidence interval - lower         0.006
    ##   90 Percent confidence interval - upper         0.030
    ##   P-value RMSEA <= 0.05                          1.000
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.017
    ## 
    ## Parameter Estimates:
    ## 
    ##   Information                                 Expected
    ##   Information saturated (h1) model        Unstructured
    ##   Standard errors                             Standard
    ## 
    ## Regressions:
    ##                     Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   y10_QI ~                                                               
    ##     y0_CATINC         -0.553    0.128   -4.302    0.000   -0.553   -0.092
    ##     y0_QI              1.240    0.068   18.293    0.000    1.240    0.712
    ##     y0_pbmi            0.095    0.024    3.899    0.000    0.095    0.098
    ##     y0_RACE            0.617    0.375    1.645    0.100    0.617    0.046
    ##     y6_et_SNKFOOD2    -0.707    0.291   -2.433    0.015   -0.707   -0.065
    ##     y5_eat_STOPEAT    -0.497    0.221   -2.251    0.024   -0.497   -0.075
    ##     y7_SECRET          0.916    0.340    2.696    0.007    0.916    0.050
    ##   y6_eat_SNKFOOD2 ~                                                      
    ##     y0_CATINC         -0.008    0.012   -0.635    0.525   -0.008   -0.014
    ##     y0_QI             -0.022    0.003   -6.815    0.000   -0.022   -0.137
    ##     y0_RACE            0.207    0.027    7.668    0.000    0.207    0.169
    ##   y5_eat_STOPEAT ~                                                       
    ##     y3_ANXR           -0.037    0.005   -8.184    0.000   -0.037   -0.241
    ##     y0_CATINC          0.075    0.026    2.903    0.004    0.075    0.082
    ##     y0_QI             -0.077    0.007  -10.298    0.000   -0.077   -0.292
    ##     y0_pbmi           -0.006    0.005   -1.270    0.204   -0.006   -0.039
    ##   y7_SECRET ~                                                            
    ##     y3_ANXR            0.009    0.001    7.165    0.000    0.009    0.159
    ##   y3_ANXR ~                                                              
    ##     y0_CATINC         -0.663    0.123   -5.395    0.000   -0.663   -0.113
    ##     y0_pbmi            0.120    0.018    6.765    0.000    0.120    0.127
    ##   y0_QI ~                                                                
    ##     y0_pbmi            0.153    0.010   15.369    0.000    0.153    0.274
    ##     y0_CATINC          0.000                               0.000    0.000
    ##     y0_RACE            0.841    0.166    5.077    0.000    0.841    0.110
    ##   y0_pbmi ~                                                              
    ##     y0_RACE            2.825    0.281   10.040    0.000    2.825    0.206
    ##   y0_CATINC ~                                                            
    ##     y0_RACE           -0.720    0.049  -14.823    0.000   -0.720   -0.326
    ## 
    ## Covariances:
    ##                      Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##  .y6_eat_SNKFOOD2 ~~                                                      
    ##    .y5_eat_STOPEAT      0.000                               0.000    0.000
    ##    .y7_SECRET          -0.014    0.004   -3.325    0.001   -0.014   -0.067
    ##  .y5_eat_STOPEAT ~~                                                       
    ##    .y7_SECRET           0.000                               0.000    0.000
    ##  .y0_pbmi ~~                                                              
    ##    .y0_CATINC           0.000                               0.000    0.000
    ## 
    ## Intercepts:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .y10_QI           -0.068    1.518   -0.045    0.964   -0.068   -0.010
    ##    .y6_et_SNKFOOD2    2.463    0.083   29.734    0.000    2.463    4.009
    ##    .y5_eat_STOPEAT    0.000                               0.000    0.000
    ##    .y7_SECRET         1.015    0.031   32.981    0.000    1.015    2.790
    ##    .y3_ANXR           9.134    0.831   10.999    0.000    9.134    1.414
    ##    .y0_QI            12.592    0.391   32.221    0.000   12.592    3.290
    ##    .y0_pbmi          26.821    0.476   56.391    0.000   26.821    3.918
    ##    .y0_CATINC         4.006    0.088   45.506    0.000    4.006    3.630
    ## 
    ## Thresholds:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##     y5_t_STOPEAT|1   -2.508    0.193  -13.021    0.000   -2.508   -2.496
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .y10_QI           15.395    2.107    7.306    0.000   15.395    0.346
    ##    .y6_et_SNKFOOD2    0.362    0.014   26.117    0.000    0.362    0.959
    ##    .y5_eat_STOPEAT    0.833                               0.833    0.825
    ##    .y7_SECRET         0.129    0.004   31.104    0.000    0.129    0.975
    ##    .y3_ANXR          40.448    1.631   24.795    0.000   40.448    0.969
    ##    .y0_QI            13.197    0.418   31.538    0.000   13.197    0.901
    ##    .y0_pbmi          44.860    0.913   49.133    0.000   44.860    0.957
    ##    .y0_CATINC         1.089    0.050   21.763    0.000    1.089    0.893
    ## 
    ## Scales y*:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##     y5_eat_STOPEAT    1.000                               1.000    1.000
