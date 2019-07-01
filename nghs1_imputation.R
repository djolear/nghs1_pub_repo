
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, haven, missForest)

df1 <- 
  read_sas("G:/My Drive/research/projects/phsr1/nghs1/nghs1_data/alldat.sas7bdat") %>% 
  mutate(
    y3_eximprt = coalesce(y3_EXIMPRT_fm2, y3_EXIMPRT_fm3, y3_EXIMPRT_fm4, y3_EXIMPRT_fm5),
  )

dfItemWise <- 
  read_csv("G:/My Drive/research/projects/phsr1/nghs1/nghs1_data/nghs1_psych_survey_item_wise.csv") %>% 
  mutate_at(
    vars(
      contains("anxr_ITEM")
    ),
    as.factor
  )


df <- 
  read_sas("G:/My Drive/research/projects/phsr1/nghs1/nghs1_data/newdat.sas7bdat") %>% 
  mutate(
    chgBMI = y10_QI - y0_QI,
    RID = as.numeric(RID),
    ######################
    # Parent Information #
    ######################
    
    y0_pbmi_fm2 = (y0_WT_fm2 * 703)/((y0_HTFT_fm2 * 60 + y0_HTIN_fm2)^2),
    y0_pbmi_fm3 = (y0_WT_fm3 * 703)/((y0_HTFT_fm3 * 60 + y0_HTIN_fm3)^2),
    
    y0_eximprt_fm = coalesce(y0_EXIMPRT_fm2, y0_EXIMPRT_fm3),
    #y3_eximprt_fm = coalesce(y3_EXIMPRT_fm2, y3_EXIMPRT_fm3, y3_EXIMPRT_fm4, y3_EXIMPRT_fm5)
    y5_eximprt_fm = coalesce(y5_EXIMPRT_fm2, y5_EXIMPRT_fm3),
    y7_eximprt_fm = coalesce(y7_EXIMPRT_fm2, y7_EXIMPRT_fm3),
    
    y0_exreg_fm = coalesce(y0_EXREG_fm2, y0_EXREG_fm3),
    y3_exreg_fm = coalesce(y3_EXREG_fm2, y3_EXREG_fm3),
    y5_exreg_fm = coalesce(y5_EXREG_fm2, y5_EXREG_fm3),
    y7_exreg_fm = coalesce(y7_EXREG_fm2, y7_EXREG_fm3),
    
    y0_phealth_fm = coalesce(y0_HEALTH_fm2, y0_HEALTH_fm3),
    y3_phealth_fm = coalesce(y3_HEALTH_fm2, y3_HEALTH_fm3),
    y5_phealth_fm = coalesce(y5_HEALTH_fm2, y5_HEALTH_fm3),
    y7_phealth_fm = coalesce(y7_HEALTH_fm2, y7_HEALTH_fm3),
    
    y0_aftmst_fm = coalesce(y0_AFTMST_fm2, y0_AFTMST_fm3),
    y0_dairy_fm = coalesce(y0_DAIRY_fm2, y0_DAIRY_fm3),
    y0_cfried_fm = coalesce(y0_CFRIED_fm2, y0_CFRIED_fm3),
    
    y0_physact_fm = coalesce(y0_PHYACT_fm2, y0_PHYACT_fm3),
    y3_physact_fm = coalesce(y3_PHYACT_fm2, y3_PHYACT_fm3),
    y5_physact_fm = coalesce(y5_PHYACT_fm2, y5_PHYACT_fm3),
    y7_phyact_fm = coalesce(y7_PHYACT_fm2, y5_PHYACT_fm3),
    
    y3_btwnmls = coalesce(y3_BETWNMLS_fm2, y3_BETWNMLS_fm3, y3_BETWNMLS_fm4, y3_BETWNMLS_fm5),
    
    y0_fstfood = coalesce(y0_FSTFOOD_fm2, y0_FSTFOOD_fm3),
    y3_fstfood = coalesce(y3_FSTFOOD_fm2, y3_FSTFOOD_fm3),
    y5_fstfood = coalesce(y5_FSTFOOD_fm2, y5_FSTFOOD_fm3),
    y7_fstfood = coalesce(y7_FSTFOOD_fm2, y7_FSTFOOD_fm3),
    
    y0_soda = coalesce(y0_SODA_fm2, y0_SODA_fm3),
    y3_soda = coalesce(y3_SODA_fm2, y3_SODA_fm3, y3_SODA_fm4, y3_SODA_fm5),
    y5_soda = coalesce(y5_SODA_fm2, y5_SODA_fm3)
  ) %>% 
  left_join(
    df1 %>% 
      select(
        RID,
        y3_eximprt,
        y0_BREAKFST,
        y3_BREAKFST,
        y4_BREAKFST,
        y5_BREAKFST,
        y6_BREAKFST,
        y7_BREAKFST,
        y8_BREAKFST,
        y9_BREAKFST,
        y10_BREAKFST,
        y0_EATTV,
        y3_EATTV,
        y4_EATTV,
        y5_EATTV,
        y6_EATTV,
        y7_EATTV,
        y8_EATTV,
        y9_EATTV,
        y10_EATTV
      ) %>% 
      mutate(RID = as.numeric(RID)),
    by = "RID"
  ) %>% 
  left_join(
    dfItemWise,
    by = "RID"
  ) %>% 
  mutate_all(as.numeric) %>% 
  as.matrix() %>% 
  as.data.frame() %>% 
  mutate_at(
    vars(
      ends_with("CATINC"),
      ends_with("CATEDUC"),
      ends_with("eximprt_fm"),
      ends_with("exreg_fm"),
      ends_with("phealth_fm"),
      ends_with("aftmst_fm"),
      ends_with("cfried_fm"),
      ends_with("dairy_fm"),
      ends_with("physact_fm"),
      ends_with("btwnmls"),
      ends_with("y0_fstfood"),
      ends_with("_soda"),
      y0_RACE,
      y0_FEMALE,
      y0_MALE,
      ends_with("SIBS"),
      contains("ANXIETY_fm"),
      contains("DEPRES_fm"),
      ends_with("MR"),
      ends_with("REWARD"),
      ends_with("HUNGY"),
      ends_with("SNACKLOT"),
      ends_with("AFTSNK"),
      ends_with("FSTFOOD4"),
      ends_with("FSTFOOD5"),
      ends_with("FAMLY"),
      ends_with("ALONE"),
      ends_with("SECRET"),
      ends_with("BREAKFST"),
      y0_AFTMST_fm2,
      y0_FSTFOOD_fm2,
      y0_DAIRY_fm2,
      y0_CFRIED_fm2,
      y0_SODA_fm2,
      y0_DEPRES_fm2,
      y0_ANXIETY_fm2,
      y0_PHYACT_fm2,
      ends_with("NOCNTL"),
      ends_with("EXIMPRT_fm2"),
      ends_with("EXIMPRT_fm3"),
      ends_with("EXREG_fm2"),
      ends_with("EXREG_fm3"),
      ends_with("HEALTH_fm2"),
      ends_with("HEALTH_fm3"),
      contains("anxr_ITEM"),
      contains("_eat_")
    ),
    as.factor
  ) %>% 
  select_if(~ (sum(is.na(.))/ 2379) < 0.3)


impStart <- proc.time()

dfImp <-
  missForest(df, verbose = TRUE, variablewise = TRUE)

impFinish <- proc.time() - impStart

dfImpT <- as_tibble(dfImp$ximp)

dfImpRename <-
  dfImpT %>% 
  # rename_all(
  #   substr, 6, length(.)
  # ) %>% 
  mutate_at(
    vars(
      ends_with("CATINC"),
      ends_with("CATEDUC"),
      ends_with("eximprt_fm"),
      ends_with("exreg_fm"),
      ends_with("phealth_fm"),
      ends_with("aftmst_fm"),
      ends_with("cfried_fm"),
      ends_with("dairy_fm"),
      ends_with("physact_fm"),
      ends_with("btwnmls"),
      ends_with("y0_fstfood"),
      ends_with("_soda"),
      y0_RACE,
      y0_FEMALE,
      y0_MALE,
      ends_with("SIBS"),
      contains("ANXIETY_fm"),
      contains("DEPRES_fm"),
      ends_with("MR"),
      ends_with("REWARD"),
      ends_with("HUNGY"),
      ends_with("SNACKLOT"),
      ends_with("AFTSNK"),
      ends_with("FSTFOOD4"),
      ends_with("FSTFOOD5"),
      ends_with("FAMLY"),
      ends_with("ALONE"),
      ends_with("SECRET"),
      ends_with("BREAKFAST"),
      #y0_AFTMST_fm2,
      #y0_FSTFOOD_fm2,
      # y0_DAIRY_fm2,
      # y0_CFRIED_fm2,
      # y0_SODA_fm2,
      #y0_DEPRES_fm2,
      #y0_ANXIETY_fm2,
      #y0_PHYACT_fm2,
      ends_with("NOCNTL"),
      ends_with("EXIMPRT_fm2"),
      ends_with("EXIMPRT_fm3"),
      ends_with("EXREG_fm2"),
      ends_with("EXREG_fm3"),
      ends_with("HEALTH_fm2"),
      ends_with("HEALTH_fm3"),
      
      
      
    ),
    as.factor
  ) 

#glimpse(dfImpRename)

# Save as RDS?

write_csv(dfImpRename, "G:/My Drive/research/projects/phsr1/nghs1/nghs1_data/nghs1_imputed_new2.csv")

# differences: 9.087691e-05 0.004041624
mse <- data.frame("mse" = dfImp$OOBerror[names(dfImp$OOBerror) == "MSE"])
pfc <- data.frame("pfc" = dfImp$OOBerror[names(dfImp$OOBerror) == "PFC"])

write_csv(mse, "G:/My Drive/research/projects/phsr1/nghs1/nghs1_data/nghs1_imputed_mse.csv")
write_csv(pfc, "G:/My Drive/research/projects/phsr1/nghs1/nghs1_data/nghs1_imputed_pfc.csv")


MSE <- mean(dfImp$OOBerror[names(dfImp$OOBerror) == "MSE"], na.rm = TRUE)
PFC <-  mean(dfImp$OOBerror[names(dfImp$OOBerror) == "PFC"], na.rm = TRUE)



#dfImp <- read_csv( "G:/My Drive/research/projects/phsr1/nghs1/nghs1_data/nghs1_imputed.csv")
