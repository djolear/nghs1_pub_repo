# Load libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, haven)

# Load data
df <-
  read_csv("G:/My Drive/research/projects/phsr1/nghs1/nghs1_data/nghs1_imputed_new2.csv") %>% 
  mutate(y0_pbmi = (y0_WT_fm2 * 703) / (semAllData$y0_HTFT_fm2 * 12 * semAllData$y0_HTFT_fm2 * 12))

# Check dimensions
dim(df)

# Set seed
set.seed(1987)

# Make first split
lassoTrainRows <- sample(1:nrow(df), nrow(df)/3, replace = FALSE)

lassoTrainingData <- df[lassoTrainRows, ]
remainingData <- df[-lassoTrainRows, ]

# Make second split
semTrainRows <- sample(1:nrow(remainingData), .5 * nrow(remainingData), replace = FALSE)

semTrainingData <- remainingData[semTrainRows, ]
semTestingData <- remainingData[-semTrainRows, ]

# Write data to disk
write_csv(lassoTrainingData, "G:/My Drive/research/projects/phsr1/nghs1/nghs1_data/nghs1_lasso_training_data.csv")
write_csv(semTrainingData, "G:/My Drive/research/projects/phsr1/nghs1/nghs1_data/nghs1_sem_training_data.csv")
write_csv(semTestingData, "G:/My Drive/research/projects/phsr1/nghs1/nghs1_data/nghs1_sem_testing_data.csv")
