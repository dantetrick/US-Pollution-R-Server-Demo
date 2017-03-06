# Set Options
options(stringsAsFactors = F)
options(scipen = 999)
options(repos=structure(c(CRAN="https://mran.revolutionanalytics.com/snapshot/2017-02-18/")))

# Select Packages to Load
pkgs <- c("readr", "lubridate", "tidyr","stringr","lattice",
          "RevoScaleR","RevoMods", "dplyr","dplyrXdf")

# Load Libraries and Source Codes
sapply(pkgs, require, character.only = T)

# Set Paths 
Main_Path <- "C:/Users/dan.tetrick/Documents/R Server Demo/"
Results_Path <- paste0(Main_Path,"Results/")
Input_Path <- paste0(Main_Path,"Input Data/")
Model_Code <- paste0(Main_Path,"Model Code/")

# Load Custom Functions
source(paste0(Model_Code,"Trim.R"))
source(paste0(Model_Code,"Dim Date Creator.R"))

# Create Date Dimension
Dates <- Dim_Date_Creator("2000-01-01", "2020-12-31")

# Load Data to Model
df <- read_csv("~/R Server Demo/Input Data/US Pollution Data 2010_2016.csv")

#############################################
# Format and Summarize Data
############################################# 


####################################################
# Create Model Data
#####################################################

# NO2 Model
NO2_Model <- df %>% ungroup() %>% 
                    select(-DIM_DATE_KEY, -DATE, -NO2_MEDIAN, -CO_MEDIAN, -SO2_MEDIAN, -O3_MEDIAN) %>%
                    lm(NO2_MEAN ~ . , .)
summary(NO2_Model)
df_Proj$NO2_PRED <- predict(NO2_Model, df_Proj)

# O3 Model
O3_Model <- df %>% ungroup() %>% 
                   select(-DIM_DATE_KEY, -DATE, -NO2_MEDIAN, -CO_MEDIAN, -SO2_MEDIAN, -O3_MEDIAN) %>%
                   lm(O3_MEAN ~ . , .)
summary(O3_Model)
df_Proj$O3_PRED <- predict(O3_Model, df_Proj)

# SO2 Model
SO2_Model <- df %>% ungroup() %>% 
                    select(-DIM_DATE_KEY, -DATE, -NO2_MEDIAN, -CO_MEDIAN, -SO2_MEDIAN, -O3_MEDIAN) %>%
                    lm(SO2_MEAN ~ . , .)
summary(SO2_Model)
df_Proj$SO2_PRED <- predict(SO2_Model, df_Proj)

# CO Model
CO_Model <- df %>% ungroup() %>% 
                   select(-DIM_DATE_KEY, -DATE, -NO2_MEDIAN, -CO_MEDIAN, -SO2_MEDIAN, -O3_MEDIAN) %>%
                   lm(CO_MEAN ~ . , .)
summary(CO_Model)
df_Proj$CO_PRED <- predict(CO_Model, df_Proj)

#####################################################
# Predict all Pollution Model
####################################################

df_Proj <- df_Proj %>% select(DIM_ADDRESS_KEY,DIM_DATE_KEY,NO2_PRED,O3_PRED,SO2_PRED,CO_PRED) %>%
                       rename(NO2_MEAN = NO2_PRED,
                              O3_MEAN = O3_PRED,
                              SO2_MEAN = SO2_PRED,
                              CO_MEAN = CO_PRED)

write.csv(df_Proj,paste0(Results_Path,"US Pollution 1 YR Projection Complete.csv"),row.names = F)

