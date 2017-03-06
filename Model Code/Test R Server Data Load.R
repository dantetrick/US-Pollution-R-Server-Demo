# Set Options
options(stringsAsFactors = F)
options(scipen = 999)
options(repos=structure(c(CRAN="https://mran.revolutionanalytics.com/snapshot/2017-02-01/")))

# Select Packages to Load
pkgs <- c("plyr","RODBC", "data.table", "lubridate", "tidyr","stringr", "parallel","dplyr",
          "RevoScaleR")

# Load Libraries and Source Codes
sapply(pkgs, require, character.only = T)

# Set Paths 
Main_Path <- "C:/Users/dan.tetrick/Documents/R Server Demo/"
Results_Path <- paste0(Main_Path,"Results/")
Input_Path <- paste0(Main_Path,"Input Data/")
Model_Code <- paste0(Main_Path,"Model Code/")

# Set Connection String to the SQL DB
sqlConnString <- "driver={SQL Server};server=cisdemosqlserver.database.windows.net;database=USPollution;Uid=dant;Pwd=SlalomDS!1;"
myconn <- odbcDriverConnect('driver={SQL Server};server=cisdemosqlserver.database.windows.net;database=CISDemoDatabase;Uid=dant;Pwd=SlalomDS_12345')

LoadData4 <- file.path(Input_Path,"Date Dimension Table.csv")

sqlLoadTable4 <- "Dim_Dates"

sqlRowsPerRead <- 200000

##########################################################
# Using a SQL Server Data Source and Compute Context
##########################################################  

sqlDatesDS <- RxSqlServerData(connectionString = sqlConnString, 
                              table = sqlLoadTable4, rowsPerRead = sqlRowsPerRead)

# Create SQL Date table context and write to SQL
inTextData <- RxTextData(file = LoadData4, 
                         colClasses = c( "DIM_DATE_KEY" = "numeric",
                                         "DATE" = "character",
                                         "DAY_OF_YEAR" = "numeric",
                                         "YEAR" = "numeric",
                                         "DAY_OF_MONTH" = "numeric",
                                         "MONTH_NUM" = "numeric",
                                         "MONTH_DAY_ABBR" = "character",
                                         "MONTH_DAY" = "character",
                                         "WEEK_OF_YEAR" = "numeric",
                                         "WEEK_DAY_ABBR" = "character",
                                         "WEEK_DAY" = "character",
                                         "WEEK_OF_YEAR" = "numeric",
                                         "QUARTER" = "character",
                                         "QUARTER_FORM" = "character")
)

rxDataStep(inData = inTextData, outFile = sqlDatesDS, overwrite = TRUE)



