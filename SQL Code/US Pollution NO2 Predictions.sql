USE [R Server Demo]
GO

/****** Object:  StoredProcedure [dbo].[Generate_USPollution_NO2_Predictions]    Script Date: 3/9/2017 12:45:43 PM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO




 
CREATE PROCEDURE [dbo].[Generate_USPollution_NO2_Predictions]   
 AS

 BEGIN
  BEGIN TRY 
		DROP TABLE [dbo].[Predictions_US_Pollution_NO2]
	END TRY
	BEGIN CATCH
	END CATCH
	;

DECLARE @NO2_Predictions table  ([DIM_ADDRESS_KEY] int,
						   [DIM_DATE_KEY] int,
						   [NO2_MEAN] float);
		
DECLARE @inputQuery nvarchar(max) = N'SELECT  * FROM [dbo].[Fact_US_Pollution_Projected]';

DECLARE @NO2_Model varbinary(max) = (SELECT Model FROM [dbo].[Model_US_Pollution_NO2]);


DECLARE @rScript nvarchar(max) =N'

########################################
# Set Options
########################################

options(stringsAsFactors = F);
options(scipen = 999); 
  
#####################################
# Select Packages to Load
#####################################

pkgs <- c("readr", "lubridate", "RODBC","tidyr","stringr","lattice",
          "RevoScaleR","RevoMods", "dplyr","dplyrXdf")

# Load Libraries and Source Codes
sapply(pkgs, require, character.only = T)
	
########################################
# Clean Data
#############################################
	
# Format df
df <- tbl_df(df)  %>%
      arrange(DIM_ADDRESS_KEY,DIM_DATE_KEY) %>%
      mutate(DATE = as.Date(DATE),
             DIM_ADDRESS_KEY = factor(as.character(DIM_ADDRESS_KEY),ordered = T),
             MONTH = factor(as.character(MONTH),ordered = T),
             DAY = factor(as.character(DAY),ordered = T))

############################
# Model 
############################
            current_model <- unserialize(as.raw(NO2_Model));
            new <- data.frame(df);
            predictions <- rxPredict(current_model, new);
			df <- data.frame(DIM_ADDRESS_KEY = df$DIM_ADDRESS_KEY,
							 DIM_DATE_KEY = df$DIM_DATE_KEY,
							 NO2_MEAN = predictions)
            
			Results <- df;
            '
;
	   

INSERT INTO @NO2_Predictions
		EXECUTE  sp_execute_external_script
				@language = N'R'
				, @script = @rScript
				, @input_data_1 = @inputQuery
				, @input_data_1_name = N'df'
				, @output_data_1_name = N'Results'	
				, @params = N'@NO2_Model varbinary(max)'
				, @NO2_Model = @NO2_Model

SELECT *
INTO [dbo].[Predictions_US_Pollution_NO2]
FROM @NO2_Predictions;




END


   



GO


