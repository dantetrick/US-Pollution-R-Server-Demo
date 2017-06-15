Create_Pollution_Models <- function(df, DV, IVint, VarOmit) {

# DV = "NO2_MEAN"
IVint <- IVint[which(IVint != DV)]


#################################################################
# CREATE MODEL FORMULAE 
#################################################################
Formula <- Formula_Creator(df_Hist_Xdf, DV, IVint, VarOmit)


# NO2 Model
LM_Model <- rxLinMod(Formula, data = df_Hist_Xdf)
Summary <- summary(LM_Model)

output <- list(Model = LM_Model,
               Summary = Summary)

return(output)



}