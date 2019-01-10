ToNumeric <- function (factor) {
  return(as.numeric(as.character(factor)))
}

ExtractNumData <- function (factor) {
  return(ToNumeric(factor[1:length(factor)]))
}

WriteDFToTable <- function(df, filename){
  write.table(
    df,
    file = file.path(getwd(), "data", filename),
    sep = ",",
    row.names = FALSE,
    na = "-9999"
  )
}

# Read Connecticut-specific CSV for median household income
getCTHouseholdIncome <- function(filepath){
  ct.data  <- read.csv(filepath, header = FALSE)
  ct.data <- ct.data[-1:-2,]
  names(ct.data) <- c("Id", "FIPS", "Geography", "Income", "MoE")
  
  ct.data$Year <- rep("2013-2017", length(ct.data[,1]))
  ct.data <- ct.data[!(grepl("not defined", ct.data$Geography)),]
  return(ct.data)
}

# group moe and income under single observational unit
gatherEstimateMoE <- function(ct.income){
  ct.income <- ct.income[,c(1,2,3,5,6,4)]
  return(
    ct.income %>%
      gather("Variable", "Value", c("Income","MoE"))
  )
}

# Sort data by FIPS Code
orderedByGeography <- function(ct.income){
  return(ct.income[order(ct.income[,2], ct.income[,3]),])
}


getGatheredCountyData <- function(ct.income){
  ct.income$Geography <- gsub(", Connecticut", "", ct.income$Geography)
  gathered <- gatherEstimateMoE(ct.income[ExtractNumData(ct.income$FIPS) < 10000,])
  return(orderedByGeography(gathered))
}

getGatheredTownData <- function(ct.income){
  ct.income$Geography <- gsub(",.*, Connecticut", "", ct.income$Geography)
  gathered<- gatherEstimateMoE(ct.income[grepl("(^Connecticut$)|town", ct.income$Geography),])
  return(orderedByGeography(gathered))
}
