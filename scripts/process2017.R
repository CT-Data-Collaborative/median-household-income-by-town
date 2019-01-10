library(dplyr)
library(acs)
library(tidyr)
source('./scripts/acsHelpers.R')
source('./scripts/datapkg_read.R')
source('./scripts/utils.R')

#Setup environment
sub_folders <- list.files()

data_location <- grep("data", sub_folders, value=T)[1]
path_to_processed_data <- (paste0(getwd(), "/", data_location))
data_file <- dir(path_to_processed_data, recursive=F, pattern = "median")
previous_data <- read.csv(paste0(path_to_processed_data,"/",data_file)[1])

raw_location <- grep("raw", sub_folders, value=T)
path_to_raw_data <- (paste0(getwd(), "/", raw_location))
x2017_files <- dir(paste0(path_to_raw_data,"/2017"), recursive=F, pattern = "ACS") 

#process 2017 data
races_for_2017 <- c("White Alone", "Black or African American Alone", "American Indian and Alaska Native Alone", 
                    "Asian Alone", "Native Hawaiian and Other Pacific Islander", "Some Other Race Alone", 
                    "Two or More Races", "White Alone Not Hispanic or Latino", "Hispanic or Latino", "All")
x2017_data <- data.frame(stringsAsFactors = FALSE)
for (i in 1:length(x2017_files)) {
  data <- read.acs(paste0(path_to_raw_data, "/2017/", x2017_files[i]), endyear=2017, span=5)
  race <- races_for_2017[i]
  year <- data@endyear
  year <- paste(year-4, year, sep="-")
  geo <- data@geography
  geo$Geography <- NULL
  geo$Id2 <- gsub("^", "0", geo$Id2)
  geo$Id <- NULL
  income <- acsSum(data, 1, "Median Household Income")
  estimates <- data.table(
    geo, 
    estimate(income),
    year,
    race,
    "Number",
    "Median Household Income"
  )
  moes <- data.table(
    geo,
    standard.error(income) * 1.645,
    year,
    race,
    "Number",
    "Margins of Error"
  )
  numberNames <- c(
    "FIPS",
    "Total",
    "Year",
    "Race/Ethnicity",
    "Measure Type",
    "Variable"
  )
  setnames(estimates, numberNames)
  setnames(moes, numberNames)
  numbersData.melt <- melt(
    rbind(estimates, moes),
    id.vars=c("FIPS", "Year", "Measure Type", "Variable", "Race/Ethnicity"),
    variable.name="Median Household Income",
    variable.factor = F,
    value.name="Value",
    value.factor = F
  )
  x2017_data <- rbind(x2017_data, numbersData.melt)  
}

x2017_wide <- spread(x2017_data, Variable, Value)
x2017_wide <- x2017_wide[x2017_wide$FIPS != "0900100000",]
ct_value <- x2017_wide[x2017_wide$FIPS == "09",]

merge <- merge(x2017_wide, ct_value, by = c("Year", "Race/Ethnicity", "Measure Type"))


merge$`Ratio to State Median Num` <- merge$`Median Household Income.x` / merge$`Median Household Income.y`
merge$`Ratio to State Median MOE` <- sqrt((merge$`Margins of Error.x`)^2 + ((merge$`Median Household Income.x`/merge$`Median Household Income.y`)*((merge$`Margins of Error.y`)^2))) / (merge$`Median Household Income.x`)

merge <- merge %>% 
  select(FIPS.x, Year, `Race/Ethnicity`, `Measure Type`, `Margins of Error.x`, 
         `Median Household Income.x`, `Ratio to State Median Num`, `Ratio to State Median MOE`) 

#Merge in FIPS
town_fips_dp_URL <- 'https://raw.githubusercontent.com/CT-Data-Collaborative/ct-town-list/master/datapackage.json'
town_fips_dp <- datapkg_read(path = town_fips_dp_URL)
towns <- (town_fips_dp$data[[1]])
towns <- as.data.table(towns)

merge <- merge(merge, towns, by.x="FIPS.x", by.y = "FIPS", all.y=T)
merge <- merge[,c(9,1:8)]
merge <- gather(merge, `Variable`, Value, 6:9)

#Clean up columns
merge$`Measure Type`[grepl("Ratio", merge$Variable)] <- "Ratio to State Median"
merge$Variable <- gsub(".x", "", merge$Variable)
merge$Variable[grepl("MOE", merge$Variable)] <- "Margins of Error"
merge$Variable[grepl("Num", merge$Variable)] <- "Median Household Income"
names(merge)[names(merge) == "FIPS.x"] <- "FIPS"
merge$Value <-  round(merge$Value, 2)

colnames(previous_data) <- colnames(merge)
upto2017 <- rbind(previous_data, merge)

upto2017 <- upto2017 %>% 
  select(Town, FIPS, Year, `Race/Ethnicity`, `Measure Type`, Variable, Value) %>% 
  arrange(Town, Year, `Race/Ethnicity`, `Measure Type`, Variable)

WriteDFToTable(upto2017, "median-household-income-town-2017.csv")
