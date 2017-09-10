require(tm)
require(pdftools)
require(stringi)
require(httr)
require(rvest)
require(tidyverse)
library(stringr)
library(lubridate)
library(readxl)

files <- list.files(path = "pdfs/",pattern = "xlsx$")
data2 <- tbl_df(matrix(nrow=length(files),ncol = 12))

###############################################################################
#
# Import regex codes
#
###############################################################################

codes <- readRDS("Data/flutracking_regex_codes.RDS")

###############################################################################
#
# Function definitions
#
###############################################################################

# datefunc is used to get the week ending date (field 1)

datefunc <- function(x, codenum){
  unlist(
    stri_extract_first_regex(
      x,
      codes$String[codenum]))
}

# numberfunc is used to get all of the raw numbers (fields 2:8)

numberfunc <- function(x, codenum){
  unlist(
    stri_extract_all_charclass(
      stri_extract_all_regex(
        x,
        codes$String[codenum]),
      codes$CharClass[codenum]))
}

# The perc functions are used to get the various percentages (fields 9:12)
# Because they come in pairs, I've defined variants for the first and
# second values returned.

percfirstfunc <- function(x, codenum){
  unlist(
    stri_extract_all_regex(
      unlist(stri_extract_first_regex(
        x,
        codes$String[codenum])),
      codes$CharClass[codenum]))
}

perclastfunc <- function(x, codenum){
  unlist(
    stri_extract_all_regex(
      unlist(stri_extract_last_regex(
        x,
        codes$String[codenum])),
      codes$CharClass[codenum]))
}

convertWEdatefunc <- function(x){

  WEday = stri_extract_first_regex(x,"[0-9]{2}")
  WEmonth = match(
    stri_extract_first_regex(
      x,
      "\\b[A-z][a-z]*\\b")[[1]],
    month.name)
  WEyear = stri_extract_first_regex(x,"[0-9]{4}")
  make_date(day = WEday, month = WEmonth, year = WEyear)
}


###############################################################################
#
# Data scraping
#
###############################################################################
setwd("pdfs")
for(i in 1:length(files)){

  tryCatch({

    txt        <- read_excel((files[i]), sheet = 1)[1:4,c(1,5)] # scrapes all txt from the pdf

    WEdate  <- ifelse(is.na(datefunc(txt[1,1],1)), # the 'week ending' date.
                      datefunc(txt[1,1],2),
                      datefunc(txt[1,1],1))

    data2[i,1]  <- convertWEdatefunc(WEdate)

    data2[i,2]  <- numberfunc(txt[3,1],3)              # the # of responses
    data2[i,3]  <- numberfunc(txt[3,1],4)              #
    data2[i,4]  <- numberfunc(txt[3,1],5)              #
    data2[i,5]  <- numberfunc(txt[4,2],6)              #
    data2[i,6]  <- numberfunc(txt[3,1],7)              #
    data2[i,7]  <- numberfunc(txt[4,2],8)              #
    data2[i,8]  <- numberfunc(txt[4,2],9)              #
    txt[4,2]   <- str_replace_all(txt[4,2]," %", "%")
    data2[i,9]  <- percfirstfunc(txt[4,2],10)          #
    data2[i,10] <- percfirstfunc(txt[4,2], 11)         #
    data2[i,11] <- perclastfunc(txt[4,2],12)           #
    data2[i,12] <- perclastfunc(txt[4,2],13)           #
  },
  error=function(e){})
}

colnames(data2) <- c("Week_end",
                    "Responses",
                    "Self_Report",
                    "Other_Report",
                    "Vaccinated_Respondents",
                    "Total_Respondents",
                    "Clinical_Staff",
                    "Clinical_Staff_Vaccinated",
                    "ILI_Vaccinated",
                    "ILI_Unvaccinated",
                    "ILI_wAbsence_Vaccinated",
                    "ILI_wAbsence_Unvaccinated")

data2[[1]] <- as_date(data2[[1]])

setwd("..")

data3 <- rbind(data, data2)
data3 <- data3[!is.na(data3[[1]]),]
###############################################################################
#
# Export
#
###############################################################################

write.csv(data3,file="Data/fludata.csv")
