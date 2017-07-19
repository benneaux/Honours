# require(tm)
# require(pdftools)
# require(stringi)
# require(httr)
# require(rvest)
# require(tidyverse)
# library(stringr)
# library(lubridate)
#

# # run the following after restarting R
# #Sys.setenv(http_proxy = "http://<username>:<password>@proxy.newcastle.edu.au:8080")
#
# page <- read_html("http://flutracking.net/Info/Reports/")
#
# page %>%
#   html_nodes("a") %>%       # find all links
#   html_attr("href") %>%     # get the url
#   str_subset("/[0-9]{3,}$") -> filenames # find those that end a numeric string > length(3)
#
# filenames %>%                    # extract the numeric id from each name
#   str_extract("[0-9]+") %>% # i.e. '201728' instead of 'Info/Reports/201728'
#   as.data.frame() -> filenames
#
# prev_retrieved_files <- data.frame(
#                           files = readRDS(
#                             "Data/retrieved_files_list.RDS"),
#                           stringsAsFactors = FALSE)
#
# names <- data.frame(
#           files = setdiff(
#                   filenames$.,
#                   prev_retrieved_files[,1])) -> names
#
# saveRDS(rbind(
#           prev_retrieved_files,
#           names),
#         "Data/retrieved_files_list.RDS")
#
# rm(prev_retrieved_files, filenames)
# #
# # ###############################################################################
# # #
# # # Dowload the files
# # #
# # ###############################################################################
# #
#
# for(i in 1:nrow(names)){
#
#   fileurl  = paste0("http://flutracking.net/Info/Reports/",
#                    as.character(names[i,1]))
#   fil      = GET(fileurl,
#                  write_disk("pdfs/tmp.fil",
#                             TRUE))
#   fname    = str_match(headers(fil)$`content-disposition`,
#                        "=(.*)")[2]
#
#   file.rename("pdfs/tmp.fil",
#               paste0("pdfs/",
#                      fname))
#
#   Sys.sleep(2)
# }

###############################################################################
#
# Setup the container for the data
#
###############################################################################

files <- list.files(path = "pdfs/",pattern = "pdf$")
data <- tbl_df(matrix(nrow=length(files),ncol = 12))

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

    txt        <- as.list(pdf_text(files[i])[1]) # scrapes all txt from the pdf

    WEdate  <- ifelse(is.na(datefunc(txt,1)), # the 'week ending' date.
                         datefunc(txt,2),
                         datefunc(txt,1))

    data[i,1]  <- convertWEdatefunc(WEdate)

    data[i,2]  <- numberfunc(txt,3)              # the # of responses
    data[i,3]  <- numberfunc(txt,4)              #
    data[i,4]  <- numberfunc(txt,5)              #
    data[i,5]  <- numberfunc(txt,6)              #
    data[i,6]  <- numberfunc(txt,7)              #
    data[i,7]  <- numberfunc(txt,8)              #
    data[i,8]  <- numberfunc(txt,9)              #
    data[i,9]  <- percfirstfunc(txt,10)          #
    data[i,10] <- percfirstfunc(txt, 11)         #
    data[i,11] <- perclastfunc(txt,12)           #
    data[i,12] <- perclastfunc(txt,13)           #
  },
  error=function(e){})
}

colnames(data) <- c("Week_end",
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

data[[1]] <- as_date(data[[1]])

setwd("..")
###############################################################################
#
# Export
#
###############################################################################

write.csv(data,file="fludata.csv")
