require(tm)
require(pdftools)
require(stringi)
require(httr)
require(rvest)
require(tidyverse)
library(stringr)

#
# page <- read_html("http://flutracking.net/Info/Reports/")
#
# page %>%
#   html_nodes("a") %>%       # find all links
#   html_attr("href") %>%     # get the url
#   str_subset("/[0-9]{3,}$") -> name # find those that end a numeric string > length(3)
#
# name %>%                    # extract the numeric id from each name
#   str_extract("[0-9]+") %>% # i.e. '201728' instead of 'Info/Reports/201728'
#   as.data.frame() -> name
#
# name %>%                    # select only those from 2017
#   filter(
#     str_detect(
#       name$.,
#       "2017")
#     ) -> name
#
# name %>%                    # Remove unnecessary factor levels
#   droplevels() -> name
#
# name$. %>%                  # convert from factors to strings
#   as.character(levels(name$.)) -> name$.
#
# ###############################################################################
# #
# # Dowload the files
# #
# ###############################################################################
#
# for(i in 1:nrow(name)){
#
#   fileurl = paste0("http://flutracking.net/Info/Reports/",
#                    as.character(name[i,1]))
#   filename = paste0(as.character(name[i,1]),
#                     ".pdf")
#   download(fileurl,
#            filename,
#            mode="wb")
#   Sys.sleep(2)
# }

###############################################################################
#
# Setup the container for the data
#
###############################################################################

files <- list.files(pattern = "pdf$")
data <- matrix(nrow=length(files),ncol = 12)

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

###############################################################################
#
# Data scraping
#
###############################################################################

for(i in 1:length(files)){

  tryCatch({

    txt        <- as.list(pdf_text(files[i])[1]) # scrapes all txt from the pdf

    data[i,1]  <- ifelse(is.na(datefunc(txt,1)), # the 'week ending' date.
                         datefunc(txt,2),
                         datefunc(txt,1))

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

###############################################################################
#
# Export
#
###############################################################################

write.csv(data,file="fludata.csv")
