library(tidyverse)
library(tabulizer)
library(stringi)

test_pdf <- paste0("pdfs/",list.files(path = "pdfs/",pattern = "pdf$")[[1]])
area <- c(locate_areas(test_pdf, pages = 1, widget = "shiny"))

file_list <- as.list(list.files(path = "pdfs/",pattern = "pdf$"))

df <- data.frame(X1 = NA, X2 = NA, X3 = NA, X4 = NA)

for(i in 1:length(file_list)){
  doc = file_list[i]
  tab = as.data.frame(extract_tables(paste0("pdfs/",doc), pages = 1, area = area))
  tab = cbind(tab, stri_extract_all_charclass(doc,"[0-9]"))
  names(tab) = c("X1","X2","X3","X4")
  df = rbind(df, tab)
}

names(df) <- c("State","Respondents","Proportion","File")
df <- df[!(is.na(df$State) | df$State == "" | df$State == "Totals" | df$State == "State" | df$State == "AUS"),]
df %>%
  gather(key = Measure, value = Value, Respondents, Proportion) %>%
  select(File, State, Measure, Value) -> df

area <- c(locate_areas(test_pdf, pages = 1, widget = "shiny"))

df <- matrix(NA)

for(i in 1:length(file_list)){

  doc  = file_list[i]
  df1  = pdftools::pdf_text(paste0("pdfs/",doc))[[1]]
  df2  = tm::stripWhitespace(df1)
  df3  = trimws(df2, "both")
  # df2  = stri_split_fixed(df1, "\n", omit_empty = TRUE, simplify = TRUE)
  # df2  = df1[stri_detect_charclass(df1, "[0-9]")]
  df = rbind(df, df3)
}
extract_tables(test_pdf,pages = 1, area = area)
