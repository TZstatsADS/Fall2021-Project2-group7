library(plyr)
library(tidyverse)
library(httr)
library(xml2)
library(rvest)
library(stringr)

#page <- read_html("https://www.nychealthandhospitals.org/covid-19-testing-sites/")
#write_html(page, "vax_find.html")
covid_vax <- read_html("vax_find.html")
test_sites <- covid_vax %>% 
  html_nodes(".contact_page_container") %>% 
  html_nodes(".m-b-20") %>% 
  html_text()


test_sites_tokens <-str_split(test_sites,'\n\t\t')

sites <- list()
address <- list()
boro_zip <- list()
time_date <- list()

for(i in 5:length(test_sites_tokens)){
  
  sites[i] <- test_sites_tokens[i][[1]][[1]]
  address[i] <- test_sites_tokens[i][[1]][2]
  boro_zip[i] <- test_sites_tokens[i][[1]][3]
  time_date[i] <- test_sites_tokens[i][[1]][4]
  output <- tibble(sites, address, boro_zip, time_date)
  }

output_tidy <- output %>% 
  filter(is.na(boro_zip) ==0) %>% 
  filter(sites != "NULL")

str_extract(output$boro_zip, "\\d+")  
  
# parse text
# extract antigen testing
# extract rapid molecular testing
# temporarily closed
# preregister

