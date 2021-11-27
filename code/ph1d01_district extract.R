library(tidyverse)
library(haven)
library(rvest)
url = "http://rchiips.org/nfhs/districtfactsheet_NFHS-5.shtml"
state_url = read_html(url) %>% 
  html_nodes("option") %>% 
  html_attr("value")
# 21 states/UTs
state_url <- state_url[-1] # Removing the title 'SELECT A DISTRICT'


# Function to parse district factsheets

library(tabulizer)

source("code/ph100_functions.R")

# Running extract, parse, write sequence -------------
district_status = data.frame()
dir.create("phase 1 release/districts")
dir.create("phase 1 release/districts/csv_output")
dir.create("phase 1 release/districts/stata_output")

# Ad-hoc corrections by Aashish
ad_hoc <- c(
   "NFHS-5_FCTS/MN/Thoubal.pdf",
  "NFHS-5_FCTS/MH/Buldana.pdf"
  )
# 
incorrect_match <- c(
            "NFHS-5_FCTS/MN/Toubal.pdf",
            "NFHS-5_FCTS/MH/Buldhana.pdf"
)
# 
# WB = 21, BR = 4, GA = 6, GJ = 7, MN = 14, TG = 19, MH = 13
# TR = , ML = 15, HP = 
for(d in state_url){
  
  s_url = paste0("http://rchiips.org/nfhs/",d)
  
  district_urls = read_html(s_url) %>% 
    html_nodes("option") %>% 
    html_attr("value")
  
  match_vec_pdf_to_adhoc = which(district_urls %in% incorrect_match,arr.ind = TRUE)
  match_vec_adhoc_to_pdf = which(incorrect_match %in% district_urls, arr.ind = TRUE)
  district_urls[match_vec_pdf_to_adhoc] = ad_hoc[match_vec_adhoc_to_pdf]
  
  state_code = str_replace(d,"NFHS-5_","") %>% str_replace(.,".shtml","")
  # if(state_code %in% c("KA","KL")){
  #   district_urls = district_urls %>% 
  #     str_replace(.,"FCTS","NFHS-5_FCTS") %>% 
  #     str_replace(.,ifelse(state_code == "KA","KA_FactSheet_[0-9]+_","KL_Factsheet_[0-9]+_")
  #                 ,"")
  # }
  
  district_urls <- district_urls[-1]
  
  # file downloads - Run this if you want to download all files and extract at the same time
  # temp_status <- district_fcts(district_urls)
  
  # Does file exist? - Comment the next line out if you are running district_fcts()
  temp_status <- detect_fcts(district_urls)
  
  temp_status <- temp_status %>% 
    map_dfr(.,.f= function(x){data.frame(state = x[1],
                                         district_file = x[2],
                                         status = x[3])}) %>% 
    data.frame()
  # Merge with existing district list
  district_status = bind_rows(district_status,
                              temp_status) %>% 
    mutate(district_name = str_replace(district_file,".pdf",""))
  
  # file parsing
  temp_tables <- district_tables(district_urls)
  district_status = district_status %>% 
    mutate(nrecords = apply(.,1,function(x) temp_tables[temp_tables$district == x["district_name"],] %>% nrow(.)))
  
  
  
  # writing output
  file_name = str_replace(s_url,"http://rchiips.org/nfhs/","") %>% str_replace(.,".shtml","")
  write.csv(temp_tables,paste0("phase 1 release/districts/csv_output/",file_name,".csv"),row.names = FALSE)
  write_dta(temp_tables,paste0("phase 1 release/districts/stata_output/",file_name,".dta"),version=12)
  
}


# Check for districts with parsing issues ---------
district_obs = data.frame()
district_df = data.frame()
for(d in state_url){
  
  s_url = paste0("http://rchiips.org/nfhs/",d)
  file_name = str_replace(d,".shtml","")
  
  district_urls = read_html(s_url) %>% 
    html_nodes("option") %>% 
    html_attr("value")
  
  match_vec_pdf_to_adhoc = which(district_urls %in% incorrect_match,arr.ind = TRUE)
  match_vec_adhoc_to_pdf = which(incorrect_match %in% district_urls, arr.ind = TRUE)
  district_urls[match_vec_pdf_to_adhoc] = ad_hoc[match_vec_adhoc_to_pdf]
  
  district_urls <- district_urls[-1]
  
  # Status dataset
  temp_obs <- detect_fcts(district_urls)
  temp_obs <- temp_obs %>% 
    map_dfr(.,.f= function(x){data.frame(state = x[1],
                                         district_file = x[2],
                                         status = x[3])}) %>% 
    data.frame()
  district_obs = bind_rows(district_obs,
                              temp_obs) %>% 
    mutate(district_name = str_replace(district_file,".pdf",""))
  
  # Consolidated dataset
  temp_df <- read_dta(paste0("phase 1 release/districts/stata_output/",file_name,".dta"))
  district_df <- bind_rows(district_df,
                               temp_df)
  
  
}

# Check district status - 1
district_obs = district_obs %>% 
  mutate(nrecords = apply(.,1,function(x) district_df[district_df$district == x["district_name"],] %>% nrow(.))) %>% 
  mutate(nrecords = case_when(district_name == "Aurangabad" ~ 104,
                              TRUE ~ as.numeric(nrecords))) %>% 
  arrange(nrecords,status) 
write.csv(district_obs,paste0("phase 1 release/districts/district status.csv"),row.names = FALSE)

# # Individual district checks ---------
# case_check = extract_tables(paste0("C:/Cloud/OneDrive - Emory University/data/NFHS/NFHS5 Factsheets/",
#                      "TG","/","Mahabubnagar.pdf"),
#               guess = TRUE, method="stream",output = "data.frame")
