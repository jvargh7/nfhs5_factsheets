gc()

library(tidyverse)
library(haven)
library(rvest)
url = "http://rchiips.org/nfhs/Factsheet_Compendium_NFHS-5.shtml"
state_url = read_html(url) %>% 
  html_nodes("option") %>% 
  html_attr("value")
state_url <- state_url[-1] # Removing the title 'Select State'

target_folder <- "C:/Cloud/OneDrive - Emory University/data/NFHS/NFHS5 Factsheets/State Compendium"

# Do you want to download files?
download_files = FALSE # Set to TRUE if you want to download

if(download_files == TRUE){
  download_status <- map(state_url,
                         .f = function(x){
                           x = unlist(x);
                           file_name = str_replace_all(x,"NFHS-5_FCTS/COMPENDIUM/","");
                           
                           y = tryCatch({download.file(url = paste0("http://rchiips.org/nfhs/NFHS-5_FCTS/COMPENDIUM/",
                                                                    file_name),
                                                       destfile = paste0(target_folder,"/",file_name),
                                                       # method = "wget",
                                                       mode = "wb"
                           );
                             
                             status = TRUE
                           },
                           error = function(e){status = FALSE});
                           
                           return(y)
                           
                         })
}




# Function to parse district factsheets

library(tabulizer)

source("code/ph200_functions.R")


# Getting district names --------------
source_folder = target_folder # Where you have downloaded the files
compendium_files = list.files(source_folder)
compendium_files[-which(compendium_files=="Lakshadweep.pdf")]

source("code/ph2d02_district lists manual.R")

district_names <- map_dfr(compendium_files,
                      function(f){
                        print(f);
                        gc();
                        s = str_replace_all(f,"(_|\\.pdf)"," ") %>% str_trim();
                        
                        files_with_issues <- c("Himachal_Pradesh.pdf","Lakshadweep.pdf",
                                               "NCT_Delhi.pdf","Rajasthan.pdf")
                        all_tables <- list();
                        
                        if(f %in% files_with_issues){
                          all_tables[[1]] <- districts_manual(f);
                          
                        };
                        
                        if(!f %in% files_with_issues){
                          all_tables <- extract_tables(paste0(source_folder,"/",f),pages = c(3,4),
                                                       guess = TRUE, method="stream",output = "data.frame");
                        };
                        
                        
                        
                        
                        d_names = all_tables[[1]] %>% 
                          mutate(state_file = f,
                                 state_name = s,
                                 district_name = case_when(str_detect(Content,"[0-9]+") ~ str_replace(Content,"^[0-9]+\\.\\s",""),
                                                           TRUE ~ NA_character_));
                        return(d_names)
                        
                        
                      }) %>%
  dplyr::filter(!is.na(Page.No.)) %>% 
  bind_rows(.,
            extract_tables(paste0(source_folder,"/Uttar_Pradesh.pdf"),pages = c(4),
                           guess = FALSE, method="stream",output = "data.frame") %>% 
              .[[1]] %>% 
              dplyr::filter(!(X == ""|X=="Content")) %>% 
              dplyr::rename(Content = X,
                     Page.No. = X.2) %>% 
  mutate(state_file = "Uttar_Pradesh.pdf",
         state_name = "Uttar Pradesh",
         Page.No. = as.numeric(Page.No.),
         district_name = case_when(str_detect(Content,"[0-9]+") ~ str_replace(Content,"^[0-9]+\\.\\s",""),
                                   TRUE ~ NA_character_)) %>% 
    dplyr::filter(!is.na(Page.No.)),
  extract_tables(paste0(source_folder,"/Madhya_Pradesh.pdf"),pages = c(4),
                 guess = FALSE, method="stream",output = "data.frame") %>% 
    .[[1]] %>% 
    dplyr::filter(!(X == ""|X=="Content")) %>% 
    dplyr::rename(Content = X,
                  Page.No. = X.2) %>% 
    mutate(state_file = "Madhya_Pradesh.pdf",
           state_name = "Madhya Pradesh",
           Page.No. = as.numeric(Page.No.),
           district_name = case_when(str_detect(Content,"[0-9]+") ~ str_replace(Content,"^[0-9]+\\.\\s",""),
                                     TRUE ~ NA_character_)) %>% 
    dplyr::filter(!is.na(Page.No.))
  )

write_csv(district_names,"phase 2 release/districts extract/district_names.csv")


# Extracting all tables except Himachal Pradesh -----

compendium_files %>% print(.)

# files_with_issues <- c(11,17,18,24,28,33)

table_output_extract_1 = map(compendium_files,
                   .f = function(f){
                     tryCatch({
                       gc()
                       print(f);
                       s = str_replace_all(f,"(_|\\.pdf)"," ") %>% str_trim();
                       tab = extract_tables(paste0(source_folder,"/",f),
                                            guess = TRUE, method="stream",output = "data.frame");
                       tab[[1]] <- NULL;
                       tabs_clean = consolidate_table_compendium(tables = tab,
                                                                statefile = f,
                                                                districtnames = district_names)
                       
                       
                       
                     },
                     error = function(e){list(district = data.frame(Indicator = NA,
                                                    NFHS5 = NA,
                                                    NFHS4 = NA,
                                                    state_name = s,
                                                    state_file = f,
                                                    district_name = NA),
                                              state = data.frame(Indicator = NA,
                                                         NFHS5_Total = NA,
                                                         NFHS5_Rural = NA,
                                                         NFHS5_Urban = NA,
                                                         NFHS4 = NA,
                                                         state_name = s,
                                                         state_file = f))
                     })
                     
                   })




saveRDS(table_output_extract_1,"phase 2 release/districts extract/ph201d_output 1.RDS")

compendium_files[files_with_issues]

# table_output_extract_1 <- readRDS("phase 2 release/districts extract/ph201d_output 1.RDS")
