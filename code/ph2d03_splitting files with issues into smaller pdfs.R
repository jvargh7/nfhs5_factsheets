library(tidyverse)
source_folder <- "C:/Cloud/OneDrive - Emory University/data/NFHS/NFHS5 Factsheets/State Compendium"
files_with_issues <- c(11,17,18,24,28,33)
# files_for_splitting <- c(28,29) 
compendium_files_ph203d = list.files(source_folder)[files_with_issues]

district_names <- read_csv("phase 2 release/districts extract/district_names.csv") 

district_names_ph203d <- district_names %>%
  mutate(Page.No. = Page.No. + 4) %>% 
  dplyr::filter(state_file %in% compendium_files_ph203d) %>% 
  group_by(state_name) %>% 
  mutate(stop_page = case_when(is.na(district_name) ~ 10,
                               TRUE ~ dplyr::lead(Page.No.) - 1)) %>% 
  rename(start_page = Page.No.) %>% 
  ungroup()


library(tabulizer)

source("code/ph200_functions.R")
table_output_extract_2 = map(compendium_files_ph203d,
                             .f = function(f){
                               tryCatch({
                                 print(f);
                                 a = 67.1; #TOP
                                 b = 28.1; #LEFT
                                 c = 546.3; #WIDTH
                                 D = c(446.3,519.67,603.34,371.90); #HEIGHT
                                 area = list(c(a,b,a+D[1],b+c), 
                                             c(a,b,a+D[2],b+c),
                                             c(a,b,a+D[3],b+c),
                                             c(a,b,a+D[4],b+c));
                                 
                                 district_pages = district_names_ph203d %>% 
                                   dplyr::filter(state_file == f) %>% 
                                   dplyr::filter(!is.na(district_name))
                                 s = str_replace_all(f,"(_|\\.pdf)"," ") %>% str_trim();
                                 tab_list = map2(district_pages$start_page,
                                                 district_pages$district_name,
                                                 function(d_start,d_name) {;
                                                   gc();
                                                   print(d_name);
                                                   if(d_start == 5){
                                                     area_d = area
                                                     start_table_d = d_start + 2
                                                     stop_table_d = d_start + 5
                                                   }
                                                   if(d_start > 5)
                                                   {
                                                     area_d <- area[1:3]
                                                     start_table_d = d_start + 2
                                                     stop_table_d = d_start + 4
                                                     }
                                                     d_tab_list <- extract_tables(paste0(source_folder,"/",f),
                                                                                  guess = FALSE,
                                                                                  area = area_d,
                                                                                  pages = c(start_table_d:stop_table_d),
                                                                                  method="stream",
                                                                                  output = "data.frame");
                                                   d_n_tabs = length(d_tab_list);
                                                     print(d_n_tabs);
                                                     
                                             
                                               return(list(d_tab_list,
                                                           d_n_tabs))}) 
                                 tab = map(tab_list,
                                           function(t_l){t_l[[1]]})%>% unlist(recursive = FALSE);
                                 n_tabs = map(tab_list,
                                              function(t_l) unlist(t_l[[2]])) %>% as.numeric(.);
                                 district_pages$start_table = c(1,cumsum(n_tabs[-length(n_tabs)]) + 1);
                                 district_pages$stop_table = cumsum(n_tabs);
                                 
                                 tabs_clean = consolidate_table_compendium(tables = tab,
                                                                           statefile = f,
                                                                           districtnames = district_pages,
                                                                           table_indices = TRUE)
                                 
                                 
                                 
                               },
                               error = function(e){list(district = data.frame(Indicator = NA_character_,
                                                                              NFHS5 = NA_character_,
                                                                              NFHS4 = NA_character_,
                                                                              state_name = s,
                                                                              state_file = f,
                                                                              district_name = NA_character_),
                                                        state = data.frame(Indicator = NA_character_,
                                                                           NFHS5_Total = NA_character_,
                                                                           NFHS5_Rural = NA_character_,
                                                                           NFHS5_Urban = NA_character_,
                                                                           NFHS4 = NA_character_,
                                                                           state_name = s,
                                                                           state_file = f))
                               })
                               
                             })


saveRDS(table_output_extract_2,"phase 2 release/districts extract/ph201d_output 2.RDS")
