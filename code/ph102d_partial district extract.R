
library(tabulizer)
partial_obs <- read_csv(paste0("phase 1 release/districts/district status.csv")) %>% 
  dplyr::filter(nrecords < 100) %>% 
  mutate(state_district = paste0(state,"/",district_file)) %>% 
  arrange(state)


extract_pdftab <- function(s_d_name, area, page = p){
  target_folder <- "C:/Cloud/OneDrive - Emory University/data/NFHS/NFHS5 Factsheets/"
  pdf_file <- paste0(target_folder,s_d_name)
  
  # folder_structure = map(d_urls,
  #                        .f = function(x){
  #                          y = str_split(x,pattern = "/")[[1]][2:3];
  #                          return(y)
  #                        })
  table_output = tryCatch({tab = extract_tables(pdf_file,pages = c(3,4,5),
                                                      guess = FALSE,
                                                      area = area,
                                                      output = "data.frame");
                       tab_clean = consolidate_table(tab)
                       },
                       error = function(e){data.frame(Indicator = NA,
                                                      NFHS5 = NA,
                                                      NFHS4 = NA
                                                      )
                       }) %>%  bind_rows(.)
  
  return(table_output)
}


# HP partial --------------
partial_HP = map(1:12,
                 # 1:nrow(partial_obs),
                 .f = function(f){
                   a = 67.1; #TOP
                   b = 26.1; #LEFT
                   c = 546.3; #WIDTH
                   D = c(446.3,519.67,603.34); #HEIGHT
                   area = list(c(a,b,a+D[1],b+c), c(a,b,a+D[2],b+c),c(a,b,a+D[3],b+c));
                   
                   
                   
                   state = partial_obs$state[f];
                   district_name = partial_obs$district_name[f];
                   
                   tab = tryCatch({extract_pdftab(partial_obs$state_district[f],area = area,page = c(3,4,5)) %>% 
                       mutate(state = state,
                              district = district_name)},
                       error = function(e){
                         data.frame(Indicator = character(),
                                    NFHS5 = numeric(),
                                    NFHS4 = numeric(),
                                    Flag_NFHS5 = character(),
                                    Flag_NFHS4 = character()) %>% 
                           mutate(state = state,
                                  district = district_name)
                         
                       });
                   return(tab)
                   
                   
                 }) %>% 
  bind_rows(.)

# XX partial ----------------
partial_XX = map(1:X,
                 # 1:nrow(partial_obs),
                 .f = function(f){
                   a = 67.1; #TOP
                   b = 26.1; #LEFT
                   c = 546.3; #WIDTH
                   D = c(446.3,519.67,603.34); #HEIGHT
                   area = list(c(a,b,a+D[1],b+c), c(a,b,a+D[2],b+c),c(a,b,a+D[3],b+c));
                   
                   
                   
                   state = partial_obs$state[f];
                   district_name = partial_obs$district_name[f];
                   
                   tab = tryCatch({extract_pdftab(partial_obs$state_district[f],area = area,page = c(3,4,5)) %>% 
                       mutate(state = state,
                              district = district_name)},
                       error = function(e){
                         data.frame(Indicator = character(),
                                    NFHS5 = numeric(),
                                    NFHS4 = numeric(),
                                    Flag_NFHS5 = character(),
                                    Flag_NFHS4 = character()) %>% 
                           mutate(state = state,
                                  district = district_name)
                         
                       });
                   return(tab)
                   
                   
                 }) %>% 
  bind_rows(.)



# Saving
write.csv(partial_HP,paste0("phase 1 release/districts/csv_output/partial_HP.csv"),row.names = FALSE)
write.csv(partial_,paste0("phase 1 release/districts/csv_output/partial_XX.csv"),row.names = FALSE)

library(haven)
write_dta(partial_HP,paste0("phase 1 release/districts/stata_output/partial_HP.dta"),version=12)
write_dta(partial_,paste0("phase 1 release/districts/stata_output/partial_XX.dta"),version=12)

