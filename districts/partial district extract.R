
library(tabulizer)
partial_obs <- read_csv(paste0("districts/district status.csv")) %>% 
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

# MH partial --------------
partial_MH = map(13:14,
                 # 1:nrow(partial_obs),
                 .f = function(f){
                   a = 45.48; #TOP
                   b = 33.25; #LEFT
                   c = 521.64; #WIDTH
                   D = c(446.3,510.6,603.34); #HEIGHT
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

# ML partial --------------
partial_ML = map(15:22,
                 # 1:nrow(partial_obs),
                 .f = function(f){
                   a = 66.61; #TOP
                   b = 26.1; #LEFT
                   c = 544.56; #WIDTH
                   D = c(446.3,510.6,603.34); #HEIGHT
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

# TR partial --------------
partial_TR = map(23:29,
                 # 1:nrow(partial_obs),
                 .f = function(f){
                   a = 66.03; #TOP
                   b = 26.1; #LEFT
                   c = 540.24; #WIDTH
                   D = c(446.3,510.6,603.34); #HEIGHT
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
write.csv(partial_HP,paste0("districts/csv_output/partial_HP.csv"),row.names = FALSE)
write.csv(partial_MH,paste0("districts/csv_output/partial_MH.csv"),row.names = FALSE)
write.csv(partial_ML,paste0("districts/csv_output/partial_ML.csv"),row.names = FALSE)
write.csv(partial_TR,paste0("districts/csv_output/partial_TR.csv"),row.names = FALSE)

write_dta(partial_HP,paste0("districts/stata_output/partial_HP.dta"),version=12)
write_dta(partial_MH,paste0("districts/stata_output/partial_MH.dta"),version=12)
write_dta(partial_ML,paste0("districts/stata_output/partial_ML.dta"),version=12)
write_dta(partial_TR,paste0("districts/stata_output/partial_TR.dta"),version=12)

