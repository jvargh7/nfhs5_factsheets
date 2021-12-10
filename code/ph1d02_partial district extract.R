
library(tabulizer)
partial_obs <- read_csv(paste0("phase 1 release/districts/district status.csv")) %>% 
  dplyr::filter(nrecords < 100) %>% 
  mutate(state_district = paste0(state,"/",district_file)) %>% 
  arrange(state)

source("code/ph100_functions.R")



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
raigarh = map_dfr(1,
                 # 1:nrow(partial_obs),
                 .f = function(f){
                   a = c(45.0,43.2,45.0); #TOP
                   b = 34.0; #LEFT
                   c = 535.6; #WIDTH
                   D = c(446.3,519.67,632); #HEIGHT
                   area = list(c(a[1],b,a[1]+D[1],b+c), c(a[2],b,a[1]+D[2],b+c),c(a[3],b,a[1]+D[3],b+c));
                   
                   
                   
                   state = "MH";
                   district_name = "Raigarh";
                   state_district = "MH/Raigarh.pdf"
                   
                   tab = tryCatch({extract_pdftab(state_district,area = area,page = c(3,4,5),guess=FALSE) %>% 
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
write.csv(raigarh,paste0("phase 1 release/districts/csv_output/raigarh.csv"),row.names = FALSE)

library(haven)
write_dta(partial_HP,paste0("phase 1 release/districts/stata_output/partial_HP.dta"),version=12)
write_dta(raigarh,paste0("phase 1 release/districts/stata_output/raigarh.dta"),version=12)

