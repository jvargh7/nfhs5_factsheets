
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
                   c = c(521.64,535.32); #WIDTH (#Palghar and Thane have different width)
                   D = c(446.3,510.6,603.34); #HEIGHT
                   area = list(c(a,b,a+D[1],b+c[f-12]), 
                                c(a,b,a+D[2],b+c[f-12]),
                                c(a,b,a+D[3],b+c[f-12])); 

                   
                   
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
  bind_rows(.) %>% 
  mutate(flag_lead = case_when(lead(Indicator) %in% c("pressure (%)") ~ 1,
                               TRUE ~ 0)) %>% 
  mutate(Indicator = case_when(flag_lead == 1 ~ paste0(Indicator,lead(Indicator)),
                               TRUE ~ Indicator),
         NFHS5 = case_when(flag_lead == 1 ~ lead(NFHS5),
                           TRUE ~ NFHS5),
         NFHS4 = case_when(flag_lead == 1 ~ lead(NFHS4),
                           TRUE ~ NFHS4)) %>% 
  dplyr::filter(!Indicator %in% c("pressure (%)"))  %>% 
  mutate(issue = case_when(is.na(NFHS5) & (!is.na(NFHS4)|!is.na(Flag_NFHS4)) ~ 1,
                           TRUE ~ 0)) %>% 
  mutate(Flag_NFHS5 = case_when(issue == 1 ~ Flag_NFHS4,
                                TRUE ~ Flag_NFHS5),
         
         NFHS5 = case_when(issue == 1 ~ NFHS4,
                           TRUE ~ NFHS5),
         NFHS4 = case_when(issue == 1 ~ NA_real_,
                           TRUE ~ NFHS4),
         Flag_NFHS4 = case_when(issue == 1 ~ "",
                                TRUE ~ Flag_NFHS4),
         
  ) %>% 
  dplyr::select(-issue,-flag_lead)

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

library(haven)
write_dta(partial_HP,paste0("districts/stata_output/partial_HP.dta"),version=12)
write_dta(partial_MH,paste0("districts/stata_output/partial_MH.dta"),version=12)
write_dta(partial_ML,paste0("districts/stata_output/partial_ML.dta"),version=12)
write_dta(partial_TR,paste0("districts/stata_output/partial_TR.dta"),version=12)

