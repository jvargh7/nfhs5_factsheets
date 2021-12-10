ph201d_output_1 <- readRDS("phase 2 release/districts extract/ph201d_output 1.RDS")

ph201d_output_2 <- readRDS("phase 2 release/districts extract/ph201d_output 2.RDS")
library(tidyverse)

district_names <- read_csv("phase 2 release/districts extract/district_names.csv") %>%
  mutate(Page.No. = Page.No. + 4)



# DISTRICTS -------------
district_output <- bind_rows(
  map_dfr(ph201d_output_1,
          function(t_list){
            return(t_list[["district"]] %>% mutate_all(~as.character(.)))
          }),
map_dfr(ph201d_output_2,
        function(t_list){
          return(t_list[["district"]]%>% mutate_all(~as.character(.)))
        }))

check_district_output <- district_output %>% 
  group_by(Indicator) %>% 
  tally()

writexl::write_xlsx(check_district_output,
                    path = "phase 2 release/districts extract/unique ph201d Indicators.xlsx")

cleaned_check_nfhs5_districts <- readxl::read_excel(path = "phase 2 release/districts extract/unique ph201d Indicators_cleaned.xlsx")


district_output_cleaned <- left_join(district_output %>% mutate(Indicator = str_trim(Indicator)),
                                     cleaned_check_nfhs5_districts %>% 
                                    dplyr::select(-n) %>% 
                                    mutate(Indicator=str_trim(Indicator)),
                                  by=c("Indicator")) %>% 
  dplyr::select(-Indicator) %>% 
  dplyr::filter(!is.na(Indicator_cleaned)) %>% 
  dplyr::rename(Indicator = Indicator_cleaned,
                state = state_name,
                district = district_name,
                file = state_file) %>% 
  dplyr::select(state,district,Indicator,NFHS5,NFHS4,starts_with("Flag")) %>% 
  distinct(state,district,Indicator,NFHS5,.keep_all = TRUE)

write.csv(district_output_cleaned,"phase 2 release/NFHS-5 Phase 2 District Factsheets.csv",row.names = FALSE)
haven::write_dta(district_output_cleaned,"phase 2 release/NFHS-5 Phase 2 District Factsheets.dta",version=12)

district_output_cleaned %>% 
  group_by(state,district) %>% 
  tally() %>% 
 
  right_join(district_names %>% 
               dplyr::filter(!is.na(district_name)) %>% 
               dplyr::select(state_name,district_name),
             by = c("state"="state_name","district" = "district_name")) %>% 
  mutate(n = case_when(is.na(n) ~ 0,
                       TRUE ~ as.numeric(n))) %>% 
  arrange(n) %>% 
  write_csv(.,"phase 2 release/status by district.csv")

district_output_cleaned %>% 
  group_by(Indicator) %>% 
  tally() %>% 
  mutate(indicator_id = str_extract(Indicator,"^[0-9]+") %>% as.numeric(.)) %>%

  arrange(indicator_id) %>% 
  dplyr::select(-indicator_id) %>% 
  write_csv(.,"phase 2 release/status by Indicator.csv")



# P
state_output <- bind_rows(
  map_dfr(ph201d_output_1,
          function(t_list){
            return(t_list[["state"]]%>% mutate_all(~as.character(.)))
          }),
  map_dfr(ph201d_output_2,
          function(t_list){
            return(t_list[["state"]]%>% mutate_all(~as.character(.)))
          }))

# write_csv(district_names,"phase 2 release/phase 2 compendium district status.csv")

