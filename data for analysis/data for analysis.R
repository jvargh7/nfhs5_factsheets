
india <- read_csv("phase 2 release/NFHS-5 Phase 2 State Factsheets.csv") %>% 
  dplyr::filter(state == "INDIA")

write.csv(india,"data for analysis/india.csv",row.names = FALSE)
haven::write_dta(india,"data for analysis/india.dta",version=12)


states <- bind_rows(read_csv("phase 1 release/NFHS-5 Phase 1 State Factsheets.csv") %>% 
                      dplyr::select(-X1),
                    read_csv("phase 2 release/NFHS-5 Phase 2 State Factsheets.csv") %>% 
                      dplyr::filter(state != "INDIA"))

write.csv(states,"data for analysis/states.csv",row.names = FALSE)
haven::write_dta(states,"data for analysis/states.dta",version=12)


districts <- bind_rows(read_csv("phase 1 release/NFHS-5 Phase 1 District Factsheets.csv") %>% 
                        dplyr::filter(district %in% c("Kangra","Chamba","Dhule")) %>% 
                         mutate(state = case_when(district == "Kangra" ~ "Himachal Pradesh",
                                                  district == "Chamba" ~ "Himachal Pradesh",
                                                  district == "Dhule" ~ "Maharashtra",
                                                  TRUE ~ NA_character_)),
                    read_csv("phase 2 release/NFHS-5 Phase 2 District Factsheets.csv") %>% 
                      dplyr::filter(!district %in% c("Kangra","Chamba","Dhule"))) %>% 
  dplyr::select(state,district,Indicator,everything()) %>% 
  arrange(state,district)

write.csv(districts,"data for analysis/districts.csv",row.names = FALSE)
haven::write_dta(districts,"data for analysis/districts.dta",version=12)
