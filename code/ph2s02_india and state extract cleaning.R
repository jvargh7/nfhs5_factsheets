


nfhs5_tables <- readRDS("phase 2 release/states extract/ph201s_output.RDS") %>% 
  mutate(Indicator = str_replace_all(Indicator,"â€“","-")) %>% 
  mutate(Indicator = str_replace_all(Indicator,"â‰¥",">"))

check_nfhs5_states <- nfhs5_tables %>% 
  group_by(Indicator) %>% 
  tally()

writexl::write_xlsx(check_nfhs5_states,
                    path = "phase 2 release/states extract/unique ph201s Indicators.xlsx")

# Manually cleaned names 

cleaned_check_nfhs5_states <- readxl::read_excel(path = "phase 2 release/states extract/unique ph201s Indicators_cleaned.xlsx")

nfhs5_tables_cleaned <- left_join(nfhs5_tables %>% mutate(Indicator = str_trim(Indicator)),
                                  cleaned_check_nfhs5_states %>% 
                                    dplyr::select(-n) %>% 
                                    mutate(Indicator=str_trim(Indicator)),
                                  by=c("Indicator")) %>% 
  dplyr::select(-Indicator) %>% 
  dplyr::rename(Indicator = Indicator_cleaned) %>% 
  dplyr::select(state,Indicator,Urban,Rural,Total,NFHS4,starts_with("Flag"))



write.csv(nfhs5_tables_cleaned,"phase 2 release/NFHS-5 Phase 2 State Factsheets.csv",row.names = FALSE)
haven::write_dta(nfhs5_tables_cleaned,"phase 2 release/NFHS-5 Phase 2 State Factsheets.dta",version=12)
