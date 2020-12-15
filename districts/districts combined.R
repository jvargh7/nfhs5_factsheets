partial_HP <- read_dta(paste0("districts/stata_output/partial_HP.dta"))
partial_MH <- read_dta(paste0("districts/stata_output/partial_MH.dta"))
partial_ML <- read_dta(paste0("districts/stata_output/partial_ML.dta"))
partial_TR <- read_dta(paste0("districts/stata_output/partial_TR.dta"))


# Check district status
district_df = district_df %>% 
  dplyr::filter(!district %in% partial_obs$district_name) %>% 
  bind_rows(partial_HP,
            partial_MH,
            partial_ML,
            partial_TR)

corrected_status = district_obs %>% 
  mutate(nrecords = apply(.,1,function(x) district_df[district_df$district == x["district_name"],] %>% nrow(.))) %>% 
  mutate(nrecords = case_when(district_name == "Aurangabad" ~ 104,
                              TRUE ~ as.numeric(nrecords))) %>% 
  arrange(nrecords,status) 


write.csv(district_obs,paste0("districts/corrected district status.csv"),row.names = FALSE)

write.csv(district_df, paste0("NFHS-5 District Factsheets.csv"),row.names=FALSE)
write_dta(district_df, paste0("NFHS-5 District Factsheets.dta"),version = 12)

