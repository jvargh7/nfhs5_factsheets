
partial_obs <- read_csv(paste0("phase 1 release/districts/district status.csv")) %>% 
  dplyr::filter(nrecords < 100) %>% 
  mutate(state_district = paste0(state,"/",district_file)) %>% 
  arrange(state)

partial_HP <- read_dta(paste0("phase 1 release/districts/stata_output/partial_HP.dta"))
raigarh <- read_dta(paste0("phase 1 release/districts/stata_output/raigarh.dta"))

# Check district status

state_files <- list.files("phase 1 release/districts/stata_output")
state_files <- state_files[regexpr("NFHS",state_files)>0]

district_df <- map_dfr(state_files,
                       function(f){
                         read_dta(paste0("phase 1 release/districts/stata_output/",f))
                       }) %>% 
  dplyr::filter(!(state == "MH" & district == "Raigarh")) %>% 
  dplyr::filter(!district %in% c(partial_obs$district_name)) %>% 
  bind_rows(partial_HP,
            raigarh
            )

# Checks
View(district_df %>% 
       group_by(Indicator) %>% 
       tally())


# Manual edits suggested by @nfra ----------------

# IMPORTANT: Check if order varies after including Phase II
# Incorrect indicator names ------------
district_df[c(11461,23316),"Indicator"] <- "21. Any method6 (%)"
district_df[c(11469,23324),"Indicator"] <- "29. Total unmet need7 (%)"
district_df[c(20798),"Indicator"] <- "102. Men age 15 years and above who use any kind of tobacco (%)"
district_df[c(22666),"Indicator"] <- "98. Ever undergone a screening test for cervical cancer (%)"

# Manual fix of missing items
kangra_31 = data.frame(Indicator = "31. Current users ever told about side effects of current method8 (%)",
                       NFHS5 = 52.2,
                       NFHS4 = 48.1,
                       Flag_NFHS5 = NA,
                       Flag_NFHS4 = "Based on 25-49 unweighted cases",
                       state = "HP",
                       district = "Kangra")
kangra_104 <- data.frame(Indicator = "104. Men age 15 years and above who consume alcohol (%)",
                         NFHS5 = 34.1,
                         NFHS4 = NA,
                         Flag_NFHS5 = NA,
                         Flag_NFHS4 = NA,
                         state = "HP",
                         district = "Kangra")
# 
chamba_31 <- data.frame(Indicator = "31. Current users ever told about side effects of current method8 (%)",
                        NFHS5 = 71.7,
                        NFHS4 = NA,
                        Flag_NFHS5 = "Based on 25-49 unweighted cases",
                        Flag_NFHS4 = "Percentage not shown; based on fewer than 25 unweighted cases",
                        state = "HP",
                        district = "Chamba")
  
dhule_101 <- data.frame(Indicator = "101. Women age 15 years and above who use any kind of tobacco (%)",
                        NFHS5 = NA,
                        NFHS4 = NA,
                        Flag_NFHS5 = NA,
                        Flag_NFHS4 = NA,
                        state = "MH",
                        district = "Dhule")

# Removing Kheda row
  
district_df <- bind_rows(district_df,
                         kangra_31,
                         # kangra_66,
                         kangra_104,
                         chamba_31,
                         dhule_101
                         # thane_103to104
                         ) %>% 
  dplyr::filter(!(district == "Kheda"&state == "GJ"&Indicator ==""))

# Spacing issues
district_df <- district_df %>% 
  mutate(Indicator = case_when(Indicator == "70. Breastfeeding children age 6-23 months receiving an adequate diet16, 17 (%)"~"70. Breastfeeding children age 6-23 months receiving an adequate diet16, 17  (%)",
                               Indicator == "72. Total children age 6-23 months receiving an adequate diet16, 17 (%)" ~ "72. Total children age 6-23 months receiving an adequate diet16, 17  (%)",
                               Indicator == "98.Ever undergone a screening test for cervical cancer (%)" ~ "98. Ever undergone a screening test for cervical cancer (%)",
                               Indicator == "99.Ever undergone a breast examination for breast cancer (%)" ~ "99. Ever undergone a breast examination for breast cancer (%)",
                               Indicator == "97. Elevated blood pressure (Systolic Ã¢â€°Â¥140 mm of Hg and/or Diastolic Ã¢â€°Â¥90 mm of Hg) or taking medicine to control blood pressure" ~ "97. Elevated blood pressure (Systolic Ã¢â€°Â¥140 mm of Hg and/or Diastolic Ã¢â€°Â¥90 mm of Hg) or taking medicine to control blood pressure (%)",
                               Indicator == "94. Elevated blood pressure (Systolic Ã¢â€°Â¥140 mm of Hg and/or Diastolic Ã¢â€°Â¥90 mm of Hg) or taking medicine to control blood pressure" ~ "94. Elevated blood pressure (Systolic Ã¢â€°Â¥140 mm of Hg and/or Diastolic Ã¢â€°Â¥90 mm of Hg) or taking medicine to control blood pressure (%)",
                               Indicator == " 16. Women age 20-24 years married before age 18 years (%)" ~ "16. Women age 20-24 years married before age 18 years (%)",
                               Indicator == " 42. Institutional births (%)" ~ "42. Institutional births (%)",
                               Indicator == " 61. Prevalence of diarrhoea in the 2 weeks preceding the survey (%)" ~ "61. Prevalence of diarrhoea in the 2 weeks preceding the survey (%)",
                               TRUE ~ Indicator))



corrected_status <- read.csv(paste0("phase 1 release/districts/corrected district status.csv")) %>% 
  mutate_at(vars(starts_with("district_")),~as.character(.)) %>% 
  mutate(district_name = case_when(district_name == "Nicobars" ~ "Nicobar",
                                   TRUE ~ district_name),
         district_file = case_when(district_file == "Nicobars.pdf" ~ "Nicobar.pdf",
                                   TRUE ~ district_file))
corrected_status = corrected_status %>% 
  mutate(nrecords = apply(.,1,function(x) district_df[district_df$district == x["district_name"],] %>% nrow(.))) %>% 
  mutate(nrecords = case_when(district_name == "Aurangabad" ~ 104,
                              TRUE ~ as.numeric(nrecords))) %>% 
  arrange(nrecords,status) %>% 
  mutate(version = lubridate::ymd_hms("2020-12-23 16:45:08",tz="EST")) %>% 
  dplyr::select(state,district_name,version,nrecords,district_file,status) %>% 
  mutate(version = case_when(district_name == "Raigarh" & state == "MH" ~ lubridate::ymd_hms("2020-12-23 21:02:00",tz="EST"),
                             
                             TRUE ~ version)) %>% 
  # mutate(version = Sys.time()) %>% 
  arrange(state,district_name)

write.csv(corrected_status,paste0("phase 1 release/districts/corrected district status vDec21.csv"),row.names = FALSE)

write.csv(district_df, paste0("phase 1 release/NFHS-5 Phase 1 District Factsheets vDec21.csv"),row.names=FALSE)
write_dta(district_df, paste0("phase 1 release/NFHS-5 Phase 1 District Factsheets vDec21.dta"),version = 12)


