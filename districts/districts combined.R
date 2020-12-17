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




# Manual edits suggested by @nfra ----------------

# Incorrect indicator names
district_df[c(11461,23108),"Indicator"] <- "21. Any method6 (%)"
district_df[c(11469,23116),"Indicator"] <- "29. Total unmet need7 (%)"
district_df[c(22561),"Indicator"] <- "97. Elevated blood pressure (Systolic Ã¢â€°Â¥140 mm of Hg and/or Diastolic Ã¢â€°Â¥90 mm of Hg) or taking medicine to control blood pressure"
district_df[c(22562),"Indicator"] <- "98.Ever undergone a screening test for cervical cancer (%)"
district_df[c(33834),"Indicator"] <- "38. Mothers who received postnatal care from a doctor/nurse/LHV/ANM/midwife/other health personnel within 2 days of delivery (%)"
district_df[c(33862),"Indicator"] <- "66. Children with fever or symptoms of ARI in the 2 weeks preceding the survey taken to a health facility or health provider (%)"
district_df[c(33223),"Indicator"] <- "49. Children age 12-23 months fully vaccinated based on information from either vaccination card or mother's recall11 (%)"
district_df[c(33224),"Indicator"] <- "50. Children age 12-23 months fully vaccinated based on information from vaccination card only12 (%)"
district_df[c(33240),"Indicator"] <- "67. Children under age 3 years breastfed within one hour of birth15 (%)"
district_df[c(20798),"Indicator"] <- "102. Men age 15 years and above who use any kind of tobacco (%)"

# Manual fix of missing items
kangra_31 = data.frame(Indicator = "31. Current users ever told about side effects of current method8 (%)",
                       NFHS5 = 52.2,
                       NFHS4 = 48.1,
                       Flag_NFHS5 = NA,
                       Flag_NFHS4 = "Based on 25-49 unweighted cases",
                       state = "HP",
                       district = "Kangra")
kangra_66 = data.frame(Indicator = "66. Children with fever or symptoms of ARI in the 2 weeks preceding the survey taken to a health facility or health provider (%)",
                       NFHS5 = 72.4,
                       NFHS4 = NA,
                       Flag_NFHS5 = "Based on 25-49 unweighted cases",
                       Flag_NFHS4 = NA,
                       state = "HP",
                       district = "Kangra")
kangra_104 <- data.frame(Indicator = "104. Men age 15 years and above who consume alcohol (%)",
                         NFHS5 = 34.1,
                         NFHS4 = NA,
                         Flag_NFHS5 = NA,
                         Flag_NFHS4 = NA,
                         state = "HP",
                         district = "Kangra")

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

thane_103to104 <- data.frame(Indicator = c("103. Women age 15 years and above who consume alcohol (%)",
                                           "104. Men age 15 years and above who consume alcohol (%)"),
                             
                             NFHS5 = c(0.4,15.6),
                             NFHS4 = c(NA,NA),
                             Flag_NFHS5 = c(NA,NA),
                             Flag_NFHS4 = c(NA,NA),
                             state = "MH",
                             district = "Thane")

# Removing Kheda row
  
district_df <- bind_rows(district_df,
                         kangra_31,
                         kangra_66,
                         kangra_104,
                         chamba_31,
                         dhule_101,
                         thane_103to104
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

district_df[c(33889),"Indicator"] <- "94. Elevated blood pressure (Systolic Ã¢â€°Â¥140 mm of Hg and/or Diastolic Ã¢â€°Â¥90 mm of Hg) or taking medicine to control blood pressure (%)"
district_df[c(33892),"Indicator"] <- "97. Elevated blood pressure (Systolic Ã¢â€°Â¥140 mm of Hg and/or Diastolic Ã¢â€°Â¥90 mm of Hg) or taking medicine to control blood pressure (%)"



corrected_status = district_obs %>% 
  mutate(nrecords = apply(.,1,function(x) district_df[district_df$district == x["district_name"],] %>% nrow(.))) %>% 
  mutate(nrecords = case_when(district_name == "Aurangabad" ~ 104,
                              TRUE ~ as.numeric(nrecords))) %>% 
  arrange(nrecords,status) %>% 
  mutate(version = lubridate::ymd_hms("2020-12-15 13:11:00",tz="EST")) %>% 
  dplyr::select(state,district_name,version,nrecords,district_file,status) %>% 
  mutate(version = case_when(district_name == "Thane" & state == "MH" ~ Sys.time(),
                             TRUE ~ version))

write.csv(corrected_status,paste0("districts/corrected district status.csv"),row.names = FALSE)

write.csv(district_df, paste0("NFHS-5 District Factsheets.csv"),row.names=FALSE)
write_dta(district_df, paste0("NFHS-5 District Factsheets.dta"),version = 12)


