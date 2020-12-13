data_path <- "C:/Cloud/OneDrive - Emory University/data/NFHS"
# https://cran.r-project.org/web/packages/tabulizer/vignettes/tabulizer.html
library(tabulizer)

nfhs5_data <- extract_tables(paste0(data_path,"/NFHS-5 State Factsheet Compendium_Phase-I.pdf"),
                             guess = TRUE, output = "data.frame")


states = c("Andhra Pradesh",
           "Assam",
           "Bihar",
           "Goa",
           "Gujarat",
           "Himachal Pradesh",
           "Karnataka",
           "Kerala",
           "Maharashtra",
           "Manipur",
           "Meghalaya",
           "Mizoram",
           "Nagaland",
           "Sikkim",
           "Telangana",
           "Tripura",
           "West Bengal",
           "Andaman & Nicobar Islands",
           "Dadra & Nagar Haveli and Daman & Diu",
           "Jammu & Kashmir",
           "Ladakh",
           "Lakshadweep"
           )

# nfhs5_data[[17]] <- NULL
# nfhs5_data[[65]] <- NULL
# nfhs5_data[[44]] <- NULL
nfhs5_tables <- map(nfhs5_data,.f=function(x){
  
  if(ncol(x) == 4){
    names(x) <- c("Indicator","NFHS5","Blank","NFHS4")
  }
  if(ncol(x) == 3){
    names(x) <- c("Indicator","NFHS5","NFHS4")
  }
  return(x)
  }) %>% 
  bind_rows(.) %>% 
  separate(col = NFHS5,into = c("Urban","Rural","Total"),sep = " +") %>% 
  dplyr::filter(!(Indicator %in% c("Indicators",
                                   "Population and Household Profile",
                                   "Characteristics of Adults (age 15-49 years)",
                                   "Marriage and Fertility",
                                   "Infant and Child Mortality Rates (per 1,000 live births)",
                                   "Current Use of Family Planning Methods (currently married women age 15-49 years)",
                                   "Unmet Need for Family Planning (currently married women age 15–49 years)",
                                   "Quality of Family Planning Services",
                                   "Maternal and Child Health",
                                   "Maternity Care (for last birth in the 5 years before the survey)",
                                   "Delivery Care (for births in the 5 years before the survey)",
                                   "Child Vaccinations and Vitamin A Supplementation",
                                   "Treatment of Childhood Diseases (children under age 5 years)",
                                   "Child Feeding Practices and Nutritional Status of Children",
                                   "Nutritional Status of Adults (age 15-49 years)",
                                   "Anaemia among Children and Adults",
                                   "Blood Sugar Level among Adults (age 15 years and above)",
                                   "Women",
                                   "Men",
                                   "Hypertension among Adults (age 15 years and above)",
                                   "Screening for Cancer among Adults (age 30-49 years)",
                                   "Knowledge of HIV/AIDS among Adults (age 15-49 years)",
                                   "Women's Empowerment (women age 15-49 years)",
                                   "Gender Based Violence (age 18-49 years)",
                                   "Tobacco Use and Alcohol Consumption among Adults (age 15 years and above)"
                                   ) | Urban %in% c("Urban","NFHS-5"))) %>% 
  mutate(urban_blank = case_when(Urban == "" ~ 1,
                           TRUE ~ 0),
         prev_urban_blank = case_when(dplyr::lag(Urban) == "" ~ 1,
                                      TRUE ~ 0),
         Indicator = case_when(urban_blank == 1 ~ paste0(Indicator," ",lead(Indicator,1)),
                           TRUE ~ Indicator),
         Urban = case_when(urban_blank == 1 ~ lead(Urban,1),
                           TRUE ~ Urban),
         Rural = case_when(urban_blank == 1 ~ lead(Rural,1),
                           TRUE ~ Rural),
         Total = case_when(urban_blank == 1 ~ lead(Total,1),
                           TRUE ~ Total),
         Blank = case_when(urban_blank == 1 ~ lead(Blank,1),
                           TRUE ~ Blank),
         NFHS4 = case_when(urban_blank == 1 ~ lead(NFHS4,1),
                           TRUE ~ NFHS4)    
         ) %>%
  dplyr::filter(prev_urban_blank == 0) %>% 
  mutate(Total = case_when(is.na(Total) ~ Blank,
                           TRUE ~ Total)) %>% 
  dplyr::select(-Blank) %>% 
  mutate(Indicator = case_when(Indicator == "Current Use of Family Planning Methods (currently married women age 15â€“49 years) 28. Any method6 (%)" ~ "28. Any method6 (%)",
                               Indicator == "Unmet Need for Family Planning (currently married women age 15â€“49 years) 36. Total unmet need7 (%)" ~ "36. Total unmet need7 (%)",
                               Indicator == "99.Blood sugar level - high (141-160 mg/dl)23 (%)" ~ "99. Blood sugar level - high (141-160 mg/dl)23 (%)",
                               Indicator == "78. Breastfeeding children age 6-23 months receiving an adequate diet16, 17  (%)" ~ "78. Breastfeeding children age 6-23 months receiving an adequate diet16, 17 (%)",
                               Indicator == "80. Total children age 6-23 months receiving an adequate diet16, 17  (%)" ~ "80. Total children age 6-23 months receiving an adequate diet16, 17 (%)",
                               TRUE ~ Indicator)) %>% 
  dplyr::select(Indicator:NFHS4) %>% 
  mutate(state_index = case_when(str_detect(Indicator,"1. Female population") ~ 1,
                                 TRUE ~ 0)
         ) %>% 
  mutate(state = states[cumsum(state_index)]) %>% 
  dplyr::select(-state_index) %>% 
  mutate(Flag_Urban = case_when(str_detect(Urban,"\\(") ~ "Based on 25-49 unweighted cases",
                                str_detect(Urban,"\\*") ~ "Percentage not shown; based on fewer than 25 unweighted cases",
                                           TRUE ~ ""),
         Flag_Rural = case_when(str_detect(Rural,"\\(") ~ "Based on 25-49 unweighted cases",
                                str_detect(Rural,"\\*") ~ "Percentage not shown; based on fewer than 25 unweighted cases",
                                TRUE ~ ""),
         Flag_Total = case_when(str_detect(Total,"\\(") ~ "Based on 25-49 unweighted cases",
                                str_detect(Total,"\\*") ~ "Percentage not shown; based on fewer than 25 unweighted cases",
                                TRUE ~ ""),
         Flag_NFHS4 = case_when(str_detect(NFHS4,"\\(") ~ "Based on 25-49 unweighted cases",
                                str_detect(NFHS4,"\\*") ~ "Percentage not shown; based on fewer than 25 unweighted cases",
                                TRUE ~ ""),
         ) %>% 
  mutate_at(vars(Urban:NFHS4), 
            function(x) {y = case_when(
    str_detect(x,"\\(") ~ as.numeric(str_replace_all(x,"[\\(|\\)]","")),
    str_detect(x,",") ~ str_replace(x,",","") %>% as.numeric(),
    x %in% c("n.a","*") ~ NA_real_,
    TRUE ~ as.numeric(x));
  return(y)}
  )


