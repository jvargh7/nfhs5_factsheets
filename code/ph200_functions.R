# Cleaning function for Compendiums -----------

clean_district_output <- function(district_o){
  
  district_o <- district_o %>% 
    dplyr::filter(!(Indicator %in% c("Indicators",
                                     "Population and Household Profile",
                                     "Characteristics of Adults (age 15-49 years)",
                                     "Characteristics of Women (age 15-49 years)",
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
                                     "Nutritional Status of Women (age 15-49 years)",
                                     "Anaemia among Children and Adults",
                                     "Anaemia among Children and Women",
                                     "Blood Sugar Level among Adults (age 15 years and above)",
                                     "Women",
                                     "Men",
                                     "Hypertension among Adults (age 15 years and above)",
                                     "Screening for Cancer among Adults (age 30-49 years)",
                                     "Screening for Cancer among Women (age 30-49 years)",
                                     "Knowledge of HIV/AIDS among Adults (age 15-49 years)",
                                     "Women's Empowerment (women age 15-49 years)",
                                     "Gender Based Violence (age 18-49 years)",
                                     "Tobacco Use and Alcohol Consumption among Adults (age 15 years and above)"
    ))) %>% 
    mutate(nfhs5_blank = case_when(NFHS5 == "" ~ 1,
                                   TRUE ~ 0),
           prev_nfhs5_blank = case_when(dplyr::lag(NFHS5) == "" ~ 1,
                                        TRUE ~ 0),
           Indicator = case_when(nfhs5_blank == 1 ~ paste0(Indicator," ",lead(Indicator,1)),
                                 TRUE ~ Indicator),
           
           NFHS5 = case_when(nfhs5_blank == 1 ~ lead(NFHS5,1),
                             TRUE ~ NFHS5),
           NFHS4 = case_when(nfhs5_blank == 1 ~ lead(NFHS4,1),
                             TRUE ~ NFHS4)    
    ) %>%
    dplyr::filter(prev_nfhs5_blank == 0) %>% 
    # mutate(Total = case_when(is.na(NFHS5) ~ Blank,
    #                          TRUE ~ Total)) %>% 
    # dplyr::select(-Blank) %>%
    mutate(Indicator = case_when(Indicator == "Characteristics of Women (age 15-49 years) 14. Women who are literate4 (%)" ~ "14. Women who are literate4 (%)",
                                 Indicator == "Current Use of Family Planning Methods (currently married women age 15â€“49 years) 20. Any method6 (%)" ~ "20. Any method6 (%)",
                                 Indicator == "Unmet Need for Family Planning (currently married women age 15â€“49 years) 28. Total unmet need7 (%)" ~ "28. Total unmet need7 (%)",
                                 Indicator == "Nutritional Status of Women (age 15-49 years) 78. Women whose Body Mass Index (BMI) is below normal (BMI <18.5 kg/m2)21 (%)" ~ "78. Women whose Body Mass Index (BMI) is below normal (BMI <18.5 kg/m2)21 (%)",
                                 Indicator == "Anaemia among Children and Women 81. Children age 6-59 months who are anaemic (<11.0 g/dl)22 (%)" ~ "81. Children age 6-59 months who are anaemic (<11.0 g/dl)22 (%)",
                                 Indicator == "Screening for Cancer among Women (age 30-49 years) 98. Ever undergone a screening test for cervical cancer (%)" ~ "98. Ever undergone a screening test for cervical cancer (%)",
                                 Indicator == "Screening for Cancer among Women (age 30-49 years) 98.Ever undergone a screening test for cervical cancer (%)" ~ "98. Ever undergone a screening test for cervical cancer (%)",
                                 Indicator == "99.Blood sugar level - high (141-160 mg/dl)23 (%)" ~ "99. Blood sugar level - high (141-160 mg/dl)23 (%)",
                                 Indicator == "78. Breastfeeding children age 6-23 months receiving an adequate diet16, 17  (%)" ~ "78. Breastfeeding children age 6-23 months receiving an adequate diet16, 17 (%)",
                                 Indicator == "80. Total children age 6-23 months receiving an adequate diet16, 17  (%)" ~ "80. Total children age 6-23 months receiving an adequate diet16, 17 (%)",
                                 TRUE ~ Indicator)) %>%
    dplyr::select(Indicator,NFHS5,NFHS4,
                  state_file,state_name,district_name) %>% 
    mutate(Flag_NFHS5 = case_when(str_detect(NFHS5,"\\(") ~ "Based on 25-49 unweighted cases",
                                  str_detect(NFHS5,"\\*") ~ "Percentage not shown; based on fewer than 25 unweighted cases",
                                  TRUE ~ ""),
           Flag_NFHS4 = case_when(str_detect(NFHS4,"\\(") ~ "Based on 25-49 unweighted cases",
                                  str_detect(NFHS4,"\\*") ~ "Percentage not shown; based on fewer than 25 unweighted cases",
                                  TRUE ~ ""),
    ) %>% 
    mutate_at(vars(NFHS5,NFHS4), 
              function(x) {
                # Check for ()
                # x =str_replace(x,",","");
                y = case_when(
                  str_detect(x,"\\(") ~ as.numeric(str_replace_all(x,"[\\(|\\)]","")),
                  str_detect(x,",") ~ str_replace(x,",","") %>% as.numeric(),
                  x %in% c("n.a","*") ~ NA_real_,
                  TRUE ~ as.numeric(x))
                
                return(y)})
  
  return(district_o)
  
  
  
  
}


clean_state_output <- function(state_o){
  
  if(nrow(state_o)==0){
    return(data.frame(Indicator = NA_character_,
                      state_name = NA_character_,
                      state_file = NA_character_))
  }
  
  state_o_cleaned <- state_o %>% 
    dplyr::filter(!(Indicator %in% c("","Indicators",
                                     "Population and Household Profile",
                                     "Characteristics of Adults (age 15-49 years)",
                                     "Characteristics of Women (age 15-49 years)",
                                     "Marriage and Fertility",
                                     "Infant and Child Mortality Rates (per 1,000 live births)",
                                     "Current Use of Family Planning Methods (currently married women age 15-49 years)",
                                     "Unmet Need for Family Planning (currently married women age 15–49 years)",
                                     "Unmet Need for Family Planning (currently married women age 15-49 years)",
                                     "Quality of Family Planning Services",
                                     "Maternal and Child Health",
                                     "Maternity Care (for last birth in the 5 years before the survey)",
                                     "Delivery Care (for births in the 5 years before the survey)",
                                     "Child Vaccinations and Vitamin A Supplementation",
                                     "Treatment of Childhood Diseases (children under age 5 years)",
                                     "Child Feeding Practices and Nutritional Status of Children",
                                     "Nutritional Status of Adults (age 15-49 years)",
                                     "Nutritional Status of Women (age 15-49 years)",
                                     "Anaemia among Children and Adults",
                                     "Anaemia among Children and Women",
                                     "Blood Sugar Level among Adults (age 15 years and above)",
                                     "Women",
                                     "Men",
                                     "Hypertension among Adults (age 15 years and above)",
                                     "Screening for Cancer among Adults (age 30-49 years)",
                                     "Screening for Cancer among Women (age 30-49 years)",
                                     "Knowledge of HIV/AIDS among Adults (age 15-49 years)",
                                     "Women's Empowerment (women age 15-49 years)",
                                     "Gender Based Violence (age 18-49 years)",
                                     "Tobacco Use and Alcohol Consumption among Adults (age 15 years and above)"
    ))) %>% 
    mutate(NFHS5_Total = case_when(is.na(NFHS5) & is.na(Total) ~ "",
                                   !is.na(Total) ~ Total,
                                   !is.na(NFHS5) ~ NFHS5,
                                   TRUE ~ NA_character_)) %>% 
    mutate(nfhs5_blank = case_when(NFHS5_Total == "" ~ 1,
                                   TRUE ~ 0),
           prev_nfhs5_blank = case_when(dplyr::lag(NFHS5_Total) == "" ~ 1,
                                        TRUE ~ 0),
           Indicator = case_when(nfhs5_blank == 1 ~ paste0(Indicator," ",lead(Indicator,1)),
                                 TRUE ~ Indicator),
           
           NFHS5_Total = case_when(nfhs5_blank == 1 ~ lead(NFHS5_Total,1),
                                   TRUE ~ NFHS5_Total),
           NFHS5_Urban = case_when(nfhs5_blank == 1 ~ lead(Urban,1),
                                   TRUE ~ Urban),
           NFHS5_Rural = case_when(nfhs5_blank == 1 ~ lead(Rural,1),
                                   TRUE ~ Rural),
           NFHS4 = case_when(nfhs5_blank == 1 ~ lead(NFHS4,1),
                             TRUE ~ NFHS4)    
    ) %>%
    dplyr::filter(prev_nfhs5_blank == 0) %>% 
    # mutate(Total = case_when(is.na(NFHS5) ~ Blank,
    #                          TRUE ~ Total)) %>% 
    # dplyr::select(-Blank) %>%
    mutate(Indicator = case_when(Indicator == "Characteristics of Women (age 15-49 years) 14. Women who are literate4 (%)" ~ "14. Women who are literate4 (%)",
                                 Indicator == "Current Use of Family Planning Methods (currently married women age 15â€“49 years) 20. Any method6 (%)" ~ "20. Any method6 (%)",
                                 Indicator == "Unmet Need for Family Planning (currently married women age 15â€“49 years) 28. Total unmet need7 (%)" ~ "28. Total unmet need7 (%)",
                                 Indicator == "Nutritional Status of Women (age 15-49 years) 78. Women whose Body Mass Index (BMI) is below normal (BMI <18.5 kg/m2)21 (%)" ~ "78. Women whose Body Mass Index (BMI) is below normal (BMI <18.5 kg/m2)21 (%)",
                                 Indicator == "Anaemia among Children and Women 81. Children age 6-59 months who are anaemic (<11.0 g/dl)22 (%)" ~ "81. Children age 6-59 months who are anaemic (<11.0 g/dl)22 (%)",
                                 Indicator == "Screening for Cancer among Women (age 30-49 years) 98. Ever undergone a screening test for cervical cancer (%)" ~ "98. Ever undergone a screening test for cervical cancer (%)",
                                 Indicator == "Screening for Cancer among Women (age 30-49 years) 98.Ever undergone a screening test for cervical cancer (%)" ~ "98. Ever undergone a screening test for cervical cancer (%)",
                                 Indicator == "99.Blood sugar level - high (141-160 mg/dl)23 (%)" ~ "99. Blood sugar level - high (141-160 mg/dl)23 (%)",
                                 Indicator == "78. Breastfeeding children age 6-23 months receiving an adequate diet16, 17  (%)" ~ "78. Breastfeeding children age 6-23 months receiving an adequate diet16, 17 (%)",
                                 Indicator == "80. Total children age 6-23 months receiving an adequate diet16, 17  (%)" ~ "80. Total children age 6-23 months receiving an adequate diet16, 17 (%)",
                                 TRUE ~ Indicator)) %>%
    dplyr::select(Indicator,NFHS5_Total,NFHS5_Urban,NFHS5_Rural,NFHS4,
                  state_file,state_name) %>% 
    mutate(Flag_Total = case_when(str_detect(NFHS5_Total,"\\(") ~ "Based on 25-49 unweighted cases",
                                  str_detect(NFHS5_Total,"\\*") ~ "Percentage not shown; based on fewer than 25 unweighted cases",
                                  TRUE ~ ""),
           Flag_Rural = case_when(str_detect(NFHS5_Rural,"\\(") ~ "Based on 25-49 unweighted cases",
                                  str_detect(NFHS5_Rural,"\\*") ~ "Percentage not shown; based on fewer than 25 unweighted cases",
                                  TRUE ~ ""),
           Flag_Urban = case_when(str_detect(NFHS5_Urban,"\\(") ~ "Based on 25-49 unweighted cases",
                                  str_detect(NFHS5_Urban,"\\*") ~ "Percentage not shown; based on fewer than 25 unweighted cases",
                                  TRUE ~ ""),
           Flag_NFHS4 = case_when(str_detect(NFHS4,"\\(") ~ "Based on 25-49 unweighted cases",
                                  str_detect(NFHS4,"\\*") ~ "Percentage not shown; based on fewer than 25 unweighted cases",
                                  TRUE ~ ""),
    ) %>% 
    mutate_at(vars(NFHS5_Total,NFHS5_Rural,NFHS5_Urban,NFHS4), 
              function(x) {
                # Check for ()
                # x =str_replace(x,",","");
                y = case_when(
                  str_detect(x,"\\(") ~ as.numeric(str_replace_all(x,"[\\(|\\)]","")),
                  str_detect(x,",") ~ str_replace(x,",","") %>% as.numeric(),
                  x %in% c("n.a","*") ~ NA_real_,
                  TRUE ~ as.numeric(x))
                
                return(y)})
  
  return(state_o_cleaned)
  
  
  
  
}



consolidate_table_compendium <- function(tables,statefile,districtnames,
                                         table_indices = FALSE){
  
  tables_cleaned = map(tables,function(t){
    if(is.character(t)){
      return(NULL)
    }
    if(is.data.frame(t)){
      return(t)
    }
    
  })
  
  if(!table_indices){
    districts_in_state = districtnames %>% 
      dplyr::filter(state_file == statefile) %>% 
      mutate(start_table = c(1,seq(5,length(tables_cleaned),by=3)),
             stop_table = c(4,seq(7,length(tables_cleaned),by=3)))
  }
  
  if(table_indices){
    districts_in_state = districtnames
  }
  
  output <- map(1:length(tables_cleaned),
                .f = function(i){
                  # print(i);
                  x = tables_cleaned[[i]]
                  # nfhs5_cols = x[[2]][1]
                  
                  # Note: This is a potentially risky if condition
                  if(ncol(x) == 4){
                    names(x) <- c("Indicator","Blank","NFHS5","NFHS4")
                    x <- x %>% 
                      mutate(NFHS5 = case_when(is.na(NFHS5) ~ "",
                                               TRUE ~ as.character(NFHS5)),
                             Blank = case_when(is.na(Blank) ~ "",
                                               TRUE ~ Blank)) %>% 
                      mutate(NFHS5 = paste0(Blank," ",NFHS5)) %>% 
                      dplyr::select(-Blank) %>% 
                      separate(col = NFHS5,into = c("Urban","Rural","Total"),sep = " +")
                    
                  };
                  if(ncol(x) == 3){
                    names(x) <- c("Indicator","NFHS5","NFHS4")
                  };
                  if(ncol(x) == 2){
                    names(x) <- c("Indicator","NFHS5")
                    x$NFHS4 = NA
                  };
                  district_details <- districts_in_state %>% 
                    dplyr::filter(i >= start_table,i<=stop_table) %>% 
                    dplyr::select(-start_table,-stop_table)
                  
                  x <- x %>% 
                    mutate(state_file = district_details$state_file,
                           state_name = district_details$state_name,
                           district_name = district_details$district_name)
                  return(x)
                }) %>% 
    plyr::rbind.fill(.) %>% 
    mutate(Indicator = str_replace_all(Indicator,"â€“","-")) %>% 
    mutate(Indicator = str_replace_all(Indicator,"â‰¥",">"))
  
  
  district_output = output %>% 
    dplyr::filter(!is.na(district_name)) %>% 
    dplyr::select(-one_of("Urban","Rural","Total")) %>% 
    clean_district_output(.)
  
  state_output = output %>% 
    dplyr::filter(is.na(district_name)) %>% 
    dplyr::select(-district_name) %>% 
    clean_state_output(.)
  
  output_compendium = list(
    district = district_output,
    state = state_output
  )
  
  
  return(output_compendium)
  
}