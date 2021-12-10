# Download district factsheets ------------
district_fcts <- function(d_urls){
  
  # Modify to your target folder
  target_folder <- "C:/Cloud/OneDrive - Emory University/data/NFHS/NFHS5 Factsheets"
  
  folder_structure = map(d_urls,
                         .f = function(x){
                           y = str_split(x,pattern = "/")[[1]][2:3];
                           return(y)
                         })
  
  download_status <- map(folder_structure,
                         .f = function(x){
                           x = unlist(x)
                           ifelse(!dir.exists(file.path(target_folder,x[1])), 
                                  dir.create(file.path(target_folder,x[1]),recursive = TRUE), FALSE);
                           y = x
                           
                           y[3] = tryCatch({download.file(url = paste0("http://rchiips.org/nfhs/",
                                                                       
                                                                       case_when(x[1] %in% c("KA","KL") ~ "NFHS-5_FCTS/",
                                                                                 TRUE ~ "NFHS-5_FCTS/"),
                                                                       
                                                                       
                                                                       x[1],"/",x[2]),
                                                          destfile = paste0(target_folder,"/",x[1],"/",x[2]),
                                                          # method = "wget",
                                                          mode = "wb"
                           );
                             
                             status = TRUE
                           },
                           error = function(e){status = FALSE});
                           
                           return(y)
                           
                         })
  
  
}

# Detect downloads -------
detect_fcts <- function(d_urls){
  
  # Modify to your target folder
  target_folder <- "C:/Cloud/OneDrive - Emory University/data/NFHS/NFHS5 Factsheets"
  
  folder_structure = map(d_urls,
                         .f = function(x){
                           y = str_split(x,pattern = "/")[[1]][2:3];
                           return(y)
                         })
  
  download_status <- map(folder_structure,
                         .f = function(x){
                           x = unlist(x)
                           ifelse(!dir.exists(file.path(target_folder,x[1])), 
                                  dir.create(file.path(target_folder,x[1]),recursive = TRUE), FALSE);
                           y = x
                           
                           y[3] = ifelse(file.exists(paste0(target_folder,"/",x[1],"/",x[2])),
                                         TRUE, FALSE);
                           
                           return(y)
                           
                         })
  return(download_status)
  
  
}


# Cleaning function -----------
consolidate_table <- function(example){
  
  # column_names = map(example,
  #                    .f = function(x){
  #                      x[[2]][1]
  #                    }) 
  
  output <- map(example,
                .f = function(x){
                  if(is.character(x)){
                    x = data.frame(Indicator = character(),
                                   NFHS5 = character(),
                                   NFHS4 = character())
                  }
                  if(is.data.frame(x)){
                    # nfhs5_cols = x[[2]][1]
                    
                    # Note: This is a potentially risky if condition
                    if(ncol(x) == 4){
                      names(x) <- c("Indicator","Blank","NFHS5","NFHS4")
                    };
                    if(ncol(x) == 3){
                      names(x) <- c("Indicator","NFHS5","NFHS4")
                    };
                    if(ncol(x) == 2){
                      names(x) <- c("Indicator","NFHS5")
                      x$NFHS4 = NA
                    };
                    
                  }
                  
                  return(x)
                }) %>% 
    bind_rows(.) %>% 
    # separate(col = NFHS5,into = c("Urban","Rural","Total"),sep = " +") %>% 
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
    dplyr::select(Indicator,NFHS5,NFHS4) %>% 
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
                
                return(y)}
              
              
    )
  
  return(output)
  
}

# Extract function -----
district_tables <- function(d_urls){
  # Modify to your source folder
  source_folder <- "C:/Cloud/OneDrive - Emory University/data/NFHS/NFHS5 Factsheets"
  
  # Same as in district_fcts()
  folder_structure = map(d_urls,
                         .f = function(x){
                           y = str_split(x,pattern = "/")[[1]][2:3];
                           return(y)
                         })
  
  table_output = map(folder_structure,
                     .f = function(x){
                       x = unlist(x);
                       tryCatch({tab = extract_tables(paste0(source_folder,"/",x[1],"/",x[2]),
                                                      guess = TRUE, method="stream",output = "data.frame");
                       tab_clean = consolidate_table(tab) %>% 
                         mutate(state = x[1],
                                district = str_replace(x[2],".pdf","")
                         )
                       
                       
                       },
                       error = function(e){data.frame(Indicator = NA,
                                                      NFHS5 = NA,
                                                      NFHS4 = NA,
                                                      state = x[1],
                                                      district = str_replace(x[2],".pdf",""))
                       })
                       
                     }) %>%  bind_rows(.)
  
  
  
  return(table_output)
  
}


extract_pdftab <- function(s_d_name, area, page = p,guess = FALSE){
  target_folder <- "C:/Cloud/OneDrive - Emory University/data/NFHS/NFHS5 Factsheets/"
  pdf_file <- paste0(target_folder,s_d_name)
  
  # folder_structure = map(d_urls,
  #                        .f = function(x){
  #                          y = str_split(x,pattern = "/")[[1]][2:3];
  #                          return(y)
  #                        })
  table_output = tryCatch({tab = extract_tables(pdf_file,pages = c(3,4,5),
                                                guess = guess,
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
