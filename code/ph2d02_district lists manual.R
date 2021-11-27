
districts_manual <- function(f){
  
  if(f == "NCT_Delhi.pdf"){
    
    nct_districts = data.frame(Content = c("NCT Delhi",
                                           "1. Central",
                                           "2. East",
                                           "3. New Delhi",
                                           "4. North",
                                           "5. North East",
                                           "6. North West",
                                           "7. Shahdara",
                                           "8. South",
                                           "9. South East",
                                           "10. South West",
                                           "11. West"),
                               
                               Page.No. = c(1,7,13,
                                            19,25,31,
                                            37,43,49,55,61,67))
    return(nct_districts)
  }
  
  if(f == "Lakshadweep.pdf"){
    lakshadweep_districts = data.frame(Content = "Lakshadweep",
                                       Page.No. = 1)
    
    return(lakshadweep_districts)
  }
  
  if(f == "Himachal_Pradesh.pdf"){
    
    himachal_districts <- data.frame(Content = c("Himachal Pradesh",
                                                 "1. Bilaspur",
                                                 "2. Chamba",
                                                 "3. Hamirpur",
                                                 "4. Kangra",
                                                 "5. Kinnaur",
                                                 "6. Kullu",
                                                 "7. Lahul and Spiti",# Lahul & Spiti in factsheet
                                                 "8. Mandi",
                                                 "9. Shimla",
                                                 "10. Sirmaur",
                                                 "11. Solan",
                                                 "12. Una"),
                                     Page.No. = c(1,7,13,
                                                  19,25,31,
                                                  37,43,49,
                                                  55,61,67,73))
    
    return(himachal_districts)
  }
  
  if(f == "Rajasthan.pdf"){
    
    rajasthan_districts <- data.frame(Content = c("Rajasthan",
                                                  "1. Ajmer",
                                                  "2. Alwar",
                                                  "3. Banswara",
                                                  "4. Baran",
                                                  "5. Barmer",
                                                  "6. Bharatpur",
                                                  "7. Bhilwara",
                                                  "8. Bikaner",
                                                  "9. Bundi",
                                                  "10. Chittaurgarh",
                                                  "11. Churu",
                                                  "12. Dausa",
                                                  "13. Dhaulpur",
                                                  "14. Dungarpur",
                                                  "15. Ganganagar",
                                                  "16. Hanumangarh",
                                                  "17. Jaipur",
                                                  "18. Jaisalmer",
                                                  "19. Jalor",
                                                  "20. Jhalawar",
                                                  "21. Jhunjhunun",
                                                  "22. Jodhpur",
                                                  "23. Karauli",
                                                  "24. Kota",
                                                  "25. Nagaur",
                                                  "26. Pali",
                                                  "27. Pratapgarh",
                                                  "28. Rajsamand",
                                                  "29. Sawai Madhopur",
                                                  "30. Sikar",
                                                  "31. Sirohi",
                                                  "32. Tonk",
                                                  "33. Udaipur"),
                                      Page.No. = c(1,seq(7,199,by=6)))
    return(rajasthan_districts)
    
    
  }
  

}
