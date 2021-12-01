states = c(
  "Andaman & Nicobar Islands",
  "Andhra Pradesh",
  "Assam",
  "Bihar",
  "Dadra & Nagar Haveli and Daman & Diu",
  "Goa",
  "Gujarat",
  "Himachal Pradesh",
  "Jammu & Kashmir",
  "Karnataka",
  "Kerala",
  "Lakshadweep",
  "Ladakh",
  "Maharashtra",
  "Meghalaya",
  "Manipur",
  "Mizoram",
  "Nagaland",
  "Sikkim",
  "Telangana",
  "Tripura",
  "West Bengal"
)

library(stringr)

for (s in states){
  
    s_url = paste0("http://rchiips.org/nfhs/NFHS-5Reports/",str_replace_all(s,"(\\s|\\&)","_"),".pdf")
    target_path <- "C:/Cloud/OneDrive - Emory University/data/NFHS/NFHS5 Factsheets/State Report"
    
    tryCatch({download.file(s_url,mode = "wb",
                            destfile = paste0(target_path,"/",
                                              str_replace_all(s,"(\\s|\\&)","_"),".pdf"))},
             error = function(e){print(paste0("not downloaded: ",s))})
    
}
