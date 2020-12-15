## nfhs5_factsheets
NFHS-5 (2019-20) factsheets are now available.   
- States: http://rchiips.org/nfhs/factsheet_NFHS-5.shtml    
- Districts: http://rchiips.org/nfhs/districtfactsheet_NFHS-5.shtml   

1. There is definitely a possibility of errors during both PDF extraction and text preprocessing steps. Please do some eye-ball checks before using the data. Also, the code (R script) is available for you to edit.   
2. The present version of district data has also not been cross-checked extensively. Please use with caution. Also, do let me know if you find any errors.     
3. The factsheets display some numbers within parentheses. The parentheses have been removed, but the comments are included as flags.      
4. Some indicators names include notes. The footnote number in the name has been kept, but the notes have been lost. Please refer to the original PDFs for them.   
5. Indicators that are missing in the PDFs are written with a * or na. They have been missing in the extracted files.       

## Link to download for non-git users
https://github.com/jvargh7/nfhs5_factsheets/archive/main.zip

## Known issues with district parsing

~districts/corrected district status.csv shows the current status of different districts. 

1. There are known conversion issues for some symbols (e.g. $\ge$ (greater than or equal to) is converted to â‰¥)      
2. Minor errors in terms of number of records during extraction of Kheda (GJ), Kangra (HP), Chamba (HP), Dhule (MH), Thane (MH)   


## Resolved issues since last update

1. Kerala and Karnataka are presently displaying 2019-20 factsheets       
2. Kasaragod (KL) is available as of now.  
3. Extraction for some districts in Himachal Pradesh, Maharashtra, Meghalaya and Tripura  have been completed using '~districts/partial district extract.R'. The updated status for those which have been extracted separately is provided in 'corrected district status.csv'. I am retaining 'district status.csv' since it is the primary check for the main run.  
