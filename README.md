## November 2021: Parsed summary sheets from NFHS-5 Phase 2 Compendium - States and Districts

NFHS-5 (2019-20) factsheets are now available for all states.   
- India and Phase 2 States: http://rchiips.org/nfhs/factsheet_NFHS-5.shtml       
- All States and Districts: http://rchiips.org/nfhs/Factsheet_Compendium_NFHS-5.shtml    

I have updated the folder structure as follows:   
1. **code**: Contains all scripts for both Phase 1 and Phase 2 extracts.   
     
2. **data for analysis**: This contains the consolidated datasets for India, States, Districts in .csv and .dta formats.    
  2.1 india.csv is from 'phase 2 release/NFHS-5 Phase 2 State Factsheets'.     
  2.2 states.csv is from 'phase 1 release/NFHS-5 Phase 1 State Factsheets' and 'phase 2 release/NFHS-5 Phase 2 State Factsheets'.        
  2.3 (PENDING) districts.csv is from 'NFHS-5 Phase 2 State Factsheets' only since these contain all States and UTs  
         
3. **phase 1 release**: The extracts from Phase 1 release only.    
    
4. **phase 2 release**: The extracts from Phase 2 release only.     

### Notes on November 2021 release
1. There is definitely a possibility of errors during both PDF extraction and text preprocessing steps. Please do some eye-ball checks before using the data. Also, the code (R script) is available for you to edit.   
2. The present versions of **data for analysis** have not been cross-checked extensively. Please use with caution. Also, do let me know if you find any errors using the *ISSUES* section (**preferably**) or email me at **jvargh7@emory.edu** .     
3. The factsheets display some numbers within parentheses. **The parentheses have been removed, but the comments are included as flags.**      
4. Some indicators names include notes. The footnote number in the name has been kept, but the notes have been lost. Please refer to the original PDFs for them.   
5. **Missing values in the original PDFs are either na (not available) or "\*" (inadequate sample size).In the extracted files, they are set to missing.**    


## December 2020: Parsed summary sheets from NFHS-5 Phase 1 States and Districts v2.1
Acknowledgements: Aashish Gupta (Twitter: @aashishg_), Sai Ankit Parashar (Twitter: @ankitp654), Pratap Vardhan (@pratapvardhan), Nathan Franz (@nfra) and Shivani Patel (Twitter: @shivania0115).
NFHS-5 (2019-20) factsheets are now available.   
- States: http://rchiips.org/nfhs/factsheet_NFHS-5.shtml    
- Districts: http://rchiips.org/nfhs/districtfactsheet_NFHS-5.shtml   
   

## Link to download for non-git users
https://github.com/jvargh7/nfhs5_factsheets/archive/main.zip  


