# Comparison - CACODE and GHE

Comparison of cause-specific mortality estimates from CA CODE and GHE. See `src` folder for code and `make.R` for how to run analysis. Processed data outputs are located in `gen` folder. 

See `./data/classification-keys/GHE-CACODE-cause-mapping.csv` for mapping of GHE causes to CA CODE causes. Need to check the CA CODE cause mapping for 00-01m and 01to59m. Does not impact the present analysis though, as only making direct comparisons between GHE and CA CODE estimates for 5-19y. Other raw data used in the analysis has not been pushed to github due to size. 

Figures and tables were generated from the code in `Rmd` files. Click links to `md` files below to see tables and figures compiled with notes.

[boxes.md](https://github.com/hallieeilerts/Comparison-CACODE-GHE/blob/main/boxes.md): Box plots with rates by region (1-4y and 5-9y; 15-19y and 20-24y) for GHE and CA CODE

[tables.md](https://github.com/hallieeilerts/Comparison-CACODE-GHE/blob/main/tables.md): Comparison of GHE and CA CODE mean deaths and CSMFs for 2000-2021 by region for 5-9y, 10-14y, 15-19y

[trends.md](https://github.com/hallieeilerts/Comparison-CACODE-GHE/blob/main/trends.md): Trends in CSMFs and age continuity (1-4y to 5-9y and 15-19y to 20-24y) for GHE and CA CODE