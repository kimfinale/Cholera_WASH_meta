Associations of Water Quality with Cholera in Case-Control Studies
================
2023-08-13

### Load the package

``` r
library (dplyr)
library (metafor)
library (readxl)
```

### Load the function

The “Functions.R” file includes meta-analysis functions tailored to our
analysis including a function to compute standard error based on the 95%
confidence limits.

``` r
source("R/Functions.R") 
```

### Data preparation

Read in the data and clean variables

``` r
## or read data from working directory on computer given that the excel sheet has the name "DataMA"
data <- read_excel ("data/DataMA.xlsx", col_names = TRUE, na ="")

# sapply(data, class)

## adjust classes of data
#data$`Crude OR`<- as.numeric(unlist(data$`Crude OR`[[1]]))
#data$`Matched OR CI UB` <- as.numeric(unlist(data$`Matched OR CI UB`[[1]]))

data$`Crude OR`<- as.numeric(data$`Crude OR`)
data$`Matched OR`<- as.numeric(data$`Matched OR`)
data$`Matched OR CI UB` <- as.numeric(data$`Matched OR CI UB`)
data$`Adjusted OR CI UB` <- as.numeric(data$`Adjusted OR CI UB`)
  
## rename the data
data1 <- data
    
data1 <- data %>% rename ("cOR"    = "Crude OR",
                          "cCI_LB" = "Crude OR CI LB",
                          "cCI_UB" = "Crude OR CI UB",
                          "aOR"    = "Adjusted OR",
                          "mOR"    = "Matched OR",
                          "mCI_LB" = "Matched OR CI LB",
                          "mCI_UB" = "Matched OR CI UB",
                          "aCI_LB" = "Adjusted OR CI LB",
                          "aCI_UB" = "Adjusted OR CI UB")

## remove blank rows with NAs
# data2 <- data1
  
data2 <- data1[!(is.na(data1$cOR) 
                 & is.na(data1$mOR)
                 & is.na(data1$aOR)),]  

## remove values that are not classified
# data3 <- data2 

data3 <- data2[!(data2$`JMP WASH Category` == "Not classified"),]

# remove values not belonging to Water Quality 

# data4 <- data3
# 
# table(data4$`JMP WASH Category`)
    
data4 <- data3[!(data3$`JMP WASH Category`   == "Hygiene - basic")
               & !(data3$`JMP WASH Category` == "Hygiene - limited")
               & !(data3$`JMP WASH Category` == "Hygiene - no facility")
               & !(data3$`JMP WASH Category` == "Sanitation - limited")
               & !(data3$`JMP WASH Category` == "Sanitation - open defecation")
               & !(data3$`JMP WASH Category` == "Sanitation- open defecation")
               & !(data3$`JMP WASH Category` == "Sanitation - safely managed")
               & !(data3$`JMP WASH Category` == "Sanitation - unimproved")
               & !(data3$`JMP WASH Category` == "Water management - safe water storage (Exclude)")
               & !(data3$`JMP WASH Category` == "Water management - unsafe water storage (Exclude)")
               & !(data3$`JMP WASH Category` == "Water source - basic (Contaminated)")
               & !(data3$`JMP WASH Category` == "Water source - limited (Contaminated)")
               & !(data3$`JMP WASH Category` == "Water source - safely managed (Contaminated)")
               & !(data3$`JMP WASH Category` == "Water source - surface water (Exclude)")
               & !(data3$`JMP WASH Category` == "Water source - limited (Exclude)")
               & !(data3$`JMP WASH Category` == "Water source - unimproved (Contaminated)")
               & !(data3$`JMP WASH Category` == "Water source - unimproved (Exclude)")
               & !(data3$`JMP WASH Category` == "Water treatment - treated (Exclude)")
               & !(data3$`JMP WASH Category` == "Water treatment - untreated (Exclude)")
               ,]
  
## remove values without CI
# data5 <- data4
  
data5 <- data4[!(is.na(data4$cCI_LB) &
                   is.na(data4$cCI_UB) &
                   is.na(data4$mCI_LB) &
                   is.na(data4$mCI_UB) &
                   is.na(data4$aCI_LB) &
                   is.na(data4$aCI_UB)) ,]
```

Crate a data frame after extracting categories, lower and upper bounds
of odds ratios

``` r
# # inspect JMP WASH Category
# unique(data5$`JMP WASH Category`)
# data5 %>% count(`JMP WASH Category`)

## create a data frame with all categories needed for the analysis later on
category_meta <- c("Water source - safely managed (Bottled Water)",
                   "Water source - safely managed (Sachet Water)",
                   "Water source - safely managed (Tap Water)",
                   "Water source - basic",
                   "Water source - limited",
                   "Water source - unimproved",
                   "Water source - surface water",
                   "Water treatment - untreated",
                   "Water treatment - treated",
                   "Water treatment - treated (Boiled)",
                   "Water treatment - treated (Chlorinated)",
                   "Water treatment - treated (Materials Observed)",
                   "Water management - safe water storage",
                   "Water management - unsafe water storage")

data5[data5 == 0.00] <- 0.001  # avoid the infinity by adding 0.001 before doing log 

## OR estimates for meta-analysis 
OR <- as.numeric(ifelse(is.na(data5$aOR), 
                        ifelse(is.na(data5$mOR),data5$cOR,data5$mOR),
                        data5$aOR)
                 )
# lower bounds
lower <- as.numeric(ifelse(is.na(data5$aCI_LB),
                           ifelse(is.na(data5$mCI_LB),data5$cCI_LB,data5$mCI_LB),
                           data5$aCI_LB)
                    )
# upper bounds
upper <- as.numeric(ifelse(is.na(data5$aCI_UB),
                           ifelse(is.na(data5$mCI_UB),data5$cCI_UB,data5$mCI_UB),
                           data5$aCI_UB)
                    )

# calculate standard error from 95% confidence limits
se <- calc_se(lower = log(lower), upper = log(upper)) 

## create data frame needed for meta-analysis    
input_data <- data.frame(category  = data5$`JMP WASH Category`,
                         study     = data5$Study,
                         log_lower = log(lower),
                         log_upper = log(upper),
                         log_OR    = log(OR), 
                         SE        = se)
```

### Meta analysis

The user needs a folder named “results” to save the resulting plots

#### Funnel plots

Run analysis, create funnel plot and save plot under the “results”
folder

``` r
# For subcategories of water source
Water_Source_Safely_Bottled_fre <- 
  meta_freq_funnel(input_category = "Water source - safely managed (Bottled Water)",
                   image_png      = "Funnel_Water_source_safely_bottled.png")

Water_Source_Safely_Sachet_fre <- 
  meta_freq_funnel(input_category = "Water source - safely managed (Sachet Water)",
                   image_png      = "Funnel_Water_source_safely_sachet.png")

Water_Source_Safely_Tap_fre <- 
  meta_freq_funnel(input_category = "Water source - safely managed (Tap Water)",
                   image_png      = "Funnel_Water_source_safely_tap.png")
      
Water_Source_Basic_fre <- 
  meta_freq_funnel(input_category = "Water source - basic",
                   image_png      = "Funnel_Water_source_basic.png")

Water_Source_Limited_fre <- 
  meta_freq_funnel(input_category = "Water source - limited",
                   image_png      = "Funnel_Water_source_limited.png")

Water_Source_Unimproved_fre <- 
  meta_freq_funnel(input_category = "Water source - unimproved",
                   image_png      = "Funnel_Water_source_unimproved.png")
      
Water_Source_Surface_water_fre <- 
  meta_freq_funnel(input_category = "Water source - surface water",
                   image_png      = "Funnel_Water_source_surface_water.png")

## For subcategories of water  treatment        
Water_Treatment_Treated_water_fre <-
  meta_freq_funnel(input_category = "Water treatment - treated",
                   image_png      = "Funnel_Water_treatment_treated.png")

Water_Treatment_Treated_Boiled_fre <-
  meta_freq_funnel(input_category = "Water treatment - treated (Boiled)",
                   image_png      = "Funnel_Water_treatment_treated_boiled.png")

Water_Treatment_Treated_Chor_fre <-
  meta_freq_funnel(input_category = "Water treatment - treated (Chlorinated)",
                   image_png      = "Funnel_Water_treatment_treated_chlor.png")

Water_Treatment_Treated_Observ_fre <-
  meta_freq_funnel(input_category = "Water treatment - treated (Materials Observed)",
                   image_png      = "Funnel_Water_treatment_treated_observ.png")

Water_Treatment_Untreated_water_fre <-
  meta_freq_funnel(input_category = "Water treatment - untreated",
                   image_png      = "Funnel_Water_treatment_untreated.png")

## run analysis, create funnel plot and save plot for subcategories of water water storage          
Water_Management_Safe_water_storage_fre <-
  meta_freq_funnel(input_category = "Water management - safe water storage",
                   image_png      = "Funnel_Water_management_safe_water_storage.png")
      
Water_Management_Unsafe_water_storage_fre <-
  meta_freq_funnel(input_category = "Water management - unsafe water storage",
                   image_png      = "Funnel_Water_management_unsafe_water_storage.png")
```

#### Forest plots with exact CIs

Run analysis, create forest plots and save plots

``` r
# For all subcategories of water source
Water_Source_Safely_Bottled_fre <- 
  meta_freq_forest(input_category = "Water source - safely managed (Bottled Water)",
                   image_png      = "Forest_Water_source_safely_bottled.png")

Water_Source_Safely_Sachet_fre <- 
  meta_freq_forest(input_category = "Water source - safely managed (Sachet Water)",
                   image_png      = "Forest_Water_source_safely_sachet.png")

Water_Source_Safely_Tap_fre <- 
  meta_freq_forest(input_category = "Water source - safely managed (Tap Water)",
                   image_png      = "Forest_Water_source_safely_tap.png")

Water_Source_Basic_fre <- 
  meta_freq_forest(input_category = "Water source - basic",
                   image_png      = "Forest_Water_source_basic.png")

Water_Source_Limited_fre <- 
  meta_freq_forest(input_category = "Water source - limited",
                   image_png      = "Forest_Water_source_limited.png")

Water_Source_Unimproved_fre <- 
  meta_freq_forest(input_category = "Water source - unimproved",
                   image_png      = "Forest_Water_source_unimproved.png")

Water_Source_Surface_water_fre <- 
  meta_freq_forest(input_category = "Water source - surface water",
                   image_png      = "Forest_Water_source_surface_water.png")

# For all subcategories of water treatment
Water_Treatment_Treated_water_fre <-
  meta_freq_forest(input_category = "Water treatment - treated",
                   image_png      = "Forest_Water_treatment_treated.png")

Water_Treatment_Treated_Boiled_fre <-
  meta_freq_forest(input_category = "Water treatment - treated (Boiled)",
                   image_png      = "Forest_Water_treatment_treated_boiled.png")

Water_Treatment_Treated_Chor_fre <-
  meta_freq_forest(input_category = "Water treatment - treated (Chlorinated)",
                   image_png      = "Forest_Water_treatment_treated_chlor.png")

Water_Treatment_Treated_Observ_fre <-
  meta_freq_forest(input_category = "Water treatment - treated (Materials Observed)",
                   image_png      = "Forest_Water_treatment_treated_observ.png")

Water_Treatment_Untreated_water_fre <-
  meta_freq_forest(input_category = "Water treatment - untreated",
                   image_png      = "Forest_Water_treatment_untreated.png")

## For all subcategories of water storage
Water_Management_Safe_water_storage_fre <-
  meta_freq_forest(input_category = "Water management - safe water storage",
                   image_png      = "Forest_Water_management_safe_water_storage.png")

Water_Management_Unsafe_water_storage_fre <-
  meta_freq_forest(input_category = "Water management - unsafe water storage",
                   image_png      = "Forest_Water_management_unsafe_water_storage.png")
```

### View results of meta-analysis

``` r
## Water source   
summary(Water_Source_Safely_Bottled_fre, digits=3) 
summary(Water_Source_Safely_Sachet_fre, digits=3)
summary(Water_Source_Safely_Tap_fre, digits=3)
summary(Water_Source_Basic_fre, digits=3)
summary(Water_Source_Limited_fre, digits=3)
summary(Water_Source_Unimproved_fre, digits=3)
summary(Water_Source_Surface_water_fre, digits=3)

## Water treatment
summary(Water_Treatment_Treated_water_fre, digits=2)
summary(Water_Treatment_Treated_Boiled_fre, digits=3)
summary(Water_Treatment_Treated_Chor_fre, digits=2)
summary(Water_Treatment_Treated_Observ_fre, digits=2)
summary(Water_Treatment_Untreated_water_fre, digits=3)

## Water storage
summary(Water_Management_Safe_water_storage_fre, digits=3)
summary(Water_Management_Unsafe_water_storage_fre, digits=3)

## view CI 
confint(Water_Source_Safely_Tap_fre, digits=3)
```

#### Eggers Regression to check for publication bias

``` r
regtest(Water_Source_Safely_Bottled_fre, digits=3)
regtest(Water_Source_Safely_Sachet_fre, digits=3)
regtest(Water_Source_Safely_Tap_fre, digits=3)
regtest(Water_Source_Basic_fre, digits=3)
regtest(Water_Source_Limited_fre, digits=3)
regtest(Water_Source_Unimproved_fre, digits=3)
regtest(Water_Source_Surface_water_fre, digits=3)
regtest(Water_Treatment_Treated_water_fre, digits=3)
regtest(Water_Treatment_Treated_Boiled_fre, digits=3)
regtest(Water_Treatment_Treated_Chor_fre, digits=3)
regtest(Water_Treatment_Treated_Observ_fre, digits=3)
regtest(Water_Treatment_Untreated_water_fre, digits=3)
regtest(Water_Management_Safe_water_storage_fre, digits=3)
regtest(Water_Management_Unsafe_water_storage_fre, digits=3)
```
