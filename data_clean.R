#####  MIDAS_Walter P Moore_Team 11 Xingyue Zhang #####

# Data Cleaning code

# Install packages 
install.packages('tidyverse')
library('tidyverse')
install.packages('GGally')
library('GGally')
install.packages('IDPmisc')
library('IDPmisc')
# Import dataset
data <- read.csv('Data_Raw.csv')
raw_data <- data
head(data)
# ---------------------------------------------------------------------------------------------------------
# Detect NAN
data %>%
  summarise(count = sum(is.na(colnames(data))))

# Remove data with 'ProjectCode' started with 15
data <- data[!grepl('15',data$ProjectCode),]

# Group by project code and calculate new columns
data_cleaned <- data %>%
                filter(TotalLaborBudgetEffort != 0) %>%
                group_by(ProjectCode) %>%
                  summarise(
                            LaborCost = max(LaborCostPTD, na.rm = TRUE),
                            Overhead = max(OverheadPTD, na.rm = TRUE),
                            OverheadRate = mean(ifelse((LaborCostMTD == 0),0,LaborOverheadMTD/LaborCostMTD)),
                            LaborRevenue = max(LaborRevenuePTD, na.rm = TRUE),
                            CompleteLabor = max(LaborRevenuePTD)/max(TotalLaborBudgetEffort),
                            ProfitPTD = max(RevenuePTD) - (max(CostPTD) + max(Overhead)),
                            WIP_PTD = max(max(RevenuePTD)-max(BillingPTD)),
                            ProfitPct = max(ifelse((RevenuePTD == 0),0,ProfitPTD/max(RevenuePTD)))) %>%
                  filter(ProfitPct > -1 & ProfitPct < 1)

# delete outliers
source('remove_outliers.R')

data_cleaned  <- data_cleaned[data_cleaned$OverheadRate %in% remove_outliers(data_cleaned$OverheadRate),]

data_cleaned <- data_cleaned %>%
  NaRV.omit()

# classify WIP to 0 and 1, 0 means WIP > 0 AND 1 means WIP <= 0
data_cleaned$WIP_PTD <- ifelse(data_cleaned$WIP_PTD > 0,0,1)
# ---------------------------------------------------------------------------------------------------------
# Pairwise Plot
p1 <- data_cleaned %>%
        ggpairs(columns = c(2:6,8,9))

# Correlation Matrix
cols <- c(2:6,8,9)
p2 <- ggcorr(data_cleaned[,cols],label = TRUE)  

write.csv(data_cleaned,'data_cleaned.csv')
