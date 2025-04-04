library(AFR)
library(tidyverse)
library(ggplot2)
library(readxl)
library(corrplot)
library(lubridate)
library(scales)
library(zoo)
library(combinat)
library(car)
library(lmtest)
library(tseries)
library(MASS)
library(dplyr)
library(ggfortify)
library(GGally)
library(glm2)
library(pscl)
library(MASS)
library(ggpubr)
library(mgcv)
library(knitr)
library(kableExtra)


banking_stats <- read_excel("C:/Users/anchi/my_programs/Test1/Data/banking_statistics.xlsx")

colnames(banking_stats)[1] <- "file_index"

head(banking_stats)
tail(banking_stats)

summary(banking_stats)
str(banking_stats)

# missing values
colSums(is.na(banking_stats))

banking_stats[is.na(banking_stats$Value),]

# checking for duplicated rows
cat("\n Duplicated rows:\n")
duplicated_rows <- banking_stats[duplicated(banking_stats),]

if(nrow(duplicated_rows)>0){
  print(duplicated_rows)
}else{
  cat("No Duplicated rows found\n")
}

# Dropping the column "file name"
banking_stats <- banking_stats %>% 
  dplyr::select(-`File Name`) 

banking_stats <- banking_stats%>%
  rename(Date = `Sheet Name`, Value = `Value`)


# Changing the data types of "Date" and "Value" columns
banking_stats$Date <- as.Date(banking_stats$Date, format = "%d.%m.%Y")
banking_stats$Value <- format(banking_stats$Value, scientific = FALSE)
str(banking_stats)
# dropping duplicates in  date column
banking_stats <- banking_stats[!duplicated(banking_stats$Date, fromLast = TRUE),]

banking_stats <- banking_stats %>% 
  mutate(file_index = seq_along(file_index))
head(banking_stats)
tail(banking_stats)


# filtering dataframe to include data for 3, 6, 9, 12 months
# or 1st, 2nd, 3rd and 4th Quarters

banking_stats_filtered <- banking_stats %>% 
  filter(format(Date, "%m") %in% c("03", "06", "09", "12"))
head(banking_stats_filtered)

banking_stats_filtered$Value <- as.numeric(banking_stats_filtered$Value)
banking_stats_filtered$Value_billion <- banking_stats_filtered$Value/1000000000
str(banking_stats_filtered)
glimpse(banking_stats_filtered)
summary(banking_stats_filtered)

# creating a line plot
ggplot(banking_stats_filtered, aes(x = Date, y = Value_billion))+
  geom_line()+
  geom_point()+
  labs(
    x = "year",
    y = 'Total Assets (Billions KZT)',
    title = "Total Assets of Banking System"
  )+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Operational loss type 1

or_type1 <- as.data.frame(lossdat[[1]])
head(or_type1)
tail(or_type1)
cat("\n Missing values per columns \n")
colSums(is.na(or_type1))

duplicated_rows <- or_type1[duplicated(or_type1),]
if(nrow(duplicated_rows)>0){
  print(duplicated_rows)
}else{
  cat("No duplicated rows found \n")
}

summary(or_type1)
glimpse(or_type1)
str(or_type1)

# histogram of distribution of operational Risk Losses type1
ggplot(or_type1, mapping = aes(x = Loss))+
  geom_histogram(binwidth = 50, fill="blue", color="black", alpha=0.7)+
  labs(title = "histogram of Operation Risk type 1",
       x = "Loss Amount (USD)", y = "Frequency")+
  theme_bw()


# Boxplot of distribution of operational risk losses type1 
ggplot(or_type1, mapping = aes(x = Loss))+
  geom_boxplot(fill="blue", color="black", alpha=0.7)+
  labs(title = "Boxplot of operation risk type 1", x = "Loss Amount (USD)")+
  theme_bw()

# extracting outlier from data
Quartile_1 = quantile(or_type1$Loss, 0.25)
Quartile_2 = quantile(or_type1$Loss, 0.75)

IQR <- Quartile_2 - Quartile_1
lower_bound = Quartile_1 - 1.5*IQR
upper_bound = Quartile_2 + 1.5*IQR

# Identifying the outliers
outlier_loss_1 = or_type1$Loss[or_type1$Loss < lower_bound | or_type1$Loss > upper_bound]
outlier_loss_1

or_type1_cleaned <- or_type1[or_type1$Loss>= lower_bound & or_type1$Loss <= upper_bound,]

summary(or_type1_cleaned)
summary(or_type1)

# Histogram of distribution of Operational Risk Losses type 1
ggplot(data = or_type1_cleaned, aes(x = Loss))+
  geom_histogram(binwidth = 50, fill="blue", color='black', alpha=0.7)+
  labs(title = "histogram of Operational risk type 1 after cleansing from outliers", x = "Loss amount(USD)", y = "frequency")+
  theme_bw()

# Boxplot of distribution of operational risk losses type1
ggplot(data=or_type1_cleaned, aes( x = Loss))+
  geom_boxplot(fill = "blue", color="black", alpha = 0.7)+
  labs(title = "Boxplot of Operational Type 1 Risk after the cleaning from outliers", x = "Loss amount (USD)")+
  theme_bw()

# extracting year and month from date columns
or_type1_cleaned$year = year(or_type1_cleaned$Date)
or_type1_cleaned$Month = month(or_type1_cleaned$Date)

# group by year and month
or_loss_type1_monthly <- or_type1_cleaned %>% 
  dplyr::group_by(year, Month) %>% 
  summarise(Loss_1 = sum(Loss, na.rm = TRUE), .groups = "drop")
head(or_loss_type1_monthly)
str(or_loss_type1_monthly)

# creating "month group" column based on the specified month intervals
or_loss_type1_monthly$Month_Group <- case_when(
  or_loss_type1_monthly$Month %in% c(1, 2, 3)~"Q1",
  or_loss_type1_monthly$Month %in% c(4, 5, 6)~"Q2",
  or_loss_type1_monthly$Month %in% c(7, 8, 9)~"Q3",
  or_loss_type1_monthly$Month %in% c(10, 11, 12 )~"Q4",
  TRUE~"Other"
)

grouped_by_month_type1 <- or_loss_type1_monthly %>% 
  dplyr::group_by(year, Month_Group) %>% 
  summarise(Total_Loss_1 = sum(Loss_1, na.rm = T), .groups = "drop")

grouped_by_month_type1$Year_Quarter <- paste0(grouped_by_month_type1$year, "-Q",
                                              gsub("Q", "", grouped_by_month_type1$Month_Group))

or_loss_type1_quarterly <- grouped_by_month_type1 %>% 
  dplyr::select(Year_Quarter, Total_Loss_1)

# operational risk type 2
or_type2 <- as.data.frame(lossdat[[2]])
head(or_type2)
# missing values
colSums(is.na(or_type2))
# duplicated row
or_type2[duplicated(or_type2),]

summary(or_type2)

ggplot(or_type2, mapping = aes(x = Loss))+
  geom_histogram(binwidth = 50, fill="red", color="black", alpha=0.7)+
  labs(title = "Histogram of operational risk type 2", x = "Loss Amount (USD)", y = "Frequency")+
  theme_bw()

ggplot(or_type2, mapping = aes(x = Loss))+
  geom_boxplot(fill = "red", color="black", alpha = 0.7)+
  labs(title = "Boxplot of operational risk of type 2", x = "Loss Amount (USD)")+
  theme_bw()



# extracting outlier from data
Quartile_1 = quantile(or_type2$Loss, 0.25)
Quartile_2 = quantile(or_type2$Loss, 0.75)

IQR <- Quartile_2 - Quartile_1
lower_bound = Quartile_1 - 1.5*IQR
upper_bound = Quartile_2 + 1.5*IQR

# Identifying the outliers
outlier_loss_2 = or_type2$Loss[or_type2$Loss < lower_bound | or_type2$Loss > upper_bound]
outlier_loss_2

or_type2_cleaned <- or_type2[or_type2$Loss>= lower_bound & or_type2$Loss <= upper_bound,]

summary(or_type2_cleaned)
summary(or_type2)

# Histogram of distribution of Operational Risk Losses type 1
ggplot(data = or_type2_cleaned, aes(x = Loss))+
  geom_histogram(binwidth = 50, fill="blue", color='black', alpha=0.7)+
  labs(title = "histogram of Operational risk type 2 after cleansing from outliers", x = "Loss amount(USD)", y = "frequency")+
  theme_bw()

# Boxplot of distribution of operational risk losses type1
ggplot(data=or_type2_cleaned, aes( x = Loss))+
  geom_boxplot(fill = "blue", color="black", alpha = 0.7)+
  labs(title = "Boxplot of Operational Type 2 Risk after the cleaning from outliers", x = "Loss amount (USD)")+
  theme_bw()

# extracting year and month from date columns
or_type2_cleaned$year = year(or_type2_cleaned$Date)
or_type2_cleaned$Month = month(or_type2_cleaned$Date)

# group by year and month
or_loss_type2_monthly <- or_type2_cleaned %>% 
  dplyr::group_by(year, Month) %>% 
  summarise(Loss_2 = sum(Loss, na.rm = TRUE), .groups = "drop")
head(or_loss_type2_monthly)
str(or_loss_type2_monthly)

# creating "month group" column based on the specified month intervals
or_loss_type2_monthly$Month_Group <- case_when(
  or_loss_type2_monthly$Month %in% c(1, 2, 3)~"Q1",
  or_loss_type2_monthly$Month %in% c(4, 5, 6)~"Q2",
  or_loss_type2_monthly$Month %in% c(7, 8, 9)~"Q3",
  or_loss_type2_monthly$Month %in% c(10, 11, 12 )~"Q4",
  TRUE~"Other"
)

grouped_by_month_type2 <- or_loss_type2_monthly %>% 
  dplyr::group_by(year, Month_Group) %>% 
  summarise(Total_Loss_2 = sum(Loss_2, na.rm = T), .groups = "drop")

grouped_by_month_type2$Year_Quarter <- paste0(grouped_by_month_type2$year, "-Q",
                                              gsub("Q", "", grouped_by_month_type2$Month_Group))

or_loss_type2_quarterly <- grouped_by_month_type2 %>% 
  dplyr::select(Year_Quarter, Total_Loss_2)


# operational risk type 3
or_type3 <- as.data.frame(lossdat[[3]])

head(or_type3)
# missing values
colSums(is.na(or_type3))
# duplicated row
or_type2[duplicated(or_type3),]

summary(or_type3)

ggplot(or_type3, mapping = aes(x = Loss))+
  geom_histogram(binwidth = 50, fill="green", color="black", alpha=0.7)+
  labs(title = "Histogram of operational risk type 3", x = "Loss Amount (USD)", y = "Frequency")+
  theme_bw()

ggplot(or_type3, mapping = aes(x = Loss))+
  geom_boxplot(fill = "green", color="black", alpha = 0.7)+
  labs(title = "Boxplot of operational risk of type 3", x = "Loss Amount (USD)")+
  theme_bw()


# extracting outlier from data
Quartile_1 = quantile(or_type3$Loss, 0.25)
Quartile_2 = quantile(or_type3$Loss, 0.75)

IQR <- Quartile_2 - Quartile_1
lower_bound = Quartile_1 - 1.5*IQR
upper_bound = Quartile_2 + 1.5*IQR

# Identifying the outliers
outlier_loss_3 = or_type3$Loss[or_type3$Loss < lower_bound | or_type3$Loss > upper_bound]
outlier_loss_3

or_type3_cleaned <- or_type3[or_type3$Loss>= lower_bound & or_type3$Loss <= upper_bound,]

summary(or_type3_cleaned)
summary(or_type3)

# Histogram of distribution of Operational Risk Losses type 1
ggplot(data = or_type3_cleaned, aes(x = Loss))+
  geom_histogram(binwidth = 50, fill="blue", color='black', alpha=0.7)+
  labs(title = "histogram of Operational risk type 3 after cleansing from outliers", x = "Loss amount(USD)", y = "frequency")+
  theme_bw()

# Boxplot of distribution of operational risk losses type1
ggplot(data=or_type3_cleaned, aes( x = Loss))+
  geom_boxplot(fill = "blue", color="black", alpha = 0.7)+
  labs(title = "Boxplot of Operational Type 3 Risk after the cleaning from outliers", x = "Loss amount (USD)")+
  theme_bw()

# extracting year and month from date columns
or_type3_cleaned$year = year(or_type3_cleaned$Date)
or_type3_cleaned$Month = month(or_type3_cleaned$Date)

# group by year and month
or_loss_type3_monthly <- or_type3_cleaned %>% 
  dplyr::group_by(year, Month) %>% 
  summarise(Loss_3 = sum(Loss, na.rm = TRUE), .groups = "drop")
head(or_loss_type3_monthly)
str(or_loss_type3_monthly)

# creating "month group" column based on the specified month intervals
or_loss_type3_monthly$Month_Group <- case_when(
  or_loss_type3_monthly$Month %in% c(1, 2, 3)~"Q1",
  or_loss_type3_monthly$Month %in% c(4, 5, 6)~"Q2",
  or_loss_type3_monthly$Month %in% c(7, 8, 9)~"Q3",
  or_loss_type3_monthly$Month %in% c(10, 11, 12 )~"Q4",
  TRUE~"Other"
)

grouped_by_month_type3 <- or_loss_type3_monthly %>% 
  dplyr::group_by(year, Month_Group) %>% 
  summarise(Total_Loss_3 = sum(Loss_3, na.rm = T), .groups = "drop")

grouped_by_month_type3$Year_Quarter <- paste0(grouped_by_month_type3$year, "-Q",
                                              gsub("Q", "", grouped_by_month_type3$Month_Group))

or_loss_type3_quarterly <- grouped_by_month_type3 %>% 
  dplyr::select(Year_Quarter, Total_Loss_3)



# operational risk type 4
or_type4 <- as.data.frame(lossdat[[4]])

head(or_type4)
# missing values
colSums(is.na(or_type4))
# duplicated row
or_type4[duplicated(or_type4),]

summary(or_type4)

ggplot(or_type4, mapping = aes(x = Loss))+
  geom_histogram(binwidth = 50, fill="green", color="black", alpha=0.7)+
  labs(title = "Histogram of operational risk type 4", x = "Loss Amount (USD)", y = "Frequency")+
  theme_bw()

ggplot(or_type4, mapping = aes(x = Loss))+
  geom_boxplot(fill = "green", color="black", alpha = 0.7)+
  labs(title = "Boxplot of operational risk of type 4", x = "Loss Amount (USD)")+
  theme_bw()


# extracting outlier from data
Quartile_1 = quantile(or_type4$Loss, 0.25)
Quartile_2 = quantile(or_type4$Loss, 0.75)

IQR <- Quartile_2 - Quartile_1
lower_bound = Quartile_1 - 1.5*IQR
upper_bound = Quartile_2 + 1.5*IQR

# Identifying the outliers
outlier_loss_4 = or_type4$Loss[or_type4$Loss < lower_bound | or_type4$Loss > upper_bound]
outlier_loss_4

or_type4_cleaned <- or_type4[or_type4$Loss>= lower_bound & or_type4$Loss <= upper_bound,]

summary(or_type4_cleaned)
summary(or_type4)

# Histogram of distribution of Operational Risk Losses type 1
ggplot(data = or_type4_cleaned, aes(x = Loss))+
  geom_histogram(binwidth = 50, fill="blue", color='black', alpha=0.7)+
  labs(title = "histogram of Operational risk type 4 after cleansing from outliers", x = "Loss amount(USD)", y = "frequency")+
  theme_bw()

# Boxplot of distribution of operational risk losses type1
ggplot(data=or_type4_cleaned, aes( x = Loss))+
  geom_boxplot(fill = "blue", color="black", alpha = 0.7)+
  labs(title = "Boxplot of Operational Type 4 Risk after the cleaning from outliers", x = "Loss amount (USD)")+
  theme_bw()

# extracting year and month from date columns
or_type4_cleaned$year = year(or_type4_cleaned$Date)
or_type4_cleaned$Month = month(or_type4_cleaned$Date)

# group by year and month
or_loss_type4_monthly <- or_type4_cleaned %>% 
  dplyr::group_by(year, Month) %>% 
  summarise(Loss_4 = sum(Loss, na.rm = TRUE), .groups = "drop")
head(or_loss_type4_monthly)
str(or_loss_type4_monthly)

# creating "month group" column based on the specified month intervals
or_loss_type4_monthly$Month_Group <- case_when(
  or_loss_type4_monthly$Month %in% c(1, 2, 3)~"Q1",
  or_loss_type4_monthly$Month %in% c(4, 5, 6)~"Q2",
  or_loss_type4_monthly$Month %in% c(7, 8, 9)~"Q3",
  or_loss_type4_monthly$Month %in% c(10, 11, 12 )~"Q4",
  TRUE~"Other"
)

grouped_by_month_type4 <- or_loss_type4_monthly %>% 
  dplyr::group_by(year, Month_Group) %>% 
  summarise(Total_Loss_4 = sum(Loss_4, na.rm = T), .groups = "drop")

grouped_by_month_type4$Year_Quarter <- paste0(grouped_by_month_type4$year, "-Q",
                                              gsub("Q", "", grouped_by_month_type4$Month_Group))

or_loss_type4_quarterly <- grouped_by_month_type4 %>% 
  dplyr::select(Year_Quarter, Total_Loss_4)

or_type2_df <- or_loss_type2_quarterly %>% 
  dplyr::select(Total_Loss_2)

or_type3_df <- or_loss_type3_quarterly %>% 
  dplyr::select(Total_Loss_3)

or_type4_df <- or_loss_type4_quarterly %>% 
  dplyr::select(Total_Loss_4)
# create a combined data frame of operational risk losses
or_losses_total <- cbind(or_loss_type1_quarterly, or_type2_df, or_type3_df, or_type4_df)
view(or_losses_total)

or_losses_total$Total_or_Losses <- or_losses_total$Total_Loss_1 + or_losses_total$Total_Loss_2 + or_losses_total$Total_Loss_3+or_losses_total$Total_Loss_4
or_losses_total <- as.data.frame(or_losses_total)
head(or_losses_total)


# Histogram for total losses
ggplot(or_losses_total, mapping = aes(x = Total_or_Losses))+
  geom_histogram(binwidth = 15000, fill="yellow", color="black", alpha=0.7, aes(y = ..density..))+
  labs(title = "Distribution of Total Operational Risk Losses", 
       x = "Operational loss (amount) in USD", y = "Frequency")+
  scale_x_continuous(labels = scales::label_number())+
  scale_y_continuous(labels = scales::label_number())+
  theme_minimal()

# Boxplot for total operational risk losses
ggplot(or_losses_total, mapping = aes(x = Total_or_Losses))+
  geom_boxplot(fill="yellow", color="black", alpha=0.7)+
  labs(title = "Boxplot of total operational risk losses", x = "Loss amount (USD)")+
  scale_x_continuous(labels = scales::label_number())+
  scale_y_continuous(labels = scales::label_number())+
  theme_bw()

head(macroKZ)
tail(macroKZ)
view(macroKZ)
?macroKZ
macrokz_df <- data.frame(macroKZ)
str(macrokz_df)

colSums(is.na(macrokz_df))
macrokz_df[duplicated(macrokz_df), ]

head(macrokz_df)

time_period <- seq(from = as.Date("2010-04-01"), to=as.Date("2024-04-01"), by="quarter")
macrokz_df$time_period <- time_period
macro_df_filtered <- macrokz_df %>% 
  dplyr::select(-imp, -exp, -GDP_DEF, -realest_resed_prim, -realest_resed_sec,
                -realest_comm, -index_stock_weighted, -ntrade_Agr, -ntrade_Min,
                -ntrade_Man, -ntrade_Elc, -ntrade_Con, ntrade_Trd, ntrade_Trn,
                -ntrade_Inf, -fed_fund_rate, -govsec_rate_kzt_3m, -govsec_rate_kzt_1y,
                -govsec_rate_kzt_7y, -govsec_rate_kzt_10y, -tonia_rate,
                -rate_kzt_mort_0y_1y, -rate_kzt_mort_1y_iy, -rate_kzt_corp_0y_1y,
                -rate_kzt_corp_1y_iy, -rate_usd_corp_0y_1y, -rate_usd_corp_1y_iy,
                -rate_kzt_indv_0y_1y, -rate_kzt_indv_1y_iy, -realest_resed_prim_rus,
                -realest_resed_sec_rus, -cred_portfolio, -coef_k1, -coef_k3,
                -provisions, -percent_margin, -com_inc, -com_exp, -oper_inc,
                -oth_inc, -DR)
head(macro_df_filtered)

# Variables Transformation
# Transfroming the real GDP variable
# Calculating yearly GDP sum (sum of every 4 quarters)
macro_df_filtered$Yearly_GDP_Sum <- c(
  sapply(1:(nrow(macro_df_filtered) - 3),
         function(i) sum(macro_df_filtered$real_gdp[i:(i+3)])
         ), rep(NA, 3)
)
View(macro_df_filtered %>% dplyr::select(GDD_Agr_R, Yearly_GDD_Agr_R_Sum))

# Calculate growth rate as the percentage of current year's GDP
# over the previous year GDP
macro_df_filtered$real_gdp_y <- c(rep(NA, 4),
                                  sapply(5:nrow(macro_df_filtered),
                                         function(i){
                                           previous_year_sum <- sum(macro_df_filtered$real_gdp[(i - 4):(i - 1)])
                                           current_year_sum <- sum(macro_df_filtered$real_gdp[(i):(i + 3)])
                                           growth_rate <- (current_year_sum / previous_year_sum - 1)*100
                                           return(round(growth_rate, 3))
                                         }
                                         )
                                  )

# Transforming the Real Gross Domestic Value added Agriculture variable
macro_df_filtered$Yearly_GDD_Agr_R_Sum <- c(
  sapply(1:(nrow(macro_df_filtered) - 3), 
         function(i) sum(macro_df_filtered$GDD_Agr_R[i:(i+3)])
         ), rep(NA, 3)
)

# Calculating growth rate as the percentage of current year
# Real gross value added Agriculture over previous year's
# Real gross value added Agriculture
macro_df_filtered$GDD_Agr_R_y <- c(
  rep(NA, 4),
  sapply(5:nrow(macro_df_filtered),
         function(i){
           previous_year_sum = sum(macro_df_filtered$GDD_Agr_R[(i -4):(i - 1)])
           current_year_sum = sum(macro_df_filtered$GDD_Agr_R[(i):(i+3)])
           growth_rate <- (current_year_sum / previous_year_sum -1)*100
           return(round(growth_rate, 3))
         }
         )
)

# Transfroming Real gross value added Mining variable
# Calculating Yearly Gross value added Mining
macro_df_filtered$Yearly_GDD_Min_R_Sum <- c(
  sapply(
    1:(nrow(macro_df_filtered)-3),
    function(i) sum(macro_df_filtered$GDD_Min_R[i:(i+3)])
         ),
  rep(NA, 3)
)

# Calculate growth rate as percentage of current year
# Real gross value added Mining over previous year 
# Real gross value added Mining
macro_df_filtered$GDD_Min_R_y <- c(
  rep(NA, 4),
  sapply(5:nrow(macro_df_filtered),
         function(i){
           previous_year_sum <- sum(macro_df_filtered$GDD_Min_R[(i-4):(i-1)])
           current_year_sum <- sum(macro_df_filtered$GDD_Min_R[(i):(i+3)])
           growth_rate <- (current_year_sum/previous_year_sum - 1)*100
           return(round(growth_rate, 3))
         }
         )
)

# Transforming Real gross value added Manufacture variable
# Calculating yearly real gross value added Manufacture
# sum (sum of every 4 quarters)
macro_df_filtered$Yearly_GDD_Man_R_Sum <- c(
  sapply(1:(nrow(macro_df_filtered)-3),
         function(i) sum(macro_df_filtered$GDD_Man_R[i:(i+3)])
         ),
  rep(NA, 3)
) 

# Calculating growth rate as the percentage of current year
# Gross value added Manufacture over previous year
# Gross value added Manufacture
macro_df_filtered$GDD_Man_R_y <- c(
  rep(NA, 4),
  sapply(
    5:nrow(macro_df_filtered),
    function(i){
      previous_year_sum <- sum(macro_df_filtered$GDD_Man_R[(i-4):(i-1)])
      current_year_sum <- sum(macro_df_filtered$GDD_Man_R[(i):(i+3)])
      growth_rate <- (current_year_sum/previous_year_sum -1)*100
      return(round(growth_rate, 3))
    }
  )
)

# Transfromation of Gross value added Electricity variable
# Calculating yearly Gross value added Electricity
macro_df_filtered$Yearly_GDD_Elc_R_Sum <- c(
  sapply(1:(nrow(macro_df_filtered)-3),
         function(i) sum(macro_df_filtered$GDD_Elc_R[i:(i+3)])),
  rep(NA, 3)
)

# Calculating Growth Rate as percentage of current year
# Gross value added Electricity over the previous year
# Gross value added Electricity
macro_df_filtered$GDD_Elc_R_y <- c(
  rep(NA, 4),
  sapply(5:nrow(macro_df_filtered),
         function(i){
           previous_year_sum <- sum(macro_df_filtered$GDD_Elc_R[(i-4):(i-1)])
           current_year_sum <- sum(macro_df_filtered$GDD_Elc_R[(i):(i+3)])
           growth_rate <- (current_year_sum/previous_year_sum-1)*100
           return(round(growth_rate, 3))
         }
         )
)

# Tranformation of Gross value added Construction Variable
# Calculating yearly Gross value added Construction

macro_df_filtered$Yearly_GDD_Con_R_Sum <- c(
  sapply(
    1:(nrow(macro_df_filtered)-3),
    function(i)sum(macro_df_filtered$GDD_Con_R[i:(i+3)])
  ),
  rep(NA, 3)
)

# Calculating Growth rate as percentage of previous year
# Gross value added Construction over the previous year
# Gross value added Construction
macro_df_filtered$GDD_Con_R_y <- c(
  rep(NA, 4),
  sapply(5:nrow(macro_df_filtered),
         function(i){
           previous_year_sum <- sum(macro_df_filtered$GDD_Con_R[(i-4):(i-1)])
           current_year_sum <- sum(macro_df_filtered$GDD_Con_R[(i):(i+3)])
           growth_rate <- (current_year_sum/previous_year_sum - 1)*100
           return(round(growth_rate, 3))
         })
)

colnames(macro_df_filtered)

# Transformation of Real gross value added Trade Variable
# Calculating yearly gross value added Trade
macro_df_filtered$Yearly_GDD_Trd_R_Sum <- c(
  sapply(1:(nrow(macro_df_filtered)-3), 
         function(i) sum(macro_df_filtered$GDD_Trd_R[i:(i+3)])),
  rep(NA, 3)
)

# Calculating growth rate as the percentage of current year 
# gross value added Trade over the previous year
# gross value added Trade

macro_df_filtered$GDD_Trd_R_y <- c(
  rep(NA, 4),
  sapply(5:nrow(macro_df_filtered), 
         function(i){
           previous_year_sum <- sum(macro_df_filtered$GDD_Trd_R[(i-4):(i-1)])
           current_year_sum <- sum(macro_df_filtered$GDD_Trd_R[(i):(i+3)])
           growth_rate <- (current_year_sum/previous_year_sum-1)*100
           return(round(growth_rate, 3))
         })
)


# Transfroming gross value added transportation variable
# calculating yearly gross value added transportion
macro_df_filtered$Yearly_GDD_Trn_R_Sum <- c(
  sapply(1:(nrow(macro_df_filtered)-3),
         function(i) sum(macro_df_filtered$GDD_Trn_R[i:(i+3)])),
  rep(NA,3)
)

# Calculating growth rate as percentage of current_year
# gross value added transportation over previous year
# gross value added
macro_df_filtered$GDD_Trn_R_y <- c(
  rep(NA, 4),
  sapply(5:nrow(macro_df_filtered), 
         function(i){
           previous_year_sum <- sum(macro_df_filtered$GDD_Trn_R[(i-4):(i-1)])
           current_year_sum <- sum(macro_df_filtered$GDD_Trn_R[(i):(i+3)])
           growth_rate <- (current_year_sum/previous_year_sum-1)*100
           return(round(growth_rate, 3))
         })
)


# Transfroming gross value added Infomation variable
# Calculating yearly gross value added information
macro_df_filtered$Yearly_GDD_Inf_R_Sum <- c(
  sapply(
    1:(nrow(macro_df_filtered)-3),
    function(i) sum(macro_df_filtered$GDD_Inf_R[(i):(i+3)])
  ), rep(NA, 3)
)

# Calculating growth rate as percentage of current year
# gross value added Information over previous year
# gross value added Information
macro_df_filtered$GDD_Inf_R_y <- c(
  rep(NA, 4),
  sapply(5:nrow(macro_df_filtered), 
         function(i){
           previous_year_sum <- sum(macro_df_filtered$GDD_Inf_R[(i-4):(i-1)])
           current_year_sum <- sum(macro_df_filtered$GDD_Inf_R[(i):(i+3)])
           growth_rate <- (current_year_sum/previous_year_sum-1)*100
           return(round(growth_rate, 3))
         })
)

# Transfroming gross value added Real Estate variable
# calculating yearly gross value added Real Estate
macro_df_filtered$Yearly_GDD_Est_R_Sum <- c(
  sapply(1:(nrow(macro_df_filtered)-3), 
         function(i) sum(macro_df_filtered$GDD_Est_R[i:(i+3)])
         ), rep(NA,3)
)

# Growth rate as percentage of current year
# gross value added Real Estate over previous year
# gross value added Real Estate
macro_df_filtered$GDD_Est_R_y <- c(
  rep(NA, 4),
  sapply(5:nrow(macro_df_filtered), function(i){
    previous_year_sum <- sum(macro_df_filtered$GDD_Est_R[(i-4):(i-1)])
    current_year_sum <- sum(macro_df_filtered$GDD_Est_R[(i):(i+3)])
    growth_rate = (current_year_sum/previous_year_sum-1)*100
    return(round(growth_rate, 3))
  })
)

# Transfroming real gross value added variable
# Calculating yearly real gross value added
macro_df_filtered$Yearly_GDD_R_Sum <- c(
  sapply(1:(nrow(macro_df_filtered)-3), 
         function(i) sum(macro_df_filtered$GDD_R[(i):(i+3)])),
  rep(NA, 3)
)

# calculating growth rate as percentage of current year
# Real gross value added over previous year
# real gross value added
macro_df_filtered$GDD_R_y <- c(
  rep(NA, 4),
  sapply(5:nrow(macro_df_filtered), function(i){
    previous_year_sum <- sum(macro_df_filtered$GDD_R[(i-4):(i-1)])
    current_year_sum <- sum(macro_df_filtered$GDD_R[(i):(i+3)])
    growth_rate <- (current_year_sum/previous_year_sum-1)*100
    return(round(growth_rate, 3))
  })
)

# macro_df_filtered$Rincpop_q
# transfroming Real average population income monthly variable
# calculating yearly real average population income monthly
macro_df_filtered$Yearly_Rincpop_q_Sum <- c(
  sapply(1:(nrow(macro_df_filtered)-3),
         function(i) sum(macro_df_filtered$Rincpop_q[i:(i+3)])),
  rep(NA, 3)
)

# Calculating growth rate as percentage of current year
# real population average monthly income over previous year
# real population average monthly income
macro_df_filtered$Rincpop_q_y <- c(
  rep(NA, 4),
  sapply(5:(nrow(macro_df_filtered)),
         function(i){
           previous_year_sum <- sum(macro_df_filtered$Rincpop_q[(i-4):(i-1)])
           current_year_sum <- sum(macro_df_filtered$Rincpop_q[(i):(i+3)])
           growth_rate <- (current_year_sum/previous_year_sum-1)*100
           return(round(growth_rate, 3))
         }
         )
)

macro_df_filtered$Rexppop_q
# Transfroming Real population average monthly expense variable
# Calculating Yearly real population average monthly expense
macro_df_filtered$Yearly_Rexppop_q_Sum <- c(
  sapply(1:(nrow(macro_df_filtered)-3), 
         function(i) sum(macro_df_filtered$Rexppop_q[i:(i+3)])),
  rep(NA,3)
)

# calculating growth rate as percentage of current year
# real population average expense over previous year
# real population average expense
macro_df_filtered$Rexppop_q_y <- c(
  rep(NA, 4),
  sapply(5:nrow(macro_df_filtered), 
         function(i){
           previous_year_sum <- sum(macro_df_filtered$Rexppop_q[(i-4):(i-1)])
           current_year_sum <- sum(macro_df_filtered$Rexppop_q[(i):(i+3)])
           growth_rate <- (current_year_sum/previous_year_sum-1)*100
           return(round(growth_rate, 3))
         })
)

macro_df_filtered$Rwage_q
# tranfroming variable real population average wage variable
# calculating yearly real population average wage
macro_df_filtered$Yearly_Rwage_q_Sum <- c(
  sapply(1:(nrow(macro_df_filtered)-3),
         function(i) sum(macro_df_filtered$Rwage_q[i:(i+3)])
         ), rep(NA, 3)
)

# growth rate as percentage of current year
# real population average monthly wage over previous year
# real population average monthly wage
macro_df_filtered$Rwage_q_y <- c(
  rep(NA, 4),
  sapply(5:(nrow(macro_df_filtered)), function(i){
    previous_year_sum <- sum(macro_df_filtered$Rwage_q[(i-4):(i-1)])
    current_year_sum <- sum(macro_df_filtered$Rwage_q[i:(i+3)])
    growth_rate = (current_year_sum/previous_year_sum-1)*100
    return(round(growth_rate,3))
  })
)

# Transformation of Inflatin variable
# Calculating quarterly growth of Inflation

macro_df_filtered$cpi_q <- c(NA, diff(macro_df_filtered$cpi)/
                               head(macro_df_filtered$cpi, -1)*100)

# calculating yearly growth of inflation
macro_df_filtered$cpi_y <- c(rep(NA, 4),
                             sapply(5:nrow(macro_df_filtered),
                                    function(i){
                                      yearly_growth <- (macro_df_filtered$cpi[i] - macro_df_filtered$cpi[i-4])/macro_df_filtered$cpi[i-4]*100
                                      return(round(yearly_growth, 3))
                                    }
                                    )
                             )
str(macro_df_filtered)


# filtering macroeconomic dataframe excluding not transformed variables
macro_df_cleaned <- macro_df_filtered %>% 
  dplyr::select(-real_gdp, -GDD_Agr_R, -GDD_Min_R, -GDD_Man_R,
                -GDD_Elc_R, -GDD_Con_R, -GDD_Trd_R, -GDD_Trn_R,
                -GDD_Inf_R, -GDD_Est_R, -GDD_R, -Rincpop_q,
                -Rexppop_q, -Rwage_q, -Yearly_GDP_Sum, -ntrade_Trd, -ntrade_Trn,
                -Yearly_GDD_Agr_R_Sum, -Yearly_GDD_Min_R_Sum, -Yearly_GDD_Man_R_Sum,
                -Yearly_GDD_Con_R_Sum, -Yearly_GDD_Elc_R_Sum, -Yearly_GDD_Est_R_Sum,
                -Yearly_GDD_Inf_R_Sum, -Yearly_GDD_Trd_R_Sum, -Yearly_GDD_Trn_R_Sum, 
                -Yearly_Rincpop_q_Sum, -Yearly_Rexppop_q_Sum, -Yearly_Rwage_q_Sum, -Yearly_GDD_R_Sum)

str(macro_df_cleaned)


# Creating final data frame for modeling
str(or_losses_total)
table(or_losses_total$Year_Quarter)

# truncating total operational Risk losses data frame from Q1-2011 to Q4-2016
or_losses_total$Year_Quarter <- as.character(or_losses_total$Year_Quarter)
str(or_losses_total)

or_losses_filtered <- or_losses_total %>% 
  filter(Year_Quarter >= "2011-Q1")

# Converting Quarter to Date
or_losses_filtered$Year <- substr(or_losses_filtered$Year_Quarter, 1, 4)
or_losses_filtered$Quarter <- substr(or_losses_filtered$Year_Quarter, 6, 7)
or_losses_filtered$Date_2 <- as.Date(
  paste0(or_losses_filtered$Year, "-", 
         case_when(or_losses_filtered$Quarter == "Q1" ~ "01",
                   or_losses_filtered$Quarter == "Q2" ~ "04",
                   or_losses_filtered$Quarter == "Q3" ~ "07",
                   or_losses_filtered$Quarter == "Q4" ~ "10"
                   ), "-01"
         ), format = "%Y-%m-%d"
) 

head(or_losses_filtered)

usd_kzt_exchange_rate <- c(
  `2016`=	342.16,
  `2015`= 221.73,
  `2014`=	179.19,
  `2013`=	152.13,
  `2012`=	149.11,
  `2011`=	146.62,
  `2010`=	147.35
)

# Defining function to convert Operational Risk Losses to KZT based on the year
convert_to_kzt <- function(year, value){
  exchange_rate <- usd_kzt_exchange_rate[as.character(year)]
  if(!is.null(exchange_rate)){
    return(value * exchange_rate)
  } else {
    return(NA)
  }
}

# Applying the conversion for each relevant column
or_losses_filtered$Total_Loss_1_KZT <- mapply(
  convert_to_kzt, as.numeric(format(or_losses_filtered$Date_2, "%Y")),
  or_losses_filtered$Total_Loss_1
)

or_losses_filtered$Total_Loss_2_KZT <- mapply(
  convert_to_kzt, as.numeric(format(or_losses_filtered$Date_2, "%Y")),
  or_losses_filtered$Total_Loss_2
)

or_losses_filtered$Total_Loss_3_KZT <- mapply(
  convert_to_kzt, as.numeric(format(or_losses_filtered$Date_2, "%Y")),
  or_losses_filtered$Total_Loss_3
)

or_losses_filtered$Total_Loss_4_KZT <- mapply(
  convert_to_kzt, as.numeric(format(or_losses_filtered$Date_2, "%Y")),
  or_losses_filtered$Total_Loss_4
)

or_losses_filtered$Total_or_Losses_KZT <- mapply(
  convert_to_kzt, as.numeric(format(or_losses_filtered$Date_2, "%Y")),
  or_losses_filtered$Total_or_Losses
)


str(banking_stats_filtered)

# Define the range for filtering
start_date <- as.Date("2011-03-01")
end_date <- as.Date("2016-12-01")

# Filtering the data between specified range
banking_stats_filtered_filtered <- banking_stats_filtered %>% 
  filter(Date >= start_date & Date <= end_date)

# Creating combined dataset for banking statistics
bank_x <- cbind(banking_stats_filtered_filtered, or_losses_filtered)


# Creating comparision chart for total assets and Total operational risk losses
bank_x$Date <- as.factor(bank_x$Date)

p <- ggplot(bank_x, aes(x = Date))+
  geom_line(aes(y = Value/10000000000, color="Total Assets"), linewidth=1, group=1)+
  geom_point(aes(y = Value/10000000000, color="Total Assets"), size = 3)+
  geom_line(aes(y = (Total_or_Losses_KZT/1000)*scale_factor, color = "Total Losses"),
            linewidth=1, group=1)+
  geom_point(aes(y = (Total_or_Losses_KZT/1000)*scale_factor, color="Total Losses"), size=3)+
  scale_color_manual(values = c("Total Assets" = "blue", "Total Losses" = "red"))+
  scale_y_continuous(
    name = "Total Assets (Billion KZT)",
    labels = comma,
    sec.axis = sec_axis(~ . / scale_factor,
                        name = "Total Losses (Thousands KZT)",
                        labels = comma
                        )
  )+ labs(
    title = "Comparison of Total Assets to Operational Risk Losses",
    x = "Yearly",
    color = ""
  )+ theme_minimal()+
  theme(
    axis.title.y.left = element_text(color = "blue"),
    axis.text.y.left = element_text(color = "blue"),
    axis.title.y.right = element_text(color = "red"),
    axis.text.y.right = element_text(color = 'red'),
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
scale_factor <- max(bank_x$Value/10000000000)/ max(bank_x$Total_or_Losses_KZT/1000)
print(p)




