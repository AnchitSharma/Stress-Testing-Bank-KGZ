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




