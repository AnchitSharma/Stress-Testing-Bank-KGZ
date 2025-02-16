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










