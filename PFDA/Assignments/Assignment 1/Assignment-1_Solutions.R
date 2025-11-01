#Set seed to 100
set.seed(100)

#The three month's data: jan, feb, mar
jan_data <- round(rnorm(31, 7, 5), 1)
feb_data <- round(rnorm(28, 14, 7), 1)
mar_data <- round(rnorm(31, 16, 3), 1)

#naming of days
names(jan_data) <- paste0("Jan-", 1:31)
names(feb_data) <- paste0("Feb-", 1:28)
names(mar_data) <- paste0("Mar-", 1:31)

#temp_list created with the monthly data
temp_list <- list(JAN = jan_data, FEB = feb_data, MAR = mar_data)
str(temp_list)
names(temp_list)
names(temp_list$JAN)
names(temp_list$FEB)
names(temp_list$MAR)

#unlist the temp_list and store it in temp_atvec
temp_atvec <- unlist(unname(temp_list))
temp_atvec
length(temp_atvec)

#Summary of data
s_data <- summary(temp_atvec)
s_data

#Frequency [Note: at March 20 I get value as 19.02 which categorizes it as Warm for me instead of mild (the data is 19.0 in the assignment for the same day)]
temp_categories <- ifelse(temp_atvec < 8, "Cold", ifelse(temp_atvec > 19, "Warm", "Mild"))
temp_categories
freq_count <- table(temp_categories)
freq_count

#coldest day, warmest day, day which are cold and days which are mild or warm
coldest_day <- temp_atvec[which.min(temp_atvec)]
coldest_day
warmest_day <- temp_atvec[which.max(temp_atvec)]
warmest_day
days_cold <- temp_atvec[which(temp_categories == "Cold")]
days_cold
days_mild_or_warm <- temp_atvec[which(temp_categories == "Mild" | temp_categories == "Warm")]