### Multilateral Development Institution Data
foo <- read.csv("https://tinyurl.com/yb4phxx8") #read in the data

names(foo) 
dim(foo)
head(foo)
date.columns <- c(11, 12, 14, 15, 16, 17, 18, 25)

for(i in date.columns)  
{
  which_values_are_missing <- which(as.character(foo[, i]) == "")
  foo[which_values_are_missing, i] <- NA
  foo[, i] <- as.Date(as.character(foo[, i]))
}

which.have.NAs <- which(is.na(foo$Rating == TRUE)) 
new_foo <- foo[-which.have.NAs, ]
new_foo

which.have.NAs.two <- which(is.na(new_foo$OriginalCompletionDate == TRUE))
new_foo.two <- new_foo[-which.have.NAs.two, ]
new_foo.two

#################################

# Question 1a.1:
#Project Duration at approval is generally 1.5 years (18 months). In other words when projects 
#are approved the difference between the original project completion date and the approval 
#data is 18 months.
#Is this claim true?

right_dates <- subset(new_foo.two, new_foo.two$CirculationDate >= "2008-01-01")
right_dates

v <- rep(NA, nrow(right_dates))

for (i in 1:nrow(right_dates)) {
  v[i] <- if (right_dates$ApprovalDate[i] == 548 + right_dates$OriginalCompletionDate[i]) {
  TRUE
} else {
  FALSE
}
}
summary(v) #there are no true values

################

#Question 1a.2:
#first_five_years
first_five_years <- subset(new_foo.two, new_foo.two$CirculationDate >= "2008-01-01" & new_foo.two$CirculationDate <= "2013-01-01")
mean(first_five_years$CirculationDate)  #mean of first five years of dataset
median(first_five_years$CirculationDate) #median of first five years of dataset
range(first_five_years$CirculationDate) #range of first five years of dataset
quantile(first_five_years$CirculationDate, probs = c(0.25, 0.50, 0.75, 1), type = 1) #quantile of first five years of dataset

first_set <- first_five_years$OriginalCompletionDate - first_five_years$ApprovalDate #defining first half of dataset
first_mean <- mean(first_set)
first_median <- median(first_set) 
first_quantile <- quantile(first_set) 

cat("The mean of the first five years of the dataset is", first_mean)
cat("The median of the first five years of the dataset is", first_median)
cat("The quantile of the first five years of the dataset is", first_quantile)

#last_five_years
last_five_years <- subset(new_foo.two, new_foo.two$CirculationDate >= "2013-01-01" & new_foo.two$CirculationDate<= "2018-01-01")
mean(last_five_years$CirculationDate) #mean of the last five years of the dataset
median(last_five_years$CirculationDate) #median of the last five years of the dataset
range(last_five_years$CirculationDate) #range of the last five years of dataset
quantile(last_five_years$CirculationDate, probs = c(0.25, 0.50, 0.75, 1), type = 1) #quantile of the last five years of dataset

second_set <- last_five_years$OriginalCompletionDate - last_five_years$ApprovalDate #defining second half of dataset
second_mean <- mean(second_set)
second_median <- median(second_set)
second_quantile <- quantile(second_set)

cat("The mean of the last five years of the dataset is", second_mean)
cat("The median of the last five years of the dataset is", second_median)
cat("The quantile of the last five years of the dataset is", second_quantile)

#analysis of first half and second half of data
differencemean <- round(second_mean - first_mean, 1)
differencemedian <- round(second_median - first_median,1)
differencequantile <- round(second_quantile - first_quantile,1)

cat("The projects completed between 2008 and 2013 are completed", differencemean, "days quicker than the projects completed between 2013 and 2018")
cat("The projects completed between 2008 and 2013 are completed", differencemedian, "days quicker than the projects completed between 2013 and 2018")
print("The difference in quantile between the second and first quantile is:")
differencequantile

######################

#Question 1b:
actual_duration <- right_dates$RevisedCompletionDate - right_dates$ApprovalDate
planned_duration <- right_dates$OriginalCompletionDate - right_dates$ApprovalDate

#stats on actual duration
mean(actual_duration) #mean of the actual duration of projects
median(actual_duration) #median of the actual duration of projects
quantile(actual_duration, probs = c(0.25, 0.50, 0.75, 1), type = 1) #quantile of the actual duration of projects

#stats on planned duration
mean(planned_duration) #mean of the planned duration of projects
median(planned_duration) #median of the planned duration of projects
quantile(planned_duration, probs = c(0.25, 0.50, 0.75, 1), type = 1) #quantile of the planned duration of projects

#differences between actual and planned duration
duration_differences <- actual_duration - planned_duration 
mean(duration_differences)
median(duration_differences)
quantile(duration_differences, probs = c(0.25, 0.50, 0.75, 1), type = 1)


#######################

#Question 2:
#What percentage of projects have a rating of 0? 1? 2? 3?
#install.packages("readr")
library(readr)
rating_table <-table(right_dates$Rating)
general_rating_table <- round(prop.table(rating_table) * 100, 1)
rownames(general_rating_table) <- c("Rating of Zero", "Rating of One", "Rating of Two", "Rating of Three")
names(dimnames(general_rating_table)) <- "General Project Ratings"
general_rating_table

########################

#Question 3:
#Question two but not including the PPTA projects! 
library(readr)
data_without_ppta <- subset(right_dates, right_dates$Type != "PPTA")
rating_table_2 <-table(data_without_ppta$Rating)
rating_table_no_ppta <- round(prop.table(rating_table_2) * 100, 1)
rownames(rating_table_no_ppta) <- c("Rating of Zero", "Rating of One", "Rating of Two", "Rating of Three")
names(dimnames(rating_table_no_ppta)) <- "Project Ratings Without PPTA Projects"
rating_table_no_ppta

  

########################

#Question 4: 

b <- 25
bottom_25 <- right_dates[right_dates$RevisedAmount < quantile(right_dates$RevisedAmount,prob=1-b/100),]
table(bottom_25$RevisedAmount)
mean(bottom_25$Rating)
table(bottom_25$Country)
table(bottom_25$Dept)
table(bottom_25$Division)
table(bottom_25$Cluster)
t <- 25
top_25 <- right_dates[right_dates$RevisedAmount > quantile(right_dates$RevisedAmount,prob=1-t/100),]
table(top_25$RevisedAmount)
mean(top_25$Rating)
table(top_25$Country)
table(top_25$Dept)
table(top_25$Division)
table(top_25$Cluster)




