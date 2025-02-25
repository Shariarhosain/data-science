install.packages("dplyr")
library(dplyr)

options(max.print = 3000)

dataset <- read.cdataset <- read.csv("E:/Midterm_Dataset_Section(A).csv", header = TRUE, sep = ",")

dataset

dataset[dataset == ""] <- NA


dataset

sum(is.na(dataset))

colSums(is.na(dataset))

is.na(dataset)

which(is.na(dataset$Age))
which(is.na(dataset$Gender))
which(is.na(dataset$Item.Purchased))
which(is.na(dataset$Frequency.of.Purchases))


missingInstance_remove <- na.omit(dataset)

missingInstance_remove


sum(is.na(missingInstance_remove))
colSums(is.na(missingInstance_remove))


num_cols <- sapply(dataset, is.numeric)
num_cols

categorical_data <- dataset[, !num_cols]

categorical_data[] <- lapply(categorical_data, function(x) {
  mode_value <- names(sort(table(x), decreasing = TRUE))[1]
  x[is.na(x)] <- mode_value
  return(x)
})

sum(is.na(categorical_data))


numerical_data <- dataset[,num_cols]

numerical_data


numerical_data[]<- lapply(numerical_data, function(x) {
  mean_value <- mean(x, na.rm = TRUE)  
  x[is.na(x)] <- mean_value          
  return(x)
})
numerical_data$Age <- round(numerical_data$Age)
sum(is.na(numerical_data))

missing <- colSums(is.na(dataset))


barplot(missing, 
        main = "Missing Values in Each Column", 
        ylab = "Number of Missing Values", 
        col = "blue", 
        las = 3,
        border = "darkblue")


class_counts <- duplicate_value %>%
  count(Frequency.of.Purchases)

minority_class_size <- min(class_counts$n)

balanced_data <- duplicate_value %>%
  group_by(Frequency.of.Purchases) %>%
  sample_n(size = min(n(), minority_class_size)) %>%
  ungroup()


table(balanced_data$Frequency.of.Purchases)




class_counts <- dataset %>%
  count(Frequency.of.Purchases)

majority_class_size <- max(class_counts$n)

balanced_data <- dataset %>%
  group_by(Frequency.of.Purchases) %>%
  sample_n(size = max(n(), majority_class_size), replace = TRUE) %>%
  ungroup()


table(balanced_data$Frequency.of.Purchases)


which(duplicated(missingInstance_remove))

duplicate_value <- distinct(missingInstance_remove)

which(duplicated(duplicate_value))

duplicate_value

f1 <- filter(missingInstance_remove, Age > 1 & Age < 50)
f1
f2 <- filter(missingInstance_remove, Gender == "Male")
f2
f3<-select(missingInstance_remove,Review.Rating)
f3


column_check <- sapply(duplicate_value, is.numeric)

numerical_Catagorical <- duplicate_value[,column_check]

numerical_Catagorical

numerical_Catagorical$Age <- cut(numerical_Catagorical$Age, 
                         breaks = c(0, 24, 39, 59, Inf), 
                         labels = c("Young", "Adult", "Middle-aged", "Senior"), 
                         right = FALSE)


numerical_Catagorical$Purchase.Amount..USD. <- cut(numerical_Catagorical$Purchase.Amount..USD., 
                              breaks = c(0, 50, 100, Inf), 
                              labels = c("Low spender", "Medium spender", "High spender"), 
                              right = FALSE)

numerical_Catagorical$Review.Rating <- cut(numerical_Catagorical$Review.Rating, 
                                           breaks = c(0, 2.9, 3.9, 4.9, 5),  # Adjust the break points
                                           labels = c("Poor", "Average", "Good", "Excellent"),
                                           right = TRUE)  # Use right = TRUE to include the upper boundary


numerical_Catagorical$Previous.Purchases <- cut(numerical_Catagorical$Previous.Purchase, 
                               breaks = c(0, 10, 50, Inf), 
                               labels = c("Few Purchases", "Moderate Purchases", "Frequent Purchases"), 
                               right = FALSE)

print(numerical_Catagorical)



Catagorical_numerical <- duplicate_value[,!column_check]


Catagorical_numerical$Gender <- as.numeric(factor(Catagorical_numerical$Gender))


Catagorical_numerical$Item.Purchased <- as.numeric(factor(Catagorical_numerical$Item.Purchased))


Catagorical_numerical$Category <- as.numeric(factor(Catagorical_numerical$Category))


Catagorical_numerical$Location <- as.numeric(factor(Catagorical_numerical$Location))


Catagorical_numerical$Size <- as.numeric(factor(Catagorical_numerical$Size))


Catagorical_numerical$Color <- as.numeric(factor(Catagorical_numerical$Color))


Catagorical_numerical$Subscription.Status <- as.numeric(factor(Catagorical_numerical$Subscription.Status))


Catagorical_numerical$Discount.Applied <- as.numeric(factor(Catagorical_numerical$Discount.Applied))


Catagorical_numerical$Promo.Code.Used <- as.numeric(factor(Catagorical_numerical$Promo.Code.Used))


Catagorical_numerical$Frequency.of.Purchases <- as.numeric(factor(Catagorical_numerical$Frequency.of.Purchases))



Catagorical_numerical


numerical_Catagorical_normalization <- duplicate_value[,column_check]

summary(numerical_Catagorical_normalization)

Q1 <- quantile(numerical_Catagorical_normalization$Age, 0.25, na.rm = TRUE)
Q3 <- quantile(numerical_Catagorical_normalization$Age, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1


lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR


outliers <- numerical_Catagorical_normalization$Age[numerical_Catagorical_normalization$Age < lower_bound | numerical_Catagorical_normalization$Age > upper_bound]
outliers



outliers_remove <- numerical_Catagorical_normalization %>% filter(Age >= lower_bound & Age <= upper_bound)

summary(outliers_remove)




normalize <- function(column) {
  (column - min(column, na.rm = TRUE)) / (max(column, na.rm = TRUE) - min(column, na.rm = TRUE))
}


outliers_remove$Rating_Normalized <- normalize(outliers_remove$Review.Rating)

outliers_remove <- subset(outliers_remove, select = -Review.Rating)
outliers_remove






