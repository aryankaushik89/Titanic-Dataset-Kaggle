


df1 <- read.csv("USDA_Macronutrients.csv", na.strings=c("NA", ""), header=TRUE)
df2 <- read.csv("USDA_Micronutrients.csv", na.strings=c("NA", ""), header=TRUE)

USDA <- merge(df1, df2, by="ID")

View(USDA)

sapply(USDA, class)

#' 
#' Changing Description to Character class and sodium + Potassium from factor to numeric class. 

USDA$Description <- as.character(as.factor(USDA$Description))
USDA$Sodium <- as.numeric(as.character(USDA$Sodium))
USDA$Potassium <- as.numeric(as.character(USDA$Potassium))

#' Removing Records with missing values in more than four vectors

keep <- rowSums(is.na(USDA)) < 4
USDA <- USDA[keep, ] 

#' How many records remain? 
#' 6677 records remain in the dataframe.

#' mean value for sugar= 8.22, vitamin E= 1.458, vitamin D=0.5773.
#' 
#' Replacing missing values with mean for the respective vectors.

USDA$Sugar[which(is.na(USDA$Sugar))]<- 8.22

USDA$VitaminE[which(is.na(USDA$VitaminE))]<- 1.458

USDA$VitaminD[which(is.na(USDA$VitaminD))]<- 0.5773

#' Removing all remaining records with missing values

USDAclean<-na.omit(USDA)

#' How many records remain? 
#' 5714 records remain in the dataframe.
#' 

#' Which food has the higest sodium level?
#' Answer: BISCUIT, PLN OR BUTMLK, REFRI DOUGH, HIGHER FAT has the higest sodium at 999. 
#'
#' Creating Scatterplot to compare Fat vs Protien.

plot(USDAclean$TotalFat, USDAclean$Protein, main="Fat vs Protien", 
xlab="Fat", ylab="Protien", col=2, cex=0.2)

#' Creating Histogram to observe the Vitamin C Distribution in foods.

hist(USDAclean$VitaminC, main = "Vitamin C distribution", xlim = c(0,100), breaks = 100)

#' Adding New variable HighSodium. if value for Sodium is > the mean= 197.9, then 1, else 0

USDAclean$HighSodium[USDAclean$Sodium > 197.9] <- 1
USDAclean$HighSodium[USDAclean$Sodium <= 197.9] <- 0

#' Adding New variable HighCalories

USDAclean$HighCalories[USDAclean$Calories > 219.9] <- 1
USDAclean$HighCalories[USDAclean$Calories <= 219.9]  <- 0

#' Adding New variable HighProtien

USDAclean$HighProtien[USDAclean$Protein > 11.73] <- 1
USDAclean$HighProtien[USDAclean$Protein <= 11.73] <- 0

#' Adding New variable HighSugar

USDAclean$HighSugar[USDAclean$Sugar > 8.22] <- 1
USDAclean$HighSugar[USDAclean$Sugar <= 8.22] <- 0

#' Adding New variable HighFat

USDAclean$HighFat[USDAclean$TotalFat > 10.26] <- 1
USDAclean$HighFat[USDAclean$TotalFat <= 10.26] <- 0

#' How many foods have both high sodium and high fat? #16 and 20 refer to the column numbers
#' 
#' answer : 552 foods have both high sodium and high fat.

answer14 <- USDAclean[USDAclean[,16]==1 & USDAclean[,20]==1,]

#' Calculate the average amount of Iron in Foods with low Protien and high Protien.
#' 
#' Assigning rows with 0 in High Protien to iron.lowP, and rows with 1 to iron.highP

iron.lowP<- USDAclean[USDAclean[,18]==0,]
iron.HighP<- USDAclean[USDAclean[,18]==1,]

#' view summary of column Iron in the two new dataframes to find the mean in both high and low Protien foods.

summary(iron.lowP$Iron)
summary(iron.HighP$Iron)

#' Average amount of iron in foods with low Protien = 2.548
#' 
#' Average amount of iron in foods with high Protien = 2.735
#