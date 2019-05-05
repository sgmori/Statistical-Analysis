# Reading the files into R
mydata <- read.csv(file.path("c:/Users/samue/Desktop/PREDICT 401 - Data Analysis Assignment 1", "abalones.csv"), sep = "")

# Checking mydata using str()
str(mydata)

# Calculating two new variables: VOLUME and RATIO and add them to mydata
mydata$VOLUME <- mydata$LENGTH * mydata$DIAM * mydata$HEIGHT 
mydata$RATIO <- mydata$SHUCK / mydata$VOLUME

# SEX and CLASS are nominal variables
summary(mydata)

# Table of counts using SEX and CLASS
table_sc <- table(mydata$SEX, mydata$CLASS)
addmargins(table_sc)

# Sex Distribution of Abalone
barplot(table(mydata$SEX, mydata$CLASS)[c(1, 2, 3), ], 
        legend.text = c("Female", "Infant", "Male"), 
        main = "CLASS membership, SEX-differentiated", ylab = "Frequency", 
        xlab = "Age Class", beside = TRUE, col = c("red", "blue", "green"), 
        names.arg = c("A1","A2","A3","A4","A5","A6"))


# Scatterplot matrix for a sample of 200 observations
set.seed(123)
work <- mydata[sample(nrow(mydata), 200),]
plot(work[,2:6])

# Plot of WHOLE versus VOLUME
WHOLE <- mydata$WHOLE
VOLUME <- mydata$VOLUME
plot(VOLUME,WHOLE, main = "Whole weight, as a function of Volume", xlab = "Volume", ylab = "Whole weight", col = "blue")

# Plot of SHUCK versus WHOLE
WHOLE <- mydata$WHOLE
SHUCK <- mydata$SHUCK
plot(WHOLE,SHUCK, main = "Shuck weight, as a function of Whole weight", xlab = "Whole weight", ylab = "Shuck weight", col = "red")
abline(a = 0, b = 0.5621)

# Determine max ratio of shuck vs whole. Calculate ratio then use summary to find max value
RATIO <- SHUCK/WHOLE
summary(RATIO)

# Histograms, boxplots and Q-Q plots of RATIO differentiated by sex
par(mfrow = c(3, 3)) 
female <- mydata[mydata$SEX == "F",]
infant <- mydata[mydata$SEX == "I",]
male <- mydata[mydata$SEX == "M",]
hist(female$RATIO, main="Female RATIO", ylab="Frequency", xlab="", xlim=c(0,0.3), col = "red")
hist(infant$RATIO, main="Infant RATIO", ylab="Frequency", xlab="", xlim=c(0,0.3), col = "green")
hist(male$RATIO, main="Male RATIO", ylab="Frequency", xlab="", xlim=c(0,0.3), col = "blue")
boxplot(female$RATIO, main="Female RATIO", ylab="", xlab="", ylim=c(0,0.3), col = "red") 
boxplot(infant$RATIO, main="Infant RATIO", ylab="", xlab="", ylim=c(0,0.3), col = "green")
boxplot(male$RATIO, main="Male RATIO", ylab="", xlab="", ylim=c(0,0.3), col = "blue")
qqnorm(female$RATIO, main="Female RATIO", ylab="Sample Quantities", xlab="Theoretical Quantities", xlim=c(-3,3), ylim=c(0,0.3), col = "red")
qqline(female$RATIO) 
qqnorm(infant$RATIO, main="Infant RATIO", ylab="Sample Quantities", xlab="Theoretical Quantities", xlim=c(-3,3), ylim=c(0,0.3), col = "green")
qqline(infant$RATIO)
qqnorm(male$RATIO, main="Male RATIO", ylab="Sample Quantities", xlab="Theoretical Quantities", xlim=c(-3,3), ylim=c(0,0.3), col = "blue")
qqline(male$RATIO)
par(mfrow = c(1, 1)) # resets 'mfrow' to default value


# loads packages 
library(ggplot2) 
library(gridExtra) 

# Side-by-side boxplots and scatter plots
grid.arrange( 
  ggplot(mydata, aes(x = CLASS, y = VOLUME, group = CLASS)) + geom_boxplot() + labs(x = "CLASS", y = "VOLUME"), 
  ggplot(mydata, aes(x = CLASS, y = WHOLE, group = CLASS)) + geom_boxplot() + labs(x = "CLASS", y = "WHOLE"), 
  ggplot(mydata, aes(x = RINGS, y = VOLUME)) + geom_point() + labs(x = "RINGS", y = "VOLUME"), 
  ggplot(mydata, aes(x = RINGS, y = WHOLE)) + geom_point() + labs(x = "RINGS", y = "WHOLE"), 
  ncol=2, nrow =2
)

library(ggplot2) 
# ggplot2 
meanVolume <- aggregate(VOLUME ~ SEX + CLASS, data = mydata, mean)
meanShuck <- aggregate(SHUCK ~ SEX + CLASS, data = mydata, mean)
meanRatio <- aggregate(RATIO ~ SEX + CLASS, data = mydata, mean)

rnames <- c("Female", "Infant", "Male")
cnames <- c("A1", "A2", "A3", "A4", "A5", "A6")

matVolume <- round(matrix(meanVolume$VOLUME, nrow = 3, ncol = 6, byrow = FALSE, dimnames = list(rnames, cnames)),2)
matShuck <- round(matrix(meanShuck$SHUCK, nrow = 3, ncol = 6, byrow = FALSE, dimnames = list(rnames, cnames)),2)
matRatio <- round(matrix(meanRatio$RATIO, nrow = 3, ncol = 6, byrow = FALSE, dimnames = list(rnames, cnames)),4)

print(matVolume)
print(matShuck)
print(matRatio)

ggplot(data = meanRatio , aes(x = CLASS, y = RATIO, group = SEX, colour = SEX)) + geom_line() + geom_point(size = 4) + ggtitle("Mean Ratio per Class")

ggplot(data = meanVolume , aes(x = CLASS, y = VOLUME, group = SEX, colour = SEX)) + geom_line() + geom_point(size = 4) + ggtitle("Mean Volume per Class")

ggplot(data = meanShuck , aes(x = CLASS, y = SHUCK, group = SEX, colour = SEX)) + geom_line() + geom_point(size = 4) + ggtitle("Mean Shuck per Class"),
