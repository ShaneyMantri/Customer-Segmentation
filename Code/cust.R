library(tidyverse)
library(plotrix)
library(reshape2)
library(Hmisc)


customer_data = read.csv("Mall_Customers.csv")
str(customer_data)
View(customer_data)
names(customer_data)

head(customer_data)
summary(customer_data)

##Findinf Standar Deviations


summary(customer_data$Age)
sd(customer_data$Age)

summary(customer_data$Annual.Income..k..)
sd(customer_data$Annual.Income..k..)

summary(customer_data$Spending.Score..1.100.)
sd(customer_data$Spending.Score..1.100.)

#Gender Visualization
gender_visualization = customer_data %>%
    group_by(Gender) %>%
    summarise(Total = n())




#Bar Plot
ggplot(gender_visualization, aes(Gender, Total, fill = Gender))+
    geom_bar(stat = "identity")



#Pie Chart
pct=round(gender_visualization$Total/sum(gender_visualization$Total)*100)
lbs=paste(c("Female","Male")," ",pct,"%",sep=" ")
library(plotrix)
pie3D(gender_visualization
      $Total,labels=lbs,
      main="Pie Chart Depicting Ratio of Female and Male")


#Histogram for Age

summary(customer_data$Age)

ggplot(customer_data, aes(Age))+
    geom_histogram(fill="salmon", col = "black", labels =TRUE)+
    scale_x_continuous(breaks = seq(18, 70, 3))


hist(customer_data$Age,
     col="blue",
     main="Histogram to Show Count of Age Class",
     xlab="Age Class",
     ylab="Frequency",
     labels=TRUE)


#Box plot of Age

boxplot(customer_data$Age,col="salmon",main="Boxplot for Descriptive Analysis of Age")


#Analysis of Annual Income

summary(customer_data$Annual.Income..k..)

hist(customer_data$Annual.Income..k..,
     col="salmon",
     main="Histogram for Annual Income",
     xlab="Annual Income Class",
     ylab="Frequency",
     labels=TRUE)


#Density curve

ggplot(customer_data,aes(Annual.Income..k..))+
    geom_density(fill="navyblue")




#Analysisng spending score

summary(customer_data$Spending.Score..1.100.)

boxplot(customer_data$Spending.Score..1.100., col="#99FF00", main="BoxPlot for Descriptive Analysis of Spending Score")


hist(customer_data$Spending.Score..1.100.,
     main="HistoGram for Spending Score",
     xlab="Spending Score Class",
     ylab="Frequency",
     col="salmon",
     labels=TRUE)









































