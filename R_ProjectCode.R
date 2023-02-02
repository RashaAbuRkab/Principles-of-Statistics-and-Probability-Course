# Read Data

data =read.csv("C:\\Users\\pc\\Downloads\\dataset_car_seats.csv")
data = data.frame(data)
head(data,10)

# Data Info

## 1) shape of data 

dim(data)  

## 2) Columns name

str(data) 

## 3) Summary Of Data

summary(data)  


# Create a sub-data

subdata= data[,c(2,3,4,9,10,11,12)]
head(subdata,10)

## The Columns Name 

str(subdata)

## Convert string Data To Numerical

subdata$Urban = as.factor(subdata$Urban)
subdata$US = as.factor(subdata$US)

## Plot Sub-Data

plot(subdata$Age)

## Box Plot Of Age

boxplot(subdata$Age,horizontal = TRUE)

# calculate mean and variance and stander deviation for Age

mean(subdata$Age)

var(subdata$Age)

sd(subdata$Age)

sum(subdata$Age)

sum((subdata$Age - mean(subdata$Age))^2)

## Save Result in A Table

restable = data.frame(matrix(NA,ncol=2,nrow = 5))   
restable[1,1] = 'Ages'
restable[2,1] = 'Mean'
restable[3,1] = 'Median'
restable[4,1] = 'SD'
restable[5,1] = 'Min,Max'

restable[2,2] = round(mean(subdata$Age),2)
restable[3,2] = median(subdata$Age)
restable[4,2] = round(sd(subdata$Age),2)
restable[5,2] = paste(min(subdata$Age),max(subdata$Age),sep = ',')

print(restable)

## Calculate The Range

RR= max(subdata$Age) - min(subdata$Age)
RR

## Calculate Z_Score
#Print The First 10 Result 

x= subdata$Age
xbar = mean(x)
sdd = sd(x)
z_score = (x - xbar)/sdd 
head(z_score,10)

## Calculate Percentiles
Create A Function And Print Data As A Table

mystat = function(x,nom){
  resTable = data.frame(matrix(NA,ncol=2,nrow = 5))
  resTable[1,1] = nom
  resTable[2,1] = 'Mean ± SD'
  resTable[3,1] = 'MD(Q1-Q3)'
  resTable[4,1] = 'Min,Max'
  
  resTable[2,2]= paste(mean(x),'±',sd(x),sep = '')
  resTable[3,2]= paste(median(x),'(',quantile(x,0.25),'-',quantile(x,0.75),')',sep = '')
  resTable[4,2]= paste(min(x),max(x),sep = ',')
  
  return(resTable)
}

mystat(x,'Age')


## skewness

library(moments)
skewness(subdata$Age)

plot(density(subdata$Age))


## T-Test

library(BSDA)
t.test(subdata$Age)
t.test(subdata$Age,subdata$Sales)
wilcox.test(subdata$Age , subdata$Sales)


# Calculate The Correlation And Regression
Find The Correlation For All Numerical Columns And Company Price 


ResTable = data.frame(matrix(NA,ncol=2,nrow = 5))
ResTable[1,1]= 'Columns'
ResTable[2,1] = "Age"
ResTable[3,1] = 'Sales'
ResTable[4,1] = 'Income'
ResTable[5,1] = 'Education'

ResTable[1,2]= 'Resule'
ResTable[2,2]= round(cor(subdata$CompPrice, subdata$Age, method = "pearson", use = "complete.obs"),2) 
ResTable[3,2]= round(cor(subdata$CompPrice, subdata$Sales, method = "pearson", use = "complete.obs"),2)
ResTable[4,2]= round(cor(subdata$CompPrice, subdata$Income, method = "pearson", use = "complete.obs"),2)
ResTable[5,2]= round(cor(subdata$CompPrice, subdata$Education, method = "pearson", use = "complete.obs"),2)

ResTable

# Correlation and Regression Analysis
##  Scatter plot with smooth fit curve With Positive Correlation
### 1) With Sales

library("ggpubr")
ggscatter(subdata, x = "Sales", y = "CompPrice", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Sales", ylab = "Company Price")


### 2) With Education
library("ggpubr")
ggscatter(subdata, x = "Education", y = "CompPrice", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Sales", ylab = "Company Price")