# Loading data into data frames

adult <- read.csv('adult_sal.csv')
adult <- as.data.frame(adult)

library(dplyr)
library(ggplot2)
library(Amelia)

adult <- select(adult,-X)

# found a lot of these columns have too many factors than necessary 
# Data Cleaning 

# 1) type of employer column

table(adult$type_employer)

unemp <- function(x){
  x <- as.character(x)
    if (x == 'Never-worked' | x == 'Without-pay'){
      return('Unemployed')
    }else{
      return(x)
    }
}

adult$type_employer <- sapply(adult$type_employer,unemp)

table(adult$type_employer)

gov <- function(x){
  x <- as.character(x)
  if (x == 'Local-gov' | x == 'State-gov'){
    return('SL-gov')
  }else{
    return(x)
  }
}

adult$type_employer <- sapply(adult$type_employer,gov)
table(adult$type_employer)

self <- function(x){
  x <- as.character(x)
  if (x == 'Self-emp-inc' | x == 'Self-emp-not-inc'){
    return('self-emp')
  }else{
    return(x)
  }
}

adult$type_employer <- sapply(adult$type_employer,self)
table(adult$type_employer)

# 2) Marital Column

table(adult$marital)

# reduced to three groups:
# Married : Married-civ, AF-spouse, absent
# Not-married : Divorced, widowed, separated
# Never-married

marry <- function(x){
  x <- as.character(x)
  if (x == 'Married-AF-spouse' | x == 'Married-civ-spouse' | x == 'Married-spouse-absent'){
  return('Married')    
  }else if (x == 'Divorced' | x == 'Widowed' | x == 'Separated'){
    return('Not-married')
  }else{
    return(x)
  }
}

adult$marital <- sapply(adult$marital,marry)
table(adult$marital)

# 3) Country Column
# group by continents

country <- function(x){
  x <- as.character(x)
  if (x == 'Cambodia' | x == 'China' | x == 'India' | 
      x == 'Laos' | x == 'Japan' | x == 'Philippines' |
      x == 'Taiwan' | x == 'Thailand' | x == 'Vietnam' |
      x == 'Hong' | x == 'South' | x == 'Iran'){
    return('Asia')
  }else if (x == 'England' | x == 'France' | x == 'Germany' |
            x == 'Greece' | x == 'Holand-Netherlands' |
            x == 'Hungary' | x == 'Ireland' | x == 'Italy' |
            x == 'Poland' | x == 'Portugal' | x == 'Scotland' |
            x == 'Yugoslavia'){
    return('Europe')
  }else if (x == 'Canada' | x == 'United-States'){
    return('North_America')
  }else if (x == 'Cuba' | x == 'Columbia' | x == 'Dominican-Republic' |
            x == 'Ecuador' | x == 'El-Salvador' | x == 'Guatemala' |
            x == 'Haiti' | x == 'Honduras' | x == 'Jamaica' |
            x == 'Mexico' | x == 'Nicaragua' | x == 'Peru' |
            x == 'Puerto-Rico' | x == 'Trinadad&Tobago'){
    return('South_America')
  }else{
    return(x)
  }
  
}
adult$country <- sapply(adult$country, country)
table(adult$country)

# re-factor
adult$type_employer <- sapply(adult$type_employer,factor)
adult$country <- sapply(adult$country,factor)
adult$marital <- sapply(adult$marital,factor)  
adult$occupation <- sapply(adult$occupation,factor)
adult$income <- sapply(adult$income,factor)

adult[adult == '?'] <- NA
# or using is.na


# Missing Data
# use Amelia

missmap(adult)
missmap(adult,y.at=c(1),y.labels = c(''),col=c('yellow','black'))

adult <- na.omit(adult)

# EDA

# Explore data using visualization 

str(adult)

ggplot(adult,aes(age)) + geom_histogram(aes(fill=income),
                                        color = 'black',binwidth = 1) + theme_bw()


ggplot(adult,aes(hr_per_week)) + geom_histogram()

colnames(adult)[14] <- 'region'
# or adult <- rename(adult,region = country) using dplyr

ggplot(adult,aes(region)) + geom_bar(aes(fill=income))


# Building a Logistic Regression Model

library(caTools)
set.seed(101)

# TRAIN TEST SPLIT

split = sample.split(adult,SplitRatio = 0.70)

final.train = subset(adult, split == TRUE)
final.test = subset(adult, split == FALSE)

final.log.model <- glm(formula = income ~., family = binomial(logit), data = final.train)
summary(final.log.model)

new.model <- step(final.log.model)
summary(new.model)

# Create a confusion matrix

fitted.probabilities <- predict(final.log.model,newdata=final.test,type='response')
fitted.results <- ifelse(fitted.probabilities > 0.5,1,0)

table(final.test$income,fitted.probabilities > 0.5)

# Accuracy

acc <- (7042+1488)/(7042+556+967+1488)
print(acc)

# acc = 0.845903418339664

#recall
rec <- 7042/(7042+556)
print(rec)

# rec = 0.972832369942197

# precision
pre <- 7042/(7042+967)
print(pre)

# pre = 0.929320817228051
