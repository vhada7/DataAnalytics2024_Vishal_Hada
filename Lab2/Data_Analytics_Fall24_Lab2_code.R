####### Data Analytics Fall 2024 Lab 01 ######

library(ggplot2)

### set working directory
setwd("~/Courses/Data Analytics/Fall24/labs/lab01/")

### read in data
epi.results <- read.csv("epi2024results06022024.csv", header=TRUE)
epi.weights <- read.csv("epi2024weights.csv")

View(epi.results)
View(epi.weights)

#### Exploratory Analysis ####

epi.results$ECO.new

epi.results[1,5]

attach(epi.results)

ECO.new

ECO.new[1]

## NA values
na.indices <- is.na(ECO.new) 

## drop NAs
Eco.new.compl <- ECO.new[!na.indices]

## convert to data frame and add country
Eco.new.compl <- data.frame(Country = country[!na.indices], EPI = ECO.new[!na.indices])

## summary stats
summary(ECO.new)

fivenum(ECO.new,na.rm=TRUE)

## histograms
hist(EPI.new)

hist(EPI.new, seq(20., 80., 2.0), prob=TRUE)

rug(EPI.new)

lines(density(EPI.new,na.rm=TRUE,bw=1))
lines(density(EPI.new,na.rm=TRUE,bw="SJ"))

x <- seq(20., 80., 1.0)
qn<- dnorm(x,mean=42, sd=5,log=FALSE)
lines(x,0.4*qn)

qn<- dnorm(x,mean=65, sd=5,log=FALSE)
lines(x,0.12*qn)

##################

### Comparing distributions of 2 variables

boxplot(ECO.old, ECO.new, names=c("ECO.old","ECO.new"))

### 3 variables
boxplot(ECO.old, ECO.new, PAR.new, names=c("ECO.old", "ECO.new", "PAR.new"))


### Quantile-quantile plots

qqnorm(ECO.new)
qqline(ECO.new)

x <- seq(20., 80., 1.0)
qqplot(qnorm(ppoints(200)), x)
qqline(x)

qqplot(qnorm(ppoints(200)),ECO.new)
qqline(ECO.new)

qqplot(rnorm(1000),ECO.new)
qqline(ECO.new)

qqplot(rnorm(1000),ECO.new)
qqline(PAR.new)

qqplot(rnorm(1000),ECO.new)
qqline(ECO.old)

d1 <- rnorm(10000)
d2 <- rnorm(10000)
qqplot(d1,d1)
qqline(d1)


### Empirical Cumulative Distribution Function
plot(ecdf(EPI.new), do.points=FALSE) 

plot(ecdf(rnorm(1000, 45, 10)), do.points=FALSE, main="Norm Dist vs. ECO.new ECDF")
lines(ecdf(ECO.new))

plot(ecdf(ECO.old), do.points=FALSE, main="ECO.old vs. PAR.old ECDF")
lines(ecdf(PAR.old))

plot(ecdf(ECO.new), do.points=FALSE, main="ECO.new vs. PAR.new ECDF")
lines(ecdf(PAR.new))


#### Populations Dataset ####

## read data
populations_2023 <- read.csv("C:/Users/hadav/Downloads/countries_populations_2023.csv")

## drop country populations that don't exist in epi results
populations <- populations_2023[-which(!populations_2023$Country %in% EPI_data$country),]

## sort populations by country name
populations <- populations[order(populations$Country),]

## drop country results that don't exist in populations
EPI_data.sub <- EPI_data[-which(!EPI_data$country %in% populations$Country),]

## sort results by country name
EPI_data.sub <- EPI_data.sub[order(EPI_data.sub$country),]

## only keep relevant columns
EPI_data.sub <- EPI_data.sub[,c("country","ECO.old","ECO.new")]

## convert to numeric
EPI_data.sub$population <- as.numeric(populations$Population)

## compute population log
EPI_data.sub$population_log <- log10(EPI_data.sub$population)

boxplot(EPI_data.sub$population_log)

attach(EPI_data.sub)

## created linear model of EPI.new = a(population_log) + b
lin.mod.epinew <- lm(EPI.new~population_log,EPI_data.sub)

plot(EPI.new~population_log)
abline(lin.mod.epinew)

summary(lin.mod.epinew)

plot(lin.mod.epinew)

library(tidyverse)

ggplot(EPI_data.sub, aes(x = population_log, y = EPI.new)) +
  geom_point() +
  stat_smooth(method = "lm")

ggplot(lin.mod.epinew, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot', x='Fitted Values', y='Residuals')


## another lm
lin.mod.pop <- lm(population_log~EPI.new,EPI_data.sub)
plot(population_log~EPI.old)
abline(lin.mod.pop)

summary(lin.mod.pop)

plot(lin.mod.pop)


ggplot(EPI_data.sub, aes(x = EPI.old, y = population_log)) +
  geom_point() +
  stat_smooth(method = "lm")

ggplot(lin.mod.pop, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot', x='Fitted Values', y='Residuals')



