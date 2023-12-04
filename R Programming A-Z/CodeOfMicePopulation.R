
a<-read.csv("femaleControlsPopulation.csv")

b<-read.csv("femaleMiceWeights.csv")
c<-read.csv("mice_pheno.csv")

b$Bodyweight[11]
  length(c)
length(b)  

v<-b$Diet=="hf"
fill<-b[v,]
mean(fill$Bodyweight)
set.seed(1)

sample_result <- sample(13:24, size = 1)
selected_weight <- b$Bodyweight[sample_result]

selected_weight
?sample

library(dplyr)
install.packages("dplyr")

control<-filter(b,b$Diet=="chow") %>% select(Bodyweight) %>% unlist





sleep<-read.csv("msleep_ggplot2.csv") 
count<-filter(sleep,order=="Primates") %>% reframe(sleep_total) 
mean(count)

dat<-load("skew.RData")
matric<-dim(dat)


load("skew.RData")

# Check the dimensions of the dataset
dim(dat)

# Set up a 3x3 grid for QQ-plots
par(mfrow = c(3, 3))

# Loop through columns and create QQ-plots
for (i in 1:9) {
  qqnorm(dat[, i], main = paste("Column", i))
}

# Reset the graph layout
par(mfrow = c(1, 1))




# Reset the graph layout
par(mfrow = c(1, 1))






hist(exec.pay)


head(InsectSprays)
a<-InsectSprays
class(a)
boxplot(split(a, factor))

boxplot(split(InsectSprays$count, InsectSprays$spray),colour="blue")
boxplot(count ~ spray,data = InsectSprays)



install.packages("UsingR")

library(dplyr)
data(nym.2002, package="UsingR")

male<-filter(nym.2002,gender=="Male") 
Female<-filter(nym.2002,gender=="Female")


boxplot(time~gender,data=nym.2002)



males <- nym.2002$time[nym.2002$gender == "Male"]
females <- nym.2002$time[nym.2002$gender == "Female"]

# Create a new plot window
par(mfrow = c(1, 2))

# Create histogram for males
hist(males, main = "Histogram of Finishing Times (Males)", xlab = "Time", ylab = "Frequency", col = "lightblue", breaks = 20)



qqnorm(males, main = "QQ-Plot for Finishing Times (Males)")
qqline(males, col = "red")

# Create QQ-plot for females
qqnorm(females, main = "QQ-Plot for Finishing Times (Females)")
qqline(females, col = "pink")
# Create histogram for females
hist(females, main = "Histogram of Finishing Times (Females)", xlab = "Time", ylab = "Frequency", col = "pink", breaks = 20)

# Reset the plot window
par(mfrow = c(1,1))


par(1,3)
males <- filter(nym.2002, gender=="Male") %>% select(time) %>% unlist
females <- filter(nym.2002, gender=="Female") %>% select(time) %>% unlist
boxplot(females, males)
hist(females,xlim=c(range( nym.2002$time)))
hist(males,xlim=c(range( nym.2002$time)))




library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename <- basename(url)
download(url, destfile=filename)
x <- unlist( read.csv(filename) )

RNGkind("Mersenne-Twister", "Inversion", "Rejection")

ac<-mean(x)
set.seed(1)
sampl <- sample(x, size = 5)
a<-abs(sampl)
mean(a)
22.86-23.89338


sample_size <- 5
sample <- sample(x, size = sample_size)
population_mean <- mean(x)
# Calculate the absolute value of the difference between the sample mean and the population mean
sample_mean <- mean(sample)
abs_difference <- abs(sample_mean - population_mean)
abs_difference

population_mean <- mean(x)
set.seed(1)

# Take a random sample of size 5
sample_size <- 5
sample <- sample(x, size = sample_size)

# Calculate the absolute value of the difference between the sample mean and the population mean
sample_mean <- mean(sample)
abs_difference <- abs(sample_mean - population_mean)


R.Version()

numbers <- c(2.23, 3.45, 1.87, 2.11, 7.33, 18.34, 19.23)

# Calculate the average
average <- mean(numbers)

# Print the average
cat("Average:", average, "\n")

cars
mean(cars[,2])
which(cars$dist==85)



population<-read.csv("femaleControlsPopulation.csv")

population<-unlist(population)

n<-10000
nulls<-vector("numeric",n)
for (i in 1:n) {
  control<-sample(population,5)
  #treatment<-sample(population,5)
  nulls[i]<-mean(control)
  
}
obs<-mean(treatment)-mean(control)





install.packages("gapminder")

library(gapminder)
data(gapminder)
head(gapminder)

prop = function(q) {
  mean(x <= q)
}

prop(40)


library(dplyr)

x<-filter(gapminder, year ==1952) %>% select(lifeExp) %>% unlist
hist(x)
mean(abs(nulls)>ac)

hist(nulls)
set.seed(1)
proportion_away <- mean(abs(nulls - ac) > 1)
proportion_away

library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename <- basename(url)
download(url, destfile=filename)
x <- unlist( read.csv(filename) )

set.seed(1)
n <- 1000
averages5 <- vector("numeric",n)
for(i in 1:n){
  X <- sample(x,5)
  averages5[i] <- mean(X)
}
par(mfrow = c(1,1))
hist(averages50)
# make averages50
set.seed(1)
n <- 1000
averages50 <- vector("numeric",n)
for(i in 1:n){
  X <- sample(x,25)
  averages50[i] <- mean(X)
}
mean(X)
rem
proportion_between_23_25 <- mean(averages50 >= 23 & averages50 <= 25)

proportion_between_23_25

mean(averages50)
pnorm( (23-23.9)/0.43 )

mu <- 23.9
sigma <- 0.43
lower_cutoff <- 23
upper_cutoff <- 25

# Calculate the proportions using pnorm()
proportion_lower <- pnorm(lower_cutoff, mu, sigma)
proportion_upper <- pnorm(upper_cutoff, mu, sigma)

# Calculate the proportion between 23 and 25
proportion_between <- proportion_upper - proportion_lower

# Print the proportion
print(proportion_between)



##############################################################

  library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/mice_pheno.csv"
filename <- basename(url)
download(url, destfile=filename)
dat <- read.csv(filename) 
head(dat)

dat<-na.omit(dat)
y<-filter(dat,Sex=="M"& Diet=="chow") %>% select(Bodyweight) %>% unlist()
mean_y<-mean(y)



download(rafalib)
library(rafalib)

install.packages("rafalib")
library(rafalib)
population_sd <- popsd(y)

population_sd


# 
# set.seed(2)
# sample_size <- 25
# random_sample <- sample(y, sample_size)
# sample_avg_y <- mean(random_sample)
# 
# # Print the sample average
# print(sample_avg_y)


mean_y-mean_x

sample_avg_y-sample_avg_x
2.375517-2.0108

upper_bound_2sd <- mean_y + population_sd
lower_bound_2sd <- mean_y - population_sd

# Calculate the proportion within two standard deviations
proportion_within_2sd <- pnorm(upper_bound_2sd, mean = mean_y, sd = population_sd) - pnorm(lower_bound_2sd, mean = mean_y, sd = population_sd)

# Print the proportion
print(proportion_within_2sd)


# Answerrrrrrrrrrrrrrrrr
#What proportion of the mice are within one standard deviation away from the average weight?
# Remember to use popsd() from rafalib for the population standard deviation.
z <- ( y - mean(y) ) / popsd(y)
mean( abs(z) <=3 )

par(mfrow = c(1,2))

y <- filter(dat, Sex=="M" & Diet=="chow") %>% select(Bodyweight) %>% unlist
set.seed(1)
avgs <- replicate(10000, mean( sample(y, 25)))

average_of_sample_averages <- mean(avgs)
print(average_of_sample_averages)

standard_deviation_of_sample_averages <- popsd(avgs)
standard_deviation_of_sample_averages

hist(avgs)
qqnorm(avgs)
qqline(avgs)



