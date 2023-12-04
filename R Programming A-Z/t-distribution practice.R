library(downloader)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleMiceWeights.csv"
filename <- "femaleMiceWeights.csv"
if(!file.exists("femaleMiceWeights.csv")) download(url,destfile=filename)
dat <- read.csv(filename)





install.packages("rafalib")
library(rafalib)

# Set the seed
set.seed(1)

# Simulate rolling dice 10,000 times and calculating proportions
n <- 100
p <- 0.01
proportions <- replicate(10000, mean(sample(1:6, n, replace = TRUE) == 6))

# Calculate standardized variable z
z <- (proportions - p) / sqrt(p * (1 - p) / n)

# Calculate the proportion of times |z| > 2
proportion_larger_than_2 <- mean(abs(z) > 2)
print(proportion_larger_than_2)




ps <- c(0.5,0.5,0.01,0.01)
ns <- c(5,30,30,100)
library(rafalib)
mypar(4,2)
for(i in 1:4){
  p <- ps[i]
  sides <- 1/p
  n <- ns[i]
  zs <- replicate(10000,{
    x <- sample(1:sides,n,replace=TRUE)
    (mean(x==1) - p) / sqrt(p*(1-p)/n)
  }) 
  hist(zs,nclass=7)
  qqnorm(zs)
  abline(0,1)
}
1.469867-0.02189533

X <- filter(dat, Diet == "chow") %>% select(Bodyweight) %>% unlist()

x<-2 * ( 1-pnorm(2/sd(X) * sqrt(12) ) )

SE_estimate <- sqrt((sigma_Y^2 / n_Y) + (sigma_X^2 / n_X))

print(SE_estimate)

sqrt( sd(X)^2/12 + sd(Y)^2/12 )
t.test()
t_observed <- 1.469867
p_value <- 2 * pt(abs(t_observed), df = df, lower.tail = FALSE)






