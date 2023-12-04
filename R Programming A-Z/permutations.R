url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/babies.txt"
filename <- basename(url)
download.file(url, destfile = filename)
babies <- read.table("babies.txt", header=TRUE)
head(babies)
bwt.nonsmoke <- filter(babies, smoke==0) %>% select(bwt)%>% sample_n(5) %>% unlist 
bwt.smoke <- filter(babies, smoke==1) %>% sample_n(5)%>% select(bwt) %>% unlist


library(rafalib)
mean(bwt.nonsmoke)-mean(bwt.smoke)
popsd(bwt.nonsmoke)
popsd(bwt.smoke)
set.seed(1)
t_test_result <- t.test(bwt.nonsmoke, bwt.smoke)
abs_tval <- abs(t_test_result$statistic)
p_value <- t_test_result$p.value

print(p_value)
print(abs_tval)

library(dplyr)
t_test_result
# Set seed to 1
set.seed(1)
N<-25
freedom<-2*N-2

pval <- freedom-(pnorm(abs(abs_tval))-pnorm(-abs(abs_tval)))
pval





library(dplyr)

# Set seed to 1
set.seed(1)

N <- 5  # Sample size
alpha <- 0.05  # Significance level
num_simulations <- 1000 # Number of simulations

# Function to perform t-test and check if null hypothesis is rejected
perform_t_test <- function() {
  dat.ns <- filter(babies, smoke == 0) %>% sample_n(N) %>% select(bwt) %>% unlist
  dat.s <- filter(babies, smoke == 1) %>% sample_n(N) %>% select(bwt) %>% unlist
  t_test_result <- t.test(dat.ns, dat.s)
  return(t_test_result$p.value < alpha)
}

# Perform simulations and calculate proportion of rejections
rejection_proportion <- mean(replicate(num_simulations, perform_t_test()))

print(rejection_proportion)
perform_t_test
calculate_power <- function(sample_size) {
  perform_t_test <- function() {
    dat.ns <- filter(babies, smoke == 0) %>% sample_n(sample_size) %>% select(bwt) %>% unlist
    dat.s <- filter(babies, smoke == 1) %>% sample_n(sample_size) %>% select(bwt) %>% unlist
    t_test_result <- t.test(dat.ns, dat.s)
    return(t_test_result$p.value < alpha)
  }
  rejection_proportion <- mean(replicate(num_simulations, perform_t_test()))
  return(rejection_proportion)
}

# Calculate power for different sample sizes
sample_sizes <- c(30, 60, 90, 120)
powers <- sapply(sample_sizes, calculate_power)

print(powers)

Ns=c(30,60,90,120)
res <- sapply(Ns, function(N){
  set.seed(1)
  rejects <- replicate(10000,{
    dat.ns <- sample(dat.ns , N)
    dat.s <- sample(dat.s , N)
    t.test(dat.s, dat.ns)$p.value < 0.01
  })
  mean(rejects)
})
Ns[ which.min( abs( res - .8) ) ] 



install.packages("swirl")
library(swirl)
swirl()


install.packages('ggplot2',dependencies=T)

library(ggplot2)

install.packages("devtools",dependencies=T)
library(devtools)
install_version("colorspace","1.2-4")







