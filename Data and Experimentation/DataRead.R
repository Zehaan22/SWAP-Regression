library(dplyr)
library(readxl)
dat2 <- read_xlsx("50 Macroeconomic Indicators.xlsx", sheet = 3)

max_corr <- 0
my_i = 0
my_j = 0
for (i in 5:21){
  for (j in (i+1):22){
    my.dat <- dat2[,c(i,j)] 
    names <- my.dat[2,]
    my.dat <- my.dat[-(1:4),]
    colnames(my.dat) <- names
    my.dat <- apply(my.dat, 2, as.numeric)
    
    my.corr <- abs(cor(my.dat)[1,2])
    if (my.corr > max_corr){
      max_corr = my.corr
      my_j = j
      my_i = i
    } 
    
  }
}
my.dat <- dat2[,c(5,19)]
names <- my.dat[2,]
my.dat <- my.dat[-(1:4),]
colnames(my.dat) <- names
my.dat <- apply(my.dat, 2, as.numeric)

my.dat <- as_tibble(my.dat)
my.dat <- my.dat%>%
  filter(!is.na(`Total Retail Payments 
(₹ Crore)`))

Y <- my.dat$`Consumer Price Index for Agricultural Labourer`
X <- cbind(1,my.dat$`Total Retail Payments 
(₹ Crore)`)
plot(my.dat)
beta_hat <- solve(t(X)%*%X)%*%t(X)%*%Y
lines( X%*%beta_hat,X[,2])
cor(my.dat)
