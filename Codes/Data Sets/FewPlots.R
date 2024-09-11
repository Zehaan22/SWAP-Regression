dat <- read.csv("insurance.csv")
summary(dat)
hist(dat$bmi)
plot(density(dat$bmi),  main = "Density Plot for BMI")

n = 1000
X = 12* runif(n) - 6

Y = 3*X**2 - 4*X + 5 + rnorm(n, sd = 10)
Y = Y/sum(Y)
plot(X,Y, type = 'p', xlab = "Portfolio Returns", ylab = "Portfolio Standard Deviation")


prob.val <- function(n){
  a <- choose(6,1)*(5**n) - choose(6,2)*(4**n) + choose(6,3)*(3**n) - choose(6,4)*(2**n) + choose(6,5)*(1**n)
  return (1- a/(6**n))
}

sum.coup = 0
for(i in 1:10){
  sum.coup = sum.coup + choose(10,i)*factorial(10-i)*i
}
print(sum.coup/(factorial(9)))