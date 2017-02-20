# number of observations to simulate
n = 200
set.seed(101) 
ID <- 1:200
happy <- runif(n, 0, 5)
SES <- rnorm(n, 0, 1)
friends  <- rnorm(n, 6, 2)
mood.group <- rbinom(n,2,.5)


b0 <- 4
b1 <- 0.35
b2 <- 0.5
b3 <- .5
b4 <- .9
sigma <- 2.5

error <- rnorm(n,0,sigma)

health <- b0 + b1*happy  + b2*SES  + b3*friends + b4*mood.group + error

th.project.3 <- data.frame(ID, happy, SES, friends, mood.group,health)
simdata$mood.group<- as.factor(simdata$mood.group)

lm.1 <- lm(health~happy+SES+friends+mood.group, simdata)
summary(lm.1)

plot(lm.1)

cor(th.project.3)


library(psych)
describe(simdata$happy)
describe(simdata$health)
cor(simdata)

write.csv(th.project.3, file = "th.project.3.csv")
