rm(list = ls())


#loading libraries
library(survival)


#loading datasets
train_surv <- read.csv("E:/ser_docs_manish/manish R scripts/NetLixx.csv")
train_surv$start <- NULL
train_surv2 <- read.csv("E:/ser_docs_manish/manish R scripts/NetLixxCox.csv")
train_surv2$start <- NULL
train_surv2$time <- NULL
str(train)

#creating survival object
train_surv$survival <- Surv(train_surv$time, train_surv$churned == 1)
str(train_surv)


#fitting survival curve (kaplan-meier estimator)

fit <- survfit(survival ~ 1, data = train_surv)
summary(fit)
plot(fit, lty = 1, mark.time = FALSE, ylim=c(.75,1), xlab = 'Days since Subscribing', ylab = 'Percent Surviving')


#trends for different subset of population(male and female)
fit1 <- survfit(survival ~ female, data = train_surv)
plot(fit1, lty = 1:2, mark.time = FALSE, ylim=c(.75,1), xlab = 'Days since Subscribing', ylab = 'Percent Surviving')
legend(20, .8, c('Male', 'Female'), lty=1:2, bty = 'n', ncol = 2)
survdiff(survival ~ female, data = train_surv)



####################################################################

#Cox regression
###############

train_surv2$survival <- Surv(train_surv2$followtime, train_surv2$churn == 1)


#cox regression
fit_cox <- coxph(survival ~ female + age + coupon, data = train_surv2)
fit_cox 
#1) According to these results, coupon users churn 1.8 times faster (or 80% 
#faster) than the baseline survival rate. coupon users churn 1.8 times 
#faster than non-coupon users.

#2) The age results are a bit more complicated. The coefficient means that a 
#1-year increase in customer age multiplies the hazard rate by .994, 
#or 99.4%. So, we get a slight reduction in churn for every additional year
#of a customer's age. Not bad! Let's target those old guys!


#validating model for non-proportional hazards
cox.zph(fit_cox)
#And, as we expected, we've got some pretty heavy-duty significance on that
#coupon variable, which is also invalidating the assumptions for the global
#model. If we want to make sure we have useful statistics, we're going to 
#have to remove the coupon variable from the model.


