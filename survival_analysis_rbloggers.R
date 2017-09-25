rm(list = ls())


#loading libraries
library(survival)
library(dplyr)
library(OIsurv)
library(ranger)
library(ggplot2)



#loading datasets
data(bmt)
str(bmt)
bmt2 <- select(bmt, -c(t2,d2,d3))
bmt2  <- mutate(bmt2,
                group = as.factor(group),
                d1 = as.factor(d1),
                da = as.factor(da),
                dc = as.factor(dc),
                dp = as.factor(dp),
                z3 = as.factor(z3),
                z4 = as.factor(z4),
                z5 = as.factor(z5),
                z6 = as.factor(z6),
                z8 = as.factor(z8),
                z9 = as.factor(z9),
                z10 = as.factor(z10)
)


#survival object
y_bmt <- Surv(bmt2$t1, bmt$d1)
y_bmt
fit_bmt <- survfit(y_bmt ~ 1)
plot(fit_bmt)


#cox regression
form <- formula(y_bmt ~ group + ta + tc + tp + dc + dp + 
                  z1 + z2 + z3 + z4 + z5 + z6 + z7 + z8 + z10)
bmt_cox <- coxph(form, data = bmt2)
bmt_cox                                                                                                          
bmt_cox_fit <- survfit(bmt_cox)
plot(bmt_cox_fit)


#ranger model
r_bmt_fit <- ranger(form, data = bmt2, importance = "permutation", seed = 1)
death_times <- r_bmt_fit$unique.death.times
death_times
surv_prob <- data.frame(r_bmt_fit$survival)
avg_prob <- sapply(surv_prob, mean)
avg_prob_df <- data.frame(avg_prob)

vi <- data.frame(sort(round(r_bmt_fit$variable.importance, 4), decreasing = TRUE))
names(vi) <- "importance"
head(vi)

#http://daynebatten.com/2015/02/customer-churn-survival-analysis/ - :D
#http://daynebatten.com/2015/02/customer-churn-cox-regression/ - :D