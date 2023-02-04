library(tidyverse)
library(mgcv)
library(refund)

## load data
load("NHANES.RData")

## Friday has the highest correlation with Age
sat_data = Act_Analysis %>% dplyr::filter(Act_Analysis$WEEKDAY==5)
#### activity data
sat_P = as.matrix(sat_data[,paste0("MIN", 1:1440)])
sat_P[is.na(sat_P)] = 0
#### down sample to every 20 min average count data
sat_P = array(sat_P, dim=c(nrow(sat_P), 20, 1440 / 20))
sat_P = apply(sat_P, c(1, 3), mean)
#### target variables
# sat_y = c(scale(sat_data$Age))
sat_y = sat_data$Age


## 5-fold Cross Validation ----
idx = c(rep(1, 543), rep(2, 543), rep(3, 542), rep(4, 542), rep(5, 542))
cv.flr = c()
cv.gpdr = c()
for(it in 1:500){
  idx_it = idx[sample(nrow(sat_P))]
  cv_it_flr = c()
  cv_it_gpdr = c()
  for(k in 1:5){
    ## functional linear regression
    train_data = data.frame(Y=sat_y[idx_it != k])
    train_data$Ps = sat_P[idx_it != k,]
    fit.pc = pfr(Y~lf(Ps, k=10), data=train_data)
    
    test_data = data.frame(Y=sat_y[idx_it == k])
    test_data$Ps = sat_P[idx_it == k,]
    resid.pc = test_data$Y - predict(fit.pc, test_data)
    cv_it_flr[k] = mean(resid.pc^2) / var(test_data$Y)
    
    ## GPDR (method is invariant to monotonic transformation)
    train_data$Ps = log(pmax(train_data$Ps, 0.025))
    test_data$Ps = log(pmax(test_data$Ps, 0.025))
    train_data$W = matrix(1/ncol(sat_P), sum(idx_it != k), ncol(sat_P))
    test_data$W = matrix(1/ncol(sat_P), sum(idx_it == k), ncol(sat_P))
    
    fit.gam = mgcv::gam(Y~s(Ps, by=W, bs="gp", m=4, k=10), data=train_data)
    resid.gam = test_data$Y - predict(fit.gam, test_data)
    cv_it_gpdr[k] = mean(resid.gam^2) / var(test_data$Y)
  }
  print(c(it, mean(cv_it_flr), mean(cv_it_gpdr)))
  cv.flr = rbind(cv.flr, cv_it_flr)
  cv.gpdr = rbind(cv.gpdr, cv_it_gpdr)
}

save(cv.flr, cv.gpdr, file="AgeExperimentResults.RData")

load("AgeExperimentResults.RData")

mean(1 - rowMeans(cv.flr)); quantile(1 - rowMeans(cv.flr), c(0.025, 0.975))

mean(1 - rowMeans(cv.gpdr)); quantile(1 - rowMeans(cv.gpdr), c(0.025, 0.975))
