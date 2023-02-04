library(tidyverse)
source("GPDR_Matern.R")

rdp = function(n, alpha){
  # sample 1 measure mu from DP(unif[0,1], alpha)
  # then sample n values from mu
  # return mu and n samples
  probs = c(); bk = 1
  while(sum(probs) < 1-1e-5){
    sk = rbeta(1, 1, alpha)  
    probs = c(probs, sk * bk)
    bk = bk * (1-sk)
  }
  probs = probs / sum(probs)
  locs = runif(length(probs))
  samples = sample(locs, n, replace=TRUE, prob=probs)
  list(samples=samples, mu=list(locs, probs))
}


generate_data = function(beta, n, m, sig=0.1, alpha=25){
  X = list(); Y = c(); kde = list()
  for(i in 1:n){
    dpi = rdp(m, alpha)
    X[[i]] = dpi$samples
    Y[i] = sum(beta(dpi$mu[[1]]) * dpi$mu[[2]]) + rnorm(1, sd=sig)
    kde[[i]] = density(X[[i]], from=0, to=1, n=512)
  }
  list(X=X, y=Y, kde=kde)
}

beta = function(xvec){
  out = c()
  for(x in xvec){
    if(x < 1/3) out = c(out, 3 / 2 * x^2)
    else if(x < 2/3) out = c(out, 1/6 + x - 3 * (x-1/3)^2 - 1/3)
    else out = c(out, -x + 3/2 * (x-2/3)^2 + 5/6)
  }
  out
}

beta1 = function(xvec){
  10 * xvec * exp(-5 * xvec)
}

# fit model with kde method
kde_fit = function(data, Kxx, Kxc, Kcc, sig, newx=seq(0, 1, 0.01)){
  n = length(data$y)
  TKf = matrix(NA, length(newx), n); fTKf = matrix(NA, n, n)
  for(i in 1:n){
    TKf[, i] = Kxc %*% (data$kde[[i]]$y / sum(data$kde[[i]]$y))
    for(j in i:n){
      fKf = data$kde[[i]]$y / sum(data$kde[[i]]$y) * c(Kxx %*% data$kde[[j]]$y / sum(data$kde[[j]]$y))
      fTKf[i, j] = sum(fKf)
      fTKf[j, i] = fTKf[i, j]
    }
  }
  M = solve(fTKf + sig^2*diag(n))
  list(
    E=c(TKf %*% M %*% data$y), 
    COV=Kcc - TKf %*% M %*% t(TKf)
  )
}

## A test, skip it

# exp_l2 = c()
# kde_l2 = c()
#
# for(i in 1:100){
# 
# data = generate_data(beta1, 400, 250)
# 
# system.time(
# fit0 <- GPfit(data$X, data$y, nu=Inf, l=0.25, k=10, sigma=0.1, verbose=FALSE)
# )
# 
# plot(fit0, scale=1.96)
# lines(seq(0,1,0.01), beta1(seq(0,1,0.01)), col="red")
# 
# 
# system.time(
# fit1 <- kde_fit(data, MK(nu=Inf, sigma=1, l=0.25), sig=0.1, newx=seq(0, 1, 0.01))
# )
# lines(seq(0,1,0.01), fit1$E, col="green")
# 
# exp_l2[i] = sum((beta1(seq(0,1,0.01)) - predict(fit0, seq(0,1,0.01), mean.only=TRUE)$E)^2)
# kde_l2[i] = sum((beta1(seq(0,1,0.01)) - fit1$E)^2)
# 
# cat("EXP: ", exp_l2[i], "  KDE: ", kde_l2[i], "\n")
# 
# }
# 
# wilcox.test(kde_l2, exp_l2)
# t.test(kde_l2, exp_l2)
# 
# ggplot(
#   data.frame(L2=c(exp_l2, kde_l2), method=c(rep("EXP", 100), rep("KDE", 100))),
#   aes(x=L2, color=method, fill=method)
# ) + geom_density(alpha=0.3) + theme_bw()


#################################################################################
## Simulation

data = generate_data(beta1, 100, 100)
K = MK(nu=Inf, sigma=1, l=0.25)
## x grid and final evaluate grid are always the same
## for all m and n, therefore we only need to calculate once
Kxx = K(data$kde[[1]]$x, data$kde[[1]]$x)
Kxc = K(seq(0, 1, 0.01), data$kde[[1]]$x)
Kcc = K(seq(0, 1, 0.01), seq(0, 1, 0.01))

set.seed(54321)

results = c()
for(n in c(100, 200, 300, 400)){
# for(n in c(50)){
  for(m in c(50, 100, 250, 500, 1000, 2000)){
  # for(m in c(50, 100, 250, 500, 1000, 2000)){
    print(c(n, m))
    for(it in 1:100){
      cat(it, " ")
      k = 10
      
      data = generate_data(beta1, n, m)
      fit0 = GPfit(data$X, data$y, nu=Inf, l=0.25, k=k, sigma=0.1, verbose=FALSE)
      fit1 = kde_fit(data, Kxx, Kxc, Kcc, sig=0.1, newx=seq(0, 1, 0.01))
      
      exp_l2 = sum((beta1(seq(0,1,0.01)) - predict(fit0, seq(0,1,0.01), mean.only=TRUE)$E)^2)
      kde_l2 = sum((beta1(seq(0,1,0.01)) - fit1$E)^2)
      results = rbind(results, c(n, m, it, exp_l2, kde_l2))
      
      rm(data, fit0, fit1)
      gc()
    }
    cat("\n")
    write.table(results, file="simulation_results", row.names=FALSE)
  }
}

simRes = results
save(simRes, file="simulation_results.RData")