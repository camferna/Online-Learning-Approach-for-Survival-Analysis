###import libraries
library('openxlsx')
library('latex2exp')


#Fixed parameters

#number of iterations of ONS algorithm 
n_it <- 10^3
#dataset dimension
d <- 4


# ============= Estimation Error ============


### grid 1

#open results
beta_real_mean <- read.xlsx('grid1_beta_real.xlsx')
beta_boa_mean <- read.xlsx('grid1_beta_boa.xlsx')
beta_surv_mean <- read.xlsx('grid1_beta_surv.xlsx')
beta_ons_mean <- read.xlsx('grid1_beta_ons.xlsx')
beta_ogd_mean <- read.xlsx('grid1_beta_ogd.xlsx')


BETA_REAL_ARR = t(matrix(rep(as.numeric(unlist(beta_real_mean)),n_it), nrow = 4))


###cumulative average

beta_boa_moy <- matrix(0,n_it,d)
beta_surv_moy <- matrix(0,n_it,d)
beta_ons_moy <- matrix(0,n_it,d)
beta_ogd_moy <- matrix(0,n_it,d)

for (i in 1:d){
 beta_boa_moy[,i] <- cumsum(beta_boa_mean[,i])/(1:n_it)
 beta_surv_moy[,i] <- cumsum(beta_surv_mean[,i])/(1:n_it)
 beta_ons_moy[,i] <- cumsum(beta_ons_mean[,i])/(1:n_it)
 beta_ogd_moy[,i] <- cumsum(beta_ogd_mean[,i])/(1:n_it)
}



#graph
plot(apply((beta_boa_moy - BETA_REAL_ARR)^2, 1, sum), type='l', col =1,lty = 2, log = 'xy', xlab="Iterations",ylab="Average Quadratic Error",lwd =2, cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5,ylim = c(5e-06,1e-01))
lines(apply((beta_ons_moy - BETA_REAL_ARR)^2, 1, sum), type='l', col = 'blue', lwd = 2, lty = 4)
lines(apply((beta_ogd_moy - BETA_REAL_ARR)^2, 1, sum), type='l', col = 'darkgreen', lwd = 2, lty = 6)
lines(apply((beta_surv_moy - BETA_REAL_ARR)^2, 1, sum), type='l', col = 'red', lwd = 2, lty = 1)
legend("bottomleft", c("ONS","OGD","SurvONS","BOA-ONS"), col = c('blue','darkgreen','red',1), lty= c(4,6,1,2), lwd =2)





### grid 2

#open results
beta_real_mean <- read.xlsx('grid2_beta_real.xlsx')
beta_boa_mean <- read.xlsx('grid2_beta_boa.xlsx')
beta_surv_mean <- read.xlsx('grid2_beta_surv.xlsx')
beta_ons_mean <- read.xlsx('grid2_beta_ons.xlsx')
beta_ogd_mean <- read.xlsx('grid2_beta_ogd.xlsx')


BETA_REAL_ARR = t(matrix(rep(as.numeric(unlist(beta_real_mean)),n_it), nrow = 4))


#cumulative average

beta_boa_moy <- matrix(0,n_it,d)
beta_surv_moy <- matrix(0,n_it,d)
beta_ons_moy <- matrix(0,n_it,d)
beta_ogd_moy <- matrix(0,n_it,d)

for (i in 1:d){
  beta_boa_moy[,i] <- cumsum(beta_boa_mean[,i])/(1:n_it)
  beta_surv_moy[,i] <- cumsum(beta_surv_mean[,i])/(1:n_it)
  beta_ons_moy[,i] <- cumsum(beta_ons_mean[,i])/(1:n_it)
  beta_ogd_moy[,i] <- cumsum(beta_ogd_mean[,i])/(1:n_it)
}




#graph
plot(apply((beta_boa_moy - BETA_REAL_ARR)^2, 1, sum), type='l', col =1,lty = 2, log = 'xy', xlab="Iterations",ylab="Average Quadratic Error",lwd =2, cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5,ylim = c(5e-06,1e-01))
lines(apply((beta_ons_moy - BETA_REAL_ARR)^2, 1, sum), type='l', col = 'blue', lwd = 2, lty = 4)
lines(apply((beta_ogd_moy - BETA_REAL_ARR)^2, 1, sum), type='l', col = 'darkgreen', lwd = 2, lty = 6)
lines(apply((beta_surv_moy - BETA_REAL_ARR)^2, 1, sum), type='l', col = 'red', lwd = 2, lty = 1)
legend("bottomleft", c("ONS","OGD","SurvONS","BOA-ONS"), col = c('blue','darkgreen','red',1), lty= c(4,6,1,2), lwd =2)









# ================= Likelihood ==============


###grid1

#open results
like_real_mean <- unlist(read.xlsx('grid1_like_real.xlsx'))
like_boa_mean <- unlist(read.xlsx('grid1_like_boa.xlsx'))
like_surv_mean <- unlist(read.xlsx('grid1_like_surv.xlsx'))
like_ons_mean <- unlist(read.xlsx('grid1_like_ons.xlsx'))
like_ogd_mean <- unlist(read.xlsx('grid1_like_ogd.xlsx'))


#graph
plot(1:n_it,cumsum(abs(like_boa_mean-like_real_mean)), type = 'l', lty =2, lwd = 2, col = 1, xlab = 'Iterations', ylab = 'Negative Log-Likelihood', cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5, ylim = c(0,0.020))
lines(1:n_it, cumsum( abs(like_ogd_mean -like_real_mean)), lty = 6, lwd =2,col = 'darkgreen')
lines(1:n_it, cumsum( abs(like_ons_mean - like_real_mean)),lty =4, lwd =2, col ='blue')
lines(1:n_it,cumsum( abs(like_surv_mean - like_real_mean)), lty =1, lwd =2, col = 'red')
legend("bottomright", c("ONS","OGD","SurvONS","BOA-ONS"), col = c('blue','darkgreen','red',1), lty= c(4,6,1,2), lwd =2)



###grid 2


#open results
like_real_mean <- unlist(read.xlsx('grid2_like_real.xlsx'))
like_boa_mean <- unlist(read.xlsx('grid2_like_boa.xlsx'))
like_surv_mean <- unlist(read.xlsx('grid2_like_surv.xlsx'))
like_ons_mean <- unlist(read.xlsx('grid2_like_ons.xlsx'))
like_ogd_mean <- unlist(read.xlsx('grid2_like_ogd.xlsx'))



#graph
plot(1:n_it,cumsum(abs(like_boa_mean-like_real_mean)), type = 'l', lty =2, lwd = 2, col = 1, xlab = 'Iterations', ylab = 'Negative Log-Likelihood', cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5, ylim = c(0,0.020))
lines(1:n_it, cumsum( abs(like_ogd_mean -like_real_mean)), lty = 6, lwd =2,col = 'darkgreen')
lines(1:n_it, cumsum( abs(like_ons_mean - like_real_mean)),lty =4, lwd =2, col ='blue')
lines(1:n_it,cumsum( abs(like_surv_mean - like_real_mean)), lty =1, lwd =2, col = 'red')
legend("bottomright", c("ONS","OGD","SurvONS","BOA-ONS"), col = c('blue','darkgreen','red',1), lty= c(4,6,1,2), lwd =2)



# ============================= gamma_t estimation =======================

#density graphs


###grid 1
#open result
gamma_t <- read.xlsx('grid1_gamma.xlsx')
#compute average
gamma_mean <- matrix(0,n_it,1)
gamma_std <- matrix(0,n_it,1)

for (t in 1:n_it){
  gamma_mean[t] <- mean(unlist(gamma_t[t,]))
  gamma_std[t] <- sd(unlist(gamma_t[t,]))
}

#density
dens <- density(gamma_mean)
#graph
plot(dens, xlab =TeX(r'($\gamma_t$ estimations )'), main = '', ylab= 'Density', cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5, ylim = c(0,1.2), xlim = c(0,4))



###grid 2
#open results
gamma_t <- read.xlsx('grid2_gamma.xlsx')
#compute average
gamma_mean <- matrix(0,n_it,1)
gamma_std <- matrix(0,n_it,1)

for (t in 1:n_it){
  gamma_mean[t] <- mean(unlist(gamma_t[t,]))
  gamma_std[t] <- sd(unlist(gamma_t[t,]))
}

#density
dens <- density(gamma_mean)
#graph
plot(dens, xlab =TeX(r'($\gamma_t$ estimations )'), main = '', ylab= 'Density', cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5, ylim = c(0,1.2), xlim = c(0,4))

