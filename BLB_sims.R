#########SIMULATIONS####
set.seed(1576)
X = rnorm(10^6, 0,25)
MSE = function(theta_hat, theta){(theta_hat-theta)^2}
subsets = 2:10

time.general = c()
for(s in subsets){
  time.100 = system.time(BLBoot(X, mean,MSE,s, iter = 100))[1]
  time.200 = system.time(BLBoot(X, mean,MSE,s, iter = 200))[1]
  time.general = cbind(time.general, c(time.100,time.200))
}

time.parl = c()
for(s in subsets){
  time.100 = system.time(BLB_par(X, mean,MSE,s, iter = 100))[3]
  time.200 = system.time(BLB_par(X, mean,MSE,s, iter = 200))[3]
  time.parl= cbind(time.parl, c(time.100,time.200))
}


#Plots
library(ggplot2)


qplot(x = subsets, y = time.general[1,], geom = "point") + labs(title = 'General Timing (iter = 100)', x = 'Subsets with iter = 100', y = 'Time in Seconds')
ggsave("general100.jpeg", width=3,height=3)

qplot(x = subsets, y = time.general[2,], geom = "point") +  labs(title = 'General Timing (iter = 200)',x = 'Subsets with iter = 200', y = 'Time in Seconds')
ggsave("general200.jpeg", width=3,height=3)


qplot(x = subsets, y = time.parl[1,], geom = "point") + labs(title = 'Parallel Timing (iter = 100)', x = 'Subsets with iter = 100', y = 'Time in Seconds')
ggsave("parallel100.jpeg", width=3,height=3)

qplot(x = subsets, y = time.parl[2,], geom = "point") + labs(title = 'Parallel Timing (iter = 200)', x = 'Subsets with iter = 200', y = 'Time in Seconds')
ggsave("parallel200.jpeg", width=3,height=3)


time.saved = time.general[1,] - time.parl[1,]

time.saved = cbind(time.saved,time.general[2,] - time.parl[2,])




