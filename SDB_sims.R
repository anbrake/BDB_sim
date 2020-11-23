#################RUN SIMULATIONS
set.seeed(1576)
X = rnorm(10^6, 0,25)
MSE = function(theta_hat, theta){(theta_hat-theta)^2}
times = seq(10,310,30)

###Regular SDB
iters = c()
act_time = c()
for(i in 1:length(times)){
  start= proc.time()
  size = SDBBoot(X, mean, MSE,subset_size = .6*10^6, time_lim = times[i])
  iters = c(iters, size$iter)
  end = proc.time() - start
  print(end)
  act_time = rbind(act_time, end)
}
penalty_time = act_time[,1]-times
reg_sdb = data.frame(iters,times, act_time[,1], penalty_time)
save(reg_sdb, file = "reg_sdb.RData")



##############Parallel Naive sdb########
#niter = 50
iters.p1 = c()
act_timeP1 = c()
for(i in 1:length(times)){
  start= proc.time()
  size = SDB_ParallelNaive(X, mean, MSE,subset_size = .6*10^6, niter = 50, time_lim = times[i])
  iters.p1 = c(iters.p1, size$iter)
  end = proc.time() - start
  print(end)
  act_timeP1 = rbind(act_timeP1, end)
}
penalty_time1 = act_timeP1[,3]-times
par50_sdb = data.frame(iters.p1,times, act_timeP1[,1], penalty_time1)
save(par50_sdb, file = "par50_sdb.RData")

#niter=100
iters.p2 = c()
act_timeP2 = c()
for(i in 1:length(times)){
  start= proc.time()
  size = SDB_ParallelNaive(X, mean, MSE,subset_size = .6*10^6, niter = 100, time_lim = times[i])
  iters.p2 = c(iters.p2, size$iter)
  end = proc.time() - start
  print(end)
  act_timeP2 = rbind(act_timeP2, end)
}
penalty_time2 = act_timeP2[,3]-times
par100_sdb = data.frame(iters.p2,times, act_timeP2[,1], penalty_time2)
save(par100_sdb, file = "par100_sdb.RData")


#####Parallel Time solve SDB####

#Use c= 0.9
iters.p3 = c()
act_timeP3 = c()
for(i in 1:length(times)){
  start= proc.time()
  size = SDB_ParallelTimeSolve(X, mean, MSE, subset_size = .6*10^6,c= 0.8, time_lim = times[i])
  iters.p3 = c(iters.p3, size$iter)
  end = proc.time() - start
  print(end)
  act_timeP3 = rbind(act_timeP3, end)
}
penalty_time3 = act_timeP3[,3]-times-30
sdb_ts1 = data.frame(iters.p3,times, act_timeP3[,1], penalty_time3)

#Use c = 0.6
iters.p4 = c()
act_timeP4 = c()
for(i in 1:length(times)){
  start= proc.time()
  size = SDB_ParallelTimeSolve(X, mean, MSE,subset_size = .6*10^6, c= 0.2, time_lim = times[i])
  iters.p4 = c(iters.p4, size$iter)
  end = proc.time() - start
  print(end)
  act_timeP4 = rbind(act_timeP4, end)
}
penalty_time4 = act_timeP3[,3]-times-30
sdb_ts2 = data.frame(iters.p4,times, act_timeP4[,1], penalty_time4)

####plots####
library(ggplot2)
qplot(x = times, y = iters, data = reg_sdb)
ggsave("sdbreg.jpeg", width = 2, height = 2)

qplot(x = times, y = penalty_time, data = reg_sdb)
ggsave("sdbreg_pt.jpeg", width = 2, height = 2)

qplot(x = times, y = iters, data = par50_sdb)
ggsave("sdb_par50.jpeg", width = 3, height = 3)

qplot(x = times, y = penalty_time, data = par50_sdb)
ggsave("sdbpar50_pt.jpeg", width = 3, height = 3)


qplot(x = times, y = iters.p2, data = par100_sdb) + ylab("Iterations")
ggsave("sdb_par100.jpeg", width = 3, height = 3)

qplot(x = times, y = penalty_time2, data = par100_sdb) + ylab("Penalty Time")
ggsave("sdbpar100_pt.jpeg", width = 3, height = 3)


