###### Bayesian Hierarchical Logistic Regression ######


setwd("C:/Users/Genghis_Zhang/Desktop/Hierarchical Logistic Regression/ped")
grant <- read.csv("grant.csv", header = T)


grant$outcome <- ifelse(grant$Response.Behavior == 0, 0, 1)
grant$FlowOn <- grant$Flow.against.North + grant$Flow.against.North
grant$pedWait <- grant$Ped.at.Same.Direc.North + grant$Ped.at.Diff.Direc.North
grant$int <- 1


X <- cbind(grant$Group.Size, grant$Age.Range, grant$Gender, 
           grant$Adjacent.Vehicle.Y.1.N.0, 
           grant$Interacted.Vehicle.Type..Motor.1..Small.Car.2..3.Truck.bus,
           grant$Distance, grant$Approaching.Speed, grant$Near.or.Far.Side, 
           grant$FlowOn, grant$pedWait, grant$Vehicle.Close.Follower, 
           grant$Cellphone, grant$Talking, grant$int)


library("rstan")
grant_data <- list(
  D = 14,
  N = length(grant$outcome),
  L = 1,
  X = as.matrix(X),
  y = grant$outcome,
  ll = grant$int
)

fit_logistic_grant <- stan(
  file = "log1.stan",  # Stan program
  data = grant_data,    # named list of data
  chains = 4,             # number of Markov chains
  warmup = 5000,          # number of warmup iterations per chain
  iter = 30000,            # total number of iterations per chain
  cores = 2,              # number of cores (could use one per chain)
  refresh = 1,             # no progress shown
  control = list(adapt_delta = 0.95)
)



########################### University St. ##########################

# data section
ped <- read.csv("Bayesian.csv", header = TRUE)          # read csv
ped$outcome <- ifelse(ped$Response.Behavior == 0, 0, 1) # outcome of interest
ped$int <- 1                                            # intercept

## 75% of the sample size
smp_size <- floor(0.75 * nrow(ped))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(ped)), size = smp_size)

train <- ped[train_ind, ]                                # train set 
test <- ped[-train_ind, ]                                # test set

# covariates

X <- cbind(ped$Group.Size, ped$Age.Range, ped$Gender, 
           ped$Adjacent.Vehicle.Y.1.N.0, 
           ped$Interacted.Vehicle.Type..Motor.1..Small.Car.2..3.Truck.bus,
           ped$Distance, ped$Approaching.Speed, 
           ped$Near.Side.1.Far.Side.2, ped$FlowOn, ped$FlowWait, 
           ped$Close.Follower., ped$Cellphone, ped$Talking, ped$int)


# stan module

library("rstan")

ped_data <- list(
  D = 14,
  N = length(ped$outcome),
  L = max(ped$Time),
  X = as.matrix(X),
  y = ped$outcome,
  ll = ped$Time
)

fit_logistic_ped1 <- stan(
  file = "log1.stan",      # Stan program
  data = ped_data,         # named list of data
  chains = 4,              # number of Markov chains
  warmup = 1500,           # number of warmup iterations per chain
  iter = 8000,             # total number of iterations per chain
  cores = 2,               # number of cores (could use one per chain)
  refresh = 1,             # no progress shown
  control = list(adapt_delta = 0.95)
)

################ test block ##############

# test <- ped
mu_beta_summary <- summary(fit_logistic_ped1, pars = "beta", 
                           probs = c(0.025, 0.975))$summary       # export csv
beta <- mu_beta_summary[,1]                                       # extract beta
y_test <- matrix(0, 1, length(test[,1]))                          # test statistics

# testing session

for(i in 1:length(test[,1])){
  X <- cbind(test$Group.Size[i], test$Age.Range[i], test$Gender[i], test$Adjacent.Vehicle.Y.1.N.0[i], 
             test$Interacted.Vehicle.Type..Motor.1..Small.Car.2..3.Truck.bus[i],
             test$Distance[i], test$Approaching.Speed[i], test$Near.Side.1.Far.Side.2[i], 
             test$FlowOn[i], test$FlowWait[i], test$Close.Follower.[i], test$Cellphone[i], test$Talking[i], test$int[i])
  
  if (test$Time[i] == 1){
    beta_t <- beta[1:14]
  }
  else if (test$Time[i] == 2){
    beta_t <- beta[15:28]
  }
  else if (test$Time[i] == 3){
    beta_t <- beta[29:42]
  }
  else if (test$Time[i] == 4){
    beta_t <- beta[43:56]
  }
  
  y_test[i] <- sum(beta_t*X)
}

y_rep <- ifelse(y_test>0, 1, 0)

# validation accuracy

accuracy <- sum(y_rep == test$outcome)/length(test[,1])


############# plot version - ggplot ################

library("ggplot2")
library("ggpubr")

dens <- stan_hist(fit_logistic_ped1, pars = "beta[1, 4]", fill = "lightblue") + 
  ggtitle("Adjacent Coefficient At Time 1") + xlab("") +
  theme(text = element_text(size=16), plot.title = element_text(size=16)) + 
  geom_vline(xintercept=c(-1.65, 0.29), linetype="dashed", color = "red", size = 1)

dens1 <- stan_hist(fit_logistic_ped1, pars = "beta[3, 4]", fill = "lightblue") + 
  ggtitle("Adjacent Coefficient At Time 2") + xlab("") + 
  theme(text = element_text(size=16), plot.title = element_text(size=16)) + 
  geom_vline(xintercept=c(-2.79, -0.66), linetype="dashed", color = "red", size = 1)

dens2 <- stan_hist(fit_logistic_ped1, pars = "beta[4, 4]", fill = "lightblue") + 
  ggtitle("Adjacent Coefficient At Time 3") + xlab("") +
  theme(text = element_text(size=16), plot.title = element_text(size=16)) + 
  geom_vline(xintercept=c(-1.9, -0.74), linetype="dashed", color = "red", size = 1)


dens3 <- stan_hist(fit_logistic_ped1, pars = "beta[2, 4]", fill = "lightblue") + 
  ggtitle("Adjacent Coefficient At Time 4") + xlab("") +
  theme(text = element_text(size=16), plot.title = element_text(size=16)) +
  geom_vline(xintercept=c(-1.14, -0.22), linetype="dashed", color = "red", size = 1)

figure3 <- ggarrange(dens, dens1, dens2, dens3)
figure3


dens <- stan_hist(fit_logistic_ped1, pars = "beta[1, 6]",fill = "lightblue") + 
  ggtitle("Distance Coefficient At Time 1") + xlab("") +
  theme(text = element_text(size=16), plot.title = element_text(size=16)) + 
  geom_vline(xintercept=c(0.030, 0.051), linetype="dashed", color = "red", size = 1)


dens1 <- stan_hist(fit_logistic_ped1, pars = "beta[3, 6]",fill = "lightblue") + 
  ggtitle("Distance Coefficient At Time 2") + xlab("") +
  theme(text = element_text(size=16), plot.title = element_text(size=16)) + 
  geom_vline(xintercept=c(0.031, 0.050), linetype="dashed", color = "red", size = 1)


dens2 <- stan_hist(fit_logistic_ped1, pars = "beta[4, 6]", fill = "lightblue") + 
  ggtitle("Distance Coefficient At Time 3") + xlab("") +
  theme(text = element_text(size=16), plot.title = element_text(size=16)) + 
  geom_vline(xintercept=c(0.028, 0.042), linetype="dashed", color = "red", size = 1)


dens3 <- stan_hist(fit_logistic_ped1, pars = "beta[2, 6]", fill = "lightblue") + 
  ggtitle("Distace Coefficient At Time 4") + xlab("") + 
  theme(text = element_text(size=16), plot.title = element_text(size=16)) + 
  geom_vline(xintercept=c(0.028, 0.040), linetype="dashed", color = "red", size = 1)

figure10 <- ggarrange(dens, dens1, dens2, dens3)
figure10

dens <- stan_hist(fit_logistic_ped1, pars = "beta[1, 7]", fill = "lightblue") + 
  ggtitle("Speed Coefficient At Time 1") + xlab("") +
  theme(text = element_text(size=16), plot.title = element_text(size=16)) + 
  geom_vline(xintercept=c(-0.0073, -0.0024), linetype="dashed", color = "red", size = 1)


dens1 <- stan_hist(fit_logistic_ped1, pars = "beta[3, 7]", fill = "lightblue") + 
  ggtitle("Speed Coefficient At Time 2") + xlab("") +
  theme(text = element_text(size=16), plot.title = element_text(size=16)) + 
  geom_vline(xintercept=c(-0.0067, -0.0032), linetype="dashed", color = "red", size = 1)

dens2 <- stan_hist(fit_logistic_ped1, pars = "beta[4, 7]", fill = "lightblue") + 
  ggtitle("Speed Coefficient At Time 3") + xlab("") +
  theme(text = element_text(size=16), plot.title = element_text(size=16)) + 
  geom_vline(xintercept=c(-0.0032, -0.0007), linetype="dashed", color = "red", size = 1)

dens3 <- stan_hist(fit_logistic_ped1, pars = "beta[2, 7]", fill = "lightblue") + 
  ggtitle("Speed Coefficient At Time 4") + xlab("") +
  theme(text = element_text(size=16), plot.title = element_text(size=16)) + 
  geom_vline(xintercept=c(-0.0047, -0.0027), linetype="dashed", color = "red", size = 1)

figure11 <- ggarrange(dens, dens1, dens2, dens3)
figure11

# FlowOn

FlowOn1_trace <- stan_hist(fit_logistic_ped1, pars = "beta[1, 9]", fill = "lightblue") + 
  ggtitle("FlowOn Coefficient At Time 1") + xlab("Iterations") +
  theme(text = element_text(size=16), plot.title = element_text(size=16)) + 
  geom_vline(xintercept=c(0.24, 1.22), linetype="dashed", color = "red", size = 1)

FlowOn2_trace <- stan_hist(fit_logistic_ped1, pars = "beta[3, 9]", fill = "lightblue") + 
  ggtitle("FlowOn Coefficient At Time 2") + xlab("Iterations")+ 
  theme(text = element_text(size=16), plot.title = element_text(size=16)) + 
  geom_vline(xintercept=c(0.55, 1.18), linetype="dashed", color = "red", size = 1)

FlowOn4_trace <- stan_hist(fit_logistic_ped1, pars = "beta[4, 9]", fill = "lightblue") + 
  ggtitle("FlowOn Coefficient At Time 4") + xlab("Iterations") + 
  theme(text = element_text(size=16), plot.title = element_text(size=16)) + 
  geom_vline(xintercept=c(0.49, 0.85), linetype="dashed", color = "red", size = 1)

FlowOn3_trace <- stan_hist(fit_logistic_ped1, pars = "beta[2, 9]",fill = "lightblue") + 
  ggtitle("FlowOn Coefficient At Time 3") + xlab("Iterations") + ylab("Estimation") + 
  theme(text = element_text(size=16), plot.title = element_text(size=16)) + 
  geom_vline(xintercept=c(0.56, 0.84), linetype="dashed", color = "red", size = 1)

figure21 <- ggarrange(FlowOn1_trace, FlowOn2_trace, FlowOn3_trace, FlowOn4_trace)
figure21


# addtional plots for the paper

library("ggplot2")
library("ggpubr")

flow1 <- ggplot(ped[ped$Time == 1, ], aes(x=FlowOn)) + 
  geom_bar(color="darkblue", fill="lightblue") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Time 1") + xlab("FlowOn") + xlim(-2, 15) + ylab("Frequency") +
  theme(text = element_text(size=16), plot.title = element_text(size=16))



flow2 <- ggplot(ped[ped$Time == 3, ], aes(x=FlowOn)) + 
  geom_bar(color="darkblue", fill="lightblue") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Time 2") + xlab("FlowOn")+ xlim(-2, 15) + ylab("Frequency") +
  theme(text = element_text(size=16), plot.title = element_text(size=16))


flow3 <- ggplot(ped[ped$Time == 4, ], aes(x=FlowOn)) + 
  geom_bar(color="darkblue", fill="lightblue") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Time 3") + xlab("FlowOn")+ xlim(-2, 15) + ylab("Frequency") +
  theme(text = element_text(size=16), plot.title = element_text(size=16))


flow4 <- ggplot(ped[ped$Time == 2, ], aes(x=FlowOn)) + 
  geom_bar(color="darkblue", fill="lightblue") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Time 4") + xlab("FlowOn")+ xlim(-2, 15) + ylab("Frequency") +
  theme(text = element_text(size=16), plot.title = element_text(size=16))


figure_hist_flow <- ggarrange(flow1, flow2, flow3, flow4)
figure_hist_flow



adj1 <- ggplot(ped[ped$Time == 1, ], aes(x=Adjacent.Vehicle.Y.1.N.0)) + 
  geom_bar(color="darkblue", fill="lightblue") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Time 1") + xlab("Adjacent") + xlim(-0.5, 4) + ylab("Frequency") +
  theme(text = element_text(size=16), plot.title = element_text(size=16))



adj2 <- ggplot(ped[ped$Time == 3, ], aes(x=Adjacent.Vehicle.Y.1.N.0)) + 
  geom_bar(color="darkblue", fill="lightblue") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Time 2") + xlab("Adjacent")+ xlim(-0.5, 4) + ylab("Frequency") +
  theme(text = element_text(size=16), plot.title = element_text(size=16))


adj3 <- ggplot(ped[ped$Time == 4, ], aes(x=Adjacent.Vehicle.Y.1.N.0)) + 
  geom_bar(color="darkblue", fill="lightblue") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Time 3") + xlab("Adjacent")+ xlim(-0.5, 4) + ylab("Frequency") +
  theme(text = element_text(size=16), plot.title = element_text(size=16))


adj4 <- ggplot(ped[ped$Time == 2, ], aes(x=Adjacent.Vehicle.Y.1.N.0)) + 
  geom_bar(color="darkblue", fill="lightblue") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Time 4") + xlab("Adjacent")+ xlim(-0.5, 4) + ylab("Frequency") +
  theme(text = element_text(size=16), plot.title = element_text(size=16))


figure_hist_adj <- ggarrange(adj1, adj2, adj3, adj4)
figure_hist_adj
