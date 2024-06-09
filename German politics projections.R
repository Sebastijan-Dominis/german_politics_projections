library(readr)
library(fitdistrplus)

# Loading the data
njem <- read_csv("D:\\Sebastijan\\modeliranje_projekt\\njem_podaci.csv")
njem <- as.data.frame(njem)
njem$datum <- as.Date(njem$datum)
str(njem)
any(is.na(njem))
head(njem)

# GDP data is flawed, so it is deleted
njem$bdp <- NULL

# Quarterly GDP for 2022 and 2023
bdp <- data.frame(
  datum = c("2022-03-15", "2022-06-15", "2022-09-15", "2022-12-15",
            "2023-03-15", "2023-06-15", "2023-09-15", "2023-12-15"),
  bdp = c(943.34, 950.28, 976.58, 1006.61,
          1011.33, 1008.07, 1034.72, 1067.04)
)
bdp$datum <- as.Date(bdp$datum)
str(bdp)

# Party popularity over time
CDUpp <- njem[njem$naziv == "CDU", c("naziv", "datum", "popularnost")]
SPDpp <- njem[njem$naziv == "SPD", c("naziv", "datum", "popularnost")]
FDPpp <- njem[njem$naziv == "FDP", c("naziv", "datum", "popularnost")]
AfDpp <- njem[njem$naziv == "AfD", c("naziv", "datum", "popularnost")]
Lftpp <- njem[njem$naziv == "Left", c("naziv", "datum", "popularnost")]
Grnpp <- njem[njem$naziv == "Greens", c("naziv", "datum", "popularnost")]

# Historical party popularity data
plot(CDUpp$datum,
     CDUpp$popularnost,
     xlab = "Time",
     ylab = "Popularity",
     main = "Popularity of the German political parties over time",
     ylim = c(0, 35),
     type = "l",
     col = "black")

lines(SPDpp$datum,
      SPDpp$popularnost,
      type = "l",
      col = "red")

lines(Grnpp$datum,
      Grnpp$popularnost,
      type = "l",
      col = "green")

lines(AfDpp$datum,
      AfDpp$popularnost,
      type = "l",
      col = "blue")

lines(FDPpp$datum,
      FDPpp$popularnost,
      type = "l",
      col = "yellow")

lines(Lftpp$datum,
      Lftpp$popularnost,
      type = "l",
      col = "purple")

legend("topleft",
       legend = c("CDU", "SPD", "AfD", "The Greens", "FDP", "The Left"),
       fill = c("black", "red", "blue", "green", "yellow", "purple"),
       title = "Political party")

# Preparation to choose appropriate distributions
CDUdist <- CDUpp$popularnost
SPDdist <- SPDpp$popularnost
Grndist <- Grnpp$popularnost
AfDdist <- AfDpp$popularnost
FDPdist <- FDPpp$popularnost
Lftdist <- Lftpp$popularnost

# Choosing a distribution for CDU
CDUfits <- list(
  CDUnormal <- fitdist(CDUdist, "norm"),
  CDUuniform <- fitdist(CDUdist, "unif"),
  CDUexponential <- fitdist(CDUdist, "exp"),
  CDUlogistic <- fitdist(CDUdist, "logis"),
  CDUlognormal <- fitdist(CDUdist, "lnorm"),
  CDUgamma <- fitdist(CDUdist, "gamma"),
  CDUweibull <- fitdist(CDUdist, "weibull")
)

sapply(CDUfits, function(i) i$loglik)

# Weibull was chosen

# Choosing a distribution for SPD
SPDfits <- list(
  SPDnormal <- fitdist(SPDdist, "norm"),
  SPDuniform <- fitdist(SPDdist, "unif"),
  SPDexponential <- fitdist(SPDdist, "exp"),
  SPDlogistic <- fitdist(SPDdist, "logis"),
  SPDlognormal <- fitdist(SPDdist, "lnorm"),
  SPDgamma <- fitdist(SPDdist, "gamma"),
  SPDweibull <- fitdist(SPDdist, "weibull")
)

sapply(SPDfits, function(i) i$loglik)

# Uniform was chosen

# Choosing a distribution for the Greens
Grnfits <- list(
  Grnnormal <- fitdist(Grndist, "norm"),
  Grnuniform <- fitdist(Grndist, "unif"),
  Grnexponential <- fitdist(Grndist, "exp"),
  Grnlogistic <- fitdist(Grndist, "logis"),
  Grnlognormal <- fitdist(Grndist, "lnorm"),
  Grngamma <- fitdist(Grndist, "gamma"),
  Grnweibull <- fitdist(Grndist, "weibull")
)

sapply(Grnfits, function(i) i$loglik)

# Uniform was chosen

# Choosing a distribution for FDP
FDPfits <- list(
  FDPnormal <- fitdist(FDPdist, "norm"),
  FDPuniform <- fitdist(FDPdist, "unif"),
  FDPexponential <- fitdist(FDPdist, "exp"),
  FDPlogistic <- fitdist(FDPdist, "logis"),
  FDPlognormal <- fitdist(FDPdist, "lnorm"),
  FDPgamma <- fitdist(FDPdist, "gamma"),
  FDPweibull <- fitdist(FDPdist, "weibull")
)

sapply(FDPfits, function(i) i$loglik)

# Uniform was chosen

# Choosing a distribution for AfD
AfDfits <- list(
  AfDnormal <- fitdist(AfDdist, "norm"),
  AfDuniform <- fitdist(AfDdist, "unif"),
  AfDexponential <- fitdist(AfDdist, "exp"),
  AfDlogistic <- fitdist(AfDdist, "logis"),
  AfDlognormal <- fitdist(AfDdist, "lnorm"),
  AfDgamma <- fitdist(AfDdist, "gamma"),
  AfDweibull <- fitdist(AfDdist, "weibull")
)

sapply(AfDfits, function(i) i$loglik)

# Uniform was chosen

# Choosing a distribution for the Left
Lftfits <- list(
  Lftnormal <- fitdist(Lftdist, "norm"),
  Lftuniform <- fitdist(Lftdist, "unif"),
  Lftexponential <- fitdist(Lftdist, "exp"),
  Lftlogistic <- fitdist(Lftdist, "logis"),
  Lftlognormal <- fitdist(Lftdist, "lnorm"),
  Lftgamma <- fitdist(Lftdist, "gamma"),
  Lftweibull <- fitdist(Lftdist, "weibull")
)

sapply(Lftfits, function(i) i$loglik)

# Gamma was chosen

# Monte Carlo function with Weibull distribution
monte_carlo_weibull <- function(data, months = 3, simulations = 100) {
  data <- data[order(data$datum), ]
  monthly_changes <- diff(data$popularnost)
  
  shift <- abs(min(monthly_changes)) + 1
  shifted_changes <- monthly_changes + shift
  
  fit <- fitdist(shifted_changes, "weibull")
  shape <- fit$estimate['shape']
  scale <- fit$estimate['scale']
  
  last_popularity <- tail(data$popularnost, 1)
  simulations_matrix <- matrix(NA, nrow = simulations, ncol = months)
  
  for (i in 1:simulations) {
    simulated_popularity <- numeric(months)
    simulated_popularity[1] <- last_popularity + rweibull(1, shape, scale) - shift
    
    for (j in 2:months) {
      simulated_popularity[j] <- simulated_popularity[j - 1] + rweibull(1, shape, scale) - shift
    }
    
    simulations_matrix[i, ] <- simulated_popularity
  }
  
  simulations_df <- as.data.frame(simulations_matrix)
  colnames(simulations_df) <- paste0("Month_", 1:months)
  
  return(simulations_df)
}

# Monte Carlo function with uniform distribution
monte_carlo_uniform <- function(data, months = 3, simulations = 100) {
  
  data <- data[order(data$datum), ]
  monthly_changes <- diff(data$popularnost)
  
  mean_change <- mean(monthly_changes)
  sd_change <- sd(monthly_changes)
  
  last_popularity <- tail(data$popularnost, 1)
  
  simulations_matrix <- matrix(NA, nrow = simulations, ncol = months)
  
  for (i in 1:simulations) {
    simulated_popularity <- numeric(months)
    simulated_popularity[1] <- last_popularity + runif(1, mean_change - sd_change, mean_change + sd_change)
    
    for (j in 2:months) {
      simulated_popularity[j] <- simulated_popularity[j - 1] + runif(1, mean_change - sd_change, mean_change + sd_change)
    }
    
    simulations_matrix[i, ] <- simulated_popularity
  }
  
  simulations_df <- as.data.frame(simulations_matrix)
  colnames(simulations_df) <- paste0("Month_", 1:months)
  
  return(simulations_df)
}

# Monte Carlo function with gamma distribution
monte_carlo_gamma <- function(data, months = 3, simulations = 100) {
  data <- data[order(data$datum), ]
  monthly_changes <- abs(diff(data$popularnost))
  
  mean_change <- mean(monthly_changes)
  var_change <- var(monthly_changes)
  
  shape <- (mean_change^2) / var_change
  rate <- mean_change / var_change
  
  last_popularity <- tail(data$popularnost, 1)
  simulations_matrix <- matrix(NA, nrow = simulations, ncol = months)
  
  for (i in 1:simulations) {
    simulated_popularity <- numeric(months)
    simulated_popularity[1] <- last_popularity + rgamma(1, shape, rate)
    
    for (j in 2:months) {
      simulated_popularity[j] <- simulated_popularity[j - 1] + rgamma(1, shape, rate)
    }
    
    simulations_matrix[i, ] <- simulated_popularity
  }
  
  simulations_df <- as.data.frame(simulations_matrix)
  colnames(simulations_df) <- paste0("Month_", 1:months)
  
  return(simulations_df)
}


# CDU simulation
sim_CDU <- monte_carlo_weibull(CDUpp, months = 3, simulations = 100)

summary_statsCDU <- apply(sim_CDU, 2, function(x) {
  c(mean = mean(x), sd = sd(x), min = min(x), max = max(x))
})

summary_statsCDU_df <- as.data.frame(t(summary_statsCDU))
summary_statsCDU_df$Month <- 1:3
summary_statsCDU_df

sr_pop_CDU <- colMeans(sim_CDU)

plot(1:3, sr_pop_CDU, type = "l", col = "red", lwd = 2,
     ylim = range(sim_CDU), xlab = "Months", ylab = "Simulated popularity - CDU",
     main = "Monte Carlo simulation for the popularity of CDU over the next three months")

for (i in 1:nrow(sim_CDU)) {
  lines(1:3, sim_CDU[i, ], col = rgb(0, 0, 1, alpha = 0.1))
}

lines(1:3, sr_pop_CDU, col = "red", lwd = 2)

legend("topleft", legend = "Average popularity", col = "red", lwd = 2)

# SPD simulation
sim_SPD <- monte_carlo_uniform(SPDpp, months = 3, simulations = 100)

summary_statsSPD <- apply(sim_SPD, 2, function(x) {
  c(mean = mean(x), sd = sd(x), min = min(x), max = max(x))
})

summary_statsSPD_df <- as.data.frame(t(summary_statsSPD))
summary_statsSPD_df$Month <- 1:3
summary_statsSPD_df

sr_pop_SPD <- colMeans(sim_SPD)

plot(1:3, sr_pop_SPD, type = "l", col = "red", lwd = 2,
     ylim = range(sim_SPD), xlab = "Months", ylab = "Simulated popularity - SPD",
     main = "Monte Carlo simulation for the popularity of SPD over the next three months")

for (i in 1:nrow(sim_SPD)) {
  lines(1:3, sim_SPD[i, ], col = rgb(0, 0, 1, alpha = 0.1))
}

lines(1:3, sr_pop_SPD, col = "red", lwd = 2)

legend("topleft", legend = "Average popularity", col = "red", lwd = 2)

# the Greens simulation
sim_Grn <- monte_carlo_uniform(Grnpp, months = 3, simulations = 100)

summary_statsGrn <- apply(sim_Grn, 2, function(x) {
  c(mean = mean(x), sd = sd(x), min = min(x), max = max(x))
})

summary_statsGrn_df <- as.data.frame(t(summary_statsGrn))
summary_statsGrn_df$Month <- 1:3
summary_statsGrn_df

sr_pop_Grn <- colMeans(sim_Grn)

plot(1:3, sr_pop_Grn, type = "l", col = "red", lwd = 2,
     ylim = range(sim_Grn), xlab = "Months", ylab = "Simulated popularity - the Greens",
     main = "Monte Carlo simulation for the popularity of the Greens over the next three months")

for (i in 1:nrow(sim_Grn)) {
  lines(1:3, sim_Grn[i, ], col = rgb(0, 0, 1, alpha = 0.1))
}

lines(1:3, sr_pop_Grn, col = "red", lwd = 2)

legend("topleft", legend = "Average popularity", col = "red", lwd = 2)

# the Left simulation
sim_Lft <- monte_carlo_gamma(Lftpp, months = 3, simulations = 100)

summary_statsLft <- apply(sim_Lft, 2, function(x) {
  c(mean = mean(x), sd = sd(x), min = min(x), max = max(x))
})

summary_statsLft_df <- as.data.frame(t(summary_statsLft))
summary_statsLft_df$Month <- 1:3
summary_statsLft_df

sr_pop_Lft <- colMeans(sim_Lft)

plot(1:3, sr_pop_Lft, type = "l", col = "red", lwd = 2,
     ylim = range(sim_Lft), xlab = "Months", ylab = "Simulated popularity - the Left",
     main = "Monte Carlo simulation for the popularity of the Left over the next three months")

for (i in 1:nrow(sim_Lft)) {
  lines(1:3, sim_Lft[i, ], col = rgb(0, 0, 1, alpha = 0.1))
}

lines(1:3, sr_pop_Lft, col = "red", lwd = 2)

legend("topleft", legend = "Average popularity", col = "red", lwd = 2)

# FDP simulation
sim_FDP <- monte_carlo_uniform(FDPpp, months = 3, simulations = 100)

summary_statsFDP <- apply(sim_FDP, 2, function(x) {
  c(mean = mean(x), sd = sd(x), min = min(x), max = max(x))
})

summary_statsFDP_df <- as.data.frame(t(summary_statsFDP))
summary_statsFDP_df$Month <- 1:3
summary_statsFDP_df

sr_pop_FDP <- colMeans(sim_FDP)

plot(1:3, sr_pop_FDP, type = "l", col = "red", lwd = 2,
     ylim = range(sim_FDP), xlab = "Months", ylab = "Simulated popularity - FDP",
     main = "Monte Carlo simulation for the popularity of FDP over the next three months")

for (i in 1:nrow(sim_FDP)) {
  lines(1:3, sim_FDP[i, ], col = rgb(0, 0, 1, alpha = 0.1))
}

lines(1:3, sr_pop_FDP, col = "red", lwd = 2)

legend("topleft", legend = "Average popularity", col = "red", lwd = 2)

# AfD simulation
sim_AfD <- monte_carlo_uniform(AfDpp, months = 3, simulations = 100)

summary_statsAfD <- apply(sim_AfD, 2, function(x) {
  c(mean = mean(x), sd = sd(x), min = min(x), max = max(x))
})

summary_statsAfD_df <- as.data.frame(t(summary_statsAfD))
summary_statsAfD_df$Month <- 1:3
summary_statsAfD_df

sr_pop_AfD <- colMeans(sim_AfD)

plot(1:3, sr_pop_AfD, type = "l", col = "red", lwd = 2,
     ylim = range(sim_AfD), xlab = "Months", ylab = "Simulated popularity - AfD",
     main = "Monte Carlo simulation for the popularity of AfD over the next three months")

for (i in 1:nrow(sim_AfD)) {
  lines(1:3, sim_AfD[i, ], col = rgb(0, 0, 1, alpha = 0.1))
}

lines(1:3, sr_pop_AfD, col = "red", lwd = 2)

legend("topleft", legend = "Average popularity", col = "red", lwd = 2)

# Potentially important factors
imigracija <- njem[!duplicated(njem$datum), c("datum", "imigracija")]
inflacija <- njem[!duplicated(njem$datum), c("datum", "inflacija")]
temperatura <- njem[!duplicated(njem$datum), c("datum", "temperature")]

# Displaying the potentially important factors
par(mfrow = c(2,2))
plot(imigracija$datum,
     imigracija$imigracija,
     xlab = "Time",
     ylab = "Number of immigrants",
     main = "Immigration over time",
     col = "black",
     type = "l")
plot(inflacija$datum,
     inflacija$inflacija,
     xlab = "Time",
     ylab = "Inflation (%)",
     main = "Inflation over time",
     type = "l",
     col = "blue")
plot(temperatura$datum,
     temperatura$temperature,
     xlab = "Time",
     ylab = "Temperature (C)",
     type = "l",
     col = "orange",
     main = "Average global temperature by month")
plot(bdp$datum,
     bdp$bdp,
     xlab = "Time",
     ylab = "GDP",
     main = "Quarterly nominal GDP (EUR, billions)",
     type = "l",
     col = "green")
