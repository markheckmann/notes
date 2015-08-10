### R code from vignette source '/Users/markheckmann/Documents/tex/2012_simulation_reworked/sim_binomial_prop.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: sim_binomial_prop.Rnw:13-15
###################################################
options(width=75)         # set width to 60 characters
options(continue=" ")     # remove continuation prompt


###################################################
### code chunk number 2: sim_binomial_prop.Rnw:35-42
###################################################
ci_standard <- function(p.hat, n, alpha=.05){
  ci <- qnorm(1 - alpha/2) * sqrt(p.hat * (1 - p.hat) / n) 
  lower <- p.hat - ci                       # oberes KI            
  upper <- p.hat + ci                       # unteres KI
  c(p.hat=p.hat, lower=lower, upper=upper)
}  
ci_standard(p.hat=.05, n=500)


###################################################
### code chunk number 3: sim_binomial_prop.Rnw:49-50
###################################################
set.seed(1)


###################################################
### code chunk number 4: sim_binomial_prop.Rnw:53-65
###################################################
sim_one_ci <- function(n, p, alpha=.05){
  vals <- rbinom(n, 1, p)                # Stichprobe ziehen 
  p.hat <- mean(vals)                    # geschätzter Anteil 
  cis <- ci_standard(p.hat, n, alpha)    # KI berechnen
  lower <- cis[["lower"]]                # unteres KI
  upper <- cis[["upper"]]                # oberes KI
  c(p.hat=p.hat,                         # Punktschätzung
    lower=lower, upper=upper,            # KIs ausgeben
    covered= lower <= p && p <= upper)   # liegt p im Intervall?
}

sim_one_ci(n=100, p=.1, alpha=.05)


###################################################
### code chunk number 5: sim_binomial_prop.Rnw:70-80
###################################################
sim_many_ci <- function(reps, n, p, alpha=.05){
  ncovered <- 0                       # Zähler p innerhalb des KI?
  for (i in 1:reps){                  # reps Male durchlaufen
    sam <- sim_one_ci(n, p, alpha)    # eine Stichprobe ziehen
    if (sam["covered"] == 1)          # ist p im Intervall?       
      ncovered <- ncovered + 1        # wenn ja Zähler erhöhen 
  }
  c(p=p, n=n,                         # Parameter zurückgeben
    prop.covered=ncovered/reps)       # Anteil p innerhalb KI
}


###################################################
### code chunk number 6: sim_binomial_prop.Rnw:85-86
###################################################
sim_many_ci(reps=1000, n=100, p=.1)  


###################################################
### code chunk number 7: sim_binomial_prop.Rnw:91-92
###################################################
sim_many_ci(reps=10000, n=100, p=.1)  


###################################################
### code chunk number 8: sim_binomial_prop.Rnw:97-98
###################################################
sim_many_ci(reps=10000, n=100, p=.05)  


###################################################
### code chunk number 9: sim_binomial_prop.Rnw:103-115
###################################################
p_study <- function(reps, n, alpha=.05,
                    p.start=.01, p.end=.99, p.step=.1){  
  ps <- seq(p.start, p.end, p.step)                   
  rmat <- matrix(0, nrow=length(ps), ncol=3)    # Ergebnismatrix
  for (i in 1:length(ps)){                      # Durchlauf für jedes p
    p <- ps[i]                                  # neuer p Wert
    rmat[i, ] <- sim_many_ci(reps, n, p, alpha) # Ziehungen simulieren 
  } 
  res <- as.data.frame(rmat)                   
  names(res) <- c("p", "n", "prop.covered")     # Spalten benennen 
  res
} 


###################################################
### code chunk number 10: sim_binomial_prop.Rnw:120-123
###################################################
x <- p_study(100, 100, .05, p.step=.005)
plot(x[c("p","prop.covered")], type="l") 
abline(h=.95, col="red")


###################################################
### code chunk number 11: sim_binomial_prop.Rnw:128-131 (eval = FALSE)
###################################################
## x <- p_study(100000, 100, .05, p.step=.005) 
## plot(x[c("p","prop.covered")], type="l") 
## abline(h=.95, col="red")


###################################################
### code chunk number 12: sim_binomial_prop.Rnw:133-134 (eval = FALSE)
###################################################
## write.csv2(x, "data/p_study_standard_p05_reps100000.csv")


###################################################
### code chunk number 13: sim_binomial_prop.Rnw:136-139
###################################################
x <- read.csv2("data/p_study_standard_p05_reps100000.csv")
plot(x[c("p","prop.covered")], type="l") 
abline(h=.95, col="red")


###################################################
### code chunk number 14: sim_binomial_prop.Rnw:150-164 (eval = FALSE)
###################################################
## sim_many_ci_2 <- function(reps, n, p, alpha=.05){
##   res <- replicate(reps, sim_one_ci(n, p, alpha))
##   prop.covered <- mean(res["covered", ])
##   c(p=p, n=n, prop.covered=prop.covered)                   
## } 
## 
## p_study_2 <- function(reps, n, alpha=.05,
##                       p.start=.01, p.end=.99, p.step=.1){
##   ps <- seq(p.start, p.end, p.step)                     
##   res <- mapply(sim_many_ci_2, reps=reps,
##                 n=n, p=ps, alpha=alpha)   
##   as.data.frame(t(res))
## }   
## 


###################################################
### code chunk number 15: sim_binomial_prop.Rnw:178-186
###################################################
ci_wilson <- function(p.hat, n, alpha=.05){
  c <- qnorm(1 - alpha/2)
  pm <- c* sqrt(p.hat * (1 - p.hat)/n + c^2/(4*n^2))   
  c(p.hat= p.hat,
    lower= 1 / (1 + c^2/n) * (p.hat + c^2/(2*n) - pm),
    upper= 1 / (1 + c^2/n) * (p.hat + c^2/(2*n) + pm))
} 
ci_wilson(p.hat=.05, n=500)


###################################################
### code chunk number 16: sim_binomial_prop.Rnw:191-199
###################################################
ci <- function(p.hat, n, alpha=.05, method="standard"){
  method <- match.arg(method, c("standard", "wilson"))  
  if (method == "standard")
    res <- ci_standard(p.hat, n, alpha)
  if (method == "wilson")       
    res <- ci_wilson(p.hat, n, alpha)
  res
}   


###################################################
### code chunk number 17: sim_binomial_prop.Rnw:204-233
###################################################
sim_one_ci <- function(n, p, alpha=.05, 
                        method="standard"){
  vals <- rbinom(n, 1, p)                # Stichprobe ziehen 
  p.hat <- mean(vals)                    # geschätzter Anteil 
  cis <- ci(p.hat, n, alpha, method)     # KI berechnen
  lower <- cis["lower"]                  # unteres KI
  upper <- cis["upper"]                  # oberes KI  
  c(p.hat=p.hat,                         # Punktschätzung
    lower= lower, upper= upper,          # KI ausgeben   
    covered= lower <= p && p <= upper)   # liegt p im Intervall?
}

sim_many_ci <- function(reps, n, p, alpha=.05,
                          method="standard"){
  res <- replicate(reps, sim_one_ci(n, p, alpha, method))
  prop.covered <- mean(res["covered", ])
  c(p=p, n=n, prop.covered=prop.covered)                   
} 

p_study <- function(reps, n, alpha=.05, method="standard",
                      p.start=.01, p.end=.99, p.step=.1){
  method <- match.arg(method, c("standard", "wilson"))  
  ps <- seq(p.start, p.end, p.step)                     
  res <- mapply(sim_many_ci, reps=reps,
                n=n, p=ps, alpha=alpha, method=method)   
  df <- as.data.frame(t(res))
  df$method <- method
  df
} 


###################################################
### code chunk number 18: sim_binomial_prop.Rnw:238-241 (eval = FALSE)
###################################################
## x <- p_study(100000, 100, .05, p.step=.01, method="wilson")
## plot(x[c("p","prop.covered")], type="l", las=1) 
## abline(h=.95, col="red")


###################################################
### code chunk number 19: sim_binomial_prop.Rnw:244-246 (eval = FALSE)
###################################################
## x <- p_study(100000, 100, .05, p.step=.01, method="wilson")
## write.csv2(x, "data/p_study_wilson_p05_reps100000.csv")


###################################################
### code chunk number 20: sim_binomial_prop.Rnw:249-252
###################################################
x <- read.csv2("data/p_study_wilson_p05_reps100000.csv")
plot(x[c("p","prop.covered")], type="l", las=1) 
abline(h=.95, col="red")


###################################################
### code chunk number 21: sim_binomial_prop.Rnw:262-270 (eval = FALSE)
###################################################
## res <- list()
## ns <- 10:100
## for (i in seq_along(ns)){
##   cat(paste("\r", "run", i, "of", length(ns)))
##   res[[i]] <- p_study(reps=1000, n=ns[i], p.step=.001, 
##                       method="standard")
## } 
## x <- do.call(rbind, res)


###################################################
### code chunk number 22: sim_binomial_prop.Rnw:277-280
###################################################
x <- read.csv2("data/p_study_standard_pstep_001_n10_100_reps_10000.csv")
#x <- read.csv2("data/p_study_standard_05_n100_reps1000.csv")
#x <- read.csv2("data/p_study_standard_05_n30_reps1000.csv")


###################################################
### code chunk number 23: sim_binomial_prop.Rnw:285-287 (eval = FALSE)
###################################################
## library(lattice)
## levelplot(prop.covered ~ n * p, data=x)


###################################################
### code chunk number 24: sim_binomial_prop.Rnw:292-297
###################################################
library(gplots)
levels <- seq(.8, 1, by=.05)
colors <- c(colorpanel(4, "darkred", "red"), "lightgreen")
levelplot(prop.covered ~ n * p, data=x, 
           at=levels, col.regions=colors)            


