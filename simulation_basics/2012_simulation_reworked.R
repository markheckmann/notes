### R code from vignette source '/Users/markheckmann/Desktop/2012_simulation_reworked/2012_simulation_reworked.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: 2012_simulation_reworked.Rnw:62-64
###################################################
options(width=75)         # set width to 60 characters
options(continue=" ")     # remove continuation prompt


###################################################
### code chunk number 2: 2012_simulation_reworked.Rnw:101-115
###################################################
# estimating Pi
set.seed(1)
library(plotrix)
plot(0, type="n", xlim=c(-1, 1), ylim=c(-1, 1), xaxt="n", yaxt="n", 
    bty="n", asp=1, xaxs="i", yaxs="i", ylab="", xlab="pi = 3.14")
lines(c(1,-1), c(0,0))
lines(c(0,0), c(1,-1))
rect(-1, -1, 1, 1)
draw.circle(0,0,1, border="red") 
# simulate some points 
n <- 200
x <- runif(n, -1, 1)
y <- runif(n, -1, 1) 
points(x,y, pch=16, cex=.5)  


###################################################
### code chunk number 3: 2012_simulation_reworked.Rnw:118-132
###################################################
# points inside the circle.
n <- 10e4
x <- runif(n, -1, 1)
y <- runif(n, -1, 1) 
z <- x^2 + y^2
system.time({
  n.inside <- sum(z <= 1)    # root is not necessary as it will not change anything
  pis <-  n.inside * 4 / n
}) 
system.time({
  n.inside <- length(z[z <= 1])    # root is not necessary as it will not change anything
  pis <-  n.inside * 4 / n
}) 
pis


###################################################
### code chunk number 4: 2012_simulation_reworked.Rnw:136-154
###################################################
# version 1
mcpi_1 <- function(n){
  x <- runif(n, -1, 1)
  y <- runif(n, -1, 1) 
  z <- x^2 + y^2
  pis <- sum(z <= 1) * 4 / n
  pis
} 
system.time({  
  mcpi_1(10e6)
}) 

mcpi_2 <- function(n){
  sum(runif(n, -1, 1)^2 + runif(n, -1, 1)^2 <= 1) * 4 / n
} 
system.time({  
  mcpi_2(10e6)
}) 


###################################################
### code chunk number 5: 2012_simulation_reworked.Rnw:169-204
###################################################
# ok
coin_runs <- function(n){
  x <- rbinom(n, 1, .5)
  re <- rle(x)$length
  c(flips=length(re) - 1, max=max(re))  
}
coin_runs(100)

# repeat a large number of times  
# using a for loop
do_coin_runs <- function(reps, n=100){
  res <- matrix(NA, reps, 2)
  for (i in 1:reps)
    res[i, ] <- coin_runs(n)
  res <- as.data.frame(res)
  names(res) <- c("flips", "max")
  res
}
do_coin_runs(10)  

# much simpler: using replicate  
do_coin_runs_2 <- function(reps, n=100){
  as.data.frame(t(replicate(reps, coin_runs(n))))
} 
x <- do_coin_runs_2(10000) 
colMeans(x)

do_coin_plot <- function(df){ 
  x <- df[, 1]
  y <- df[, 2]
  plot(jitter(x), jitter(y), pch=16, cex=.2, col="darkblue", 
       xlab="flips", ylab="longest run") 
  grid()
} 



###################################################
### code chunk number 6: 2012_simulation_reworked.Rnw:207-211
###################################################
par(mfrow=c(1,2))
do_coin_plot(x)  
library(geneplotter)
smoothScatter(x)


###################################################
### code chunk number 7: 2012_simulation_reworked.Rnw:233-237
###################################################
ci_standard <- function(p.hat, n, alpha=.05){
  qnorm(1 - alpha/2) * sqrt(p.hat * (1 - p.hat) / n) 
}  
ci_standard(p.hat=.05, n=500)


###################################################
### code chunk number 8: 2012_simulation_reworked.Rnw:244-245
###################################################
set.seed(1)


###################################################
### code chunk number 9: 2012_simulation_reworked.Rnw:248-260
###################################################
sim_one_ci <- function(n, p, alpha=.05){
  vals <- rbinom(n, 1, p)                # Stichprobe ziehen 
  p.hat <- mean(vals)                    # geschätzter Anteil 
  ci <- ci_standard(p.hat, n, alpha)     # KI berechnen
  lower <- p.hat - ci                    # oberes KI            
  upper <- p.hat + ci                    # unteres KI
  c(p.hat=p.hat,                         # 
    lower= lower, upper= upper,          # KI ausgeben   
    covered= lower <= p && p <= upper)   # liegt p im Intervall?
}

sim_one_ci(n=100, p=.1, alpha=.05)


###################################################
### code chunk number 10: 2012_simulation_reworked.Rnw:265-275
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
### code chunk number 11: 2012_simulation_reworked.Rnw:280-281
###################################################
sim_many_ci(reps=1000, n=100, p=.1)  


###################################################
### code chunk number 12: 2012_simulation_reworked.Rnw:286-287
###################################################
sim_many_ci(reps=10000, n=100, p=.1)  


###################################################
### code chunk number 13: 2012_simulation_reworked.Rnw:292-293
###################################################
sim_many_ci(reps=10000, n=100, p=.05)  


###################################################
### code chunk number 14: 2012_simulation_reworked.Rnw:298-312
###################################################
p_study <- function(reps, n, alpha=.05,
                    p.start=.01, p.end=.99, p.step=.1){  
  ps <- seq(p.start, p.end, p.step)                   
  rmat <- matrix(0, nrow=length(ps), ncol=3)    # Ergebnismatrix
  for (i in 1:length(ps)){                      # Durchlauf für jedes p
    p <- ps[i]                                  # neuer p Wert
    rmat[i, ] <- sim_many_ci(reps, n, p, alpha) # Ziehungen simulieren 
    cat("\r", i, "of", length(ps)) 
    flush.console()
  } 
  res <- as.data.frame(rmat)                   
  names(res) <- c("p", "n", "prop.covered")     # Spalten benennen 
  res
} 


###################################################
### code chunk number 15: 2012_simulation_reworked.Rnw:317-318 (eval = FALSE)
###################################################
## x <- p_study(1000, 100, .05, p.step=.005)


###################################################
### code chunk number 16: 2012_simulation_reworked.Rnw:320-321 (eval = FALSE)
###################################################
## write.csv2(x, "data/p_study_standard_p05_reps1000.csv")


###################################################
### code chunk number 17: 2012_simulation_reworked.Rnw:323-324
###################################################
x <- read.csv2("data/p_study_standard_p05_reps_1000.csv")


###################################################
### code chunk number 18: 2012_simulation_reworked.Rnw:326-328
###################################################
plot(x[c("p","prop.covered")], type="l") 
abline(h=.95, col="red")


###################################################
### code chunk number 19: 2012_simulation_reworked.Rnw:331-332 (eval = FALSE)
###################################################
## x <- p_study(100000, 100, .05, p.step=.005)


###################################################
### code chunk number 20: 2012_simulation_reworked.Rnw:334-335 (eval = FALSE)
###################################################
## write.csv2(x, "data/p_study_standard_p05_reps100000.csv")


###################################################
### code chunk number 21: 2012_simulation_reworked.Rnw:337-338
###################################################
x <- read.csv2("data/p_study_standard_p05_reps_100000.csv")


###################################################
### code chunk number 22: 2012_simulation_reworked.Rnw:340-342
###################################################
plot(x[c("p","prop.covered")], type="l") 
abline(h=.95, col="red")


###################################################
### code chunk number 23: 2012_simulation_reworked.Rnw:349-363 (eval = FALSE)
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
### code chunk number 24: 2012_simulation_reworked.Rnw:377-384
###################################################
ci_wilson <- function(p.hat, n, alpha=.05){
  c <- qnorm(1 - alpha/2)
  pm <- c* sqrt(p.hat * (1 - p.hat)/n + c^2/(4*n^2))   
  c(p.hat=p.hat,
    lower=1 / (1 + c^2/n) * (p.hat + c^2/(2*n) - pm),
    upper=1 / (1 + c^2/n) * (p.hat + c^2/(2*n) + pm))
}


###################################################
### code chunk number 25: 2012_simulation_reworked.Rnw:389-397
###################################################
ci <- function(p.hat, n, alpha, method="standard"){
  method <- match.arg(method, c("standard", "wilson"))  
  if (method == "standard")
    res <- ci_standard(p.hat, n, alpha)
  if (method == "wilson")       
    res <- ci_wilson(p.hat, n, alpha)
  res
}   


###################################################
### code chunk number 26: 2012_simulation_reworked.Rnw:402-431
###################################################
sim_one_ci <- function(n, p, alpha=.05, 
                        method="standard"){
  vals <- rbinom(n, 1, p)                # Stichprobe ziehen 
  p.hat <- mean(vals)                    # geschätzter Anteil 
  ci <- ci(p.hat, n, alpha, method)      # KI berechnen
  lower <- p.hat - ci                    # oberes KI            
  upper <- p.hat + ci                    # unteres KI
  c(p.hat=p.hat,                         # 
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
### code chunk number 27: 2012_simulation_reworked.Rnw:436-439
###################################################
x <- p_study(1000, 100, .05, p.step=.01, method="wilson")
plot(x[c("p","prop.covered")], type="l", las=1) 
abline(h=.95, col="red")


###################################################
### code chunk number 28: 2012_simulation_reworked.Rnw:447-455 (eval = FALSE)
###################################################
## res <- list()
## ns <- 10:40
## for (i in seq_along(ns)){
##   cat(paste("\r", "run", i, "of", length(ns)))
##   res[[i]] <- p_study(reps=1000, n=ns[i], p.step=.001, 
##                       method="standard")
## } 
## x <- do.call(rbind, res)


###################################################
### code chunk number 29: 2012_simulation_reworked.Rnw:461-462
###################################################
x <- read.csv2("data/p_study_standard.csv")


###################################################
### code chunk number 30: 2012_simulation_reworked.Rnw:467-469 (eval = FALSE)
###################################################
## library(lattice)
## levelplot(prop.covered ~ n * p, data=x)


###################################################
### code chunk number 31: 2012_simulation_reworked.Rnw:474-479
###################################################
library(gplots)
levels <- seq(.8, 1, by=.05)
colors <- c(colorpanel(4, "red", "darkred"), "lightgreen")
levelplot(prop.covered ~ n * p, data=x, 
           at=levels, col.regions=colors) 


###################################################
### code chunk number 32: 2012_simulation_reworked.Rnw:506-509
###################################################
choice <- sample(1:3, 1)
change <- sample(0:1, 1)



###################################################
### code chunk number 33: 2012_simulation_reworked.Rnw:528-553
###################################################
# prisoner walk
prisoner <- function(p=c(1/3, 1/3, 1/3), penalty=c(0,2,5)){
  penalty.sum <- 0
  door <- 0
  while(door != 1){
    door <- sample(1:length(p), 1)
    penalty.sum <- penalty.sum + penalty[door]    
  }
  penalty.sum  
}      
prisoner()

sim_prisoner <- function(reps=1000, p=c(1/3, 1/3, 1/3), 
                         penalty=c(0,2,5), do.plot=TRUE){
  res <- rep(NA, reps)
  for (i in 1:reps)
    res[i] <- prisoner(p, penalty)
  mn <- mean(res)    
  if (do.plot){   
    h <- hist(res, col="lightgrey", xlab="Hours", breaks=15)
    abline(v=mn, col="red")
    text(mn, max(h$counts)/2, paste("expected value=", mn), pos=4)  
  }
  res
}


###################################################
### code chunk number 34: 2012_simulation_reworked.Rnw:556-557
###################################################
dummy <- sim_prisoner() 


###################################################
### code chunk number 35: 2012_simulation_reworked.Rnw:567-569
###################################################
d <- rchisq(100, 3)
#bootstrap(d)


###################################################
### code chunk number 36: 2012_simulation_reworked.Rnw:577-611 (eval = FALSE)
###################################################
## # test
## library(plotrix) 
## 
## foo <- function(f, ...){  
##   dots <- list(...)
##   args <- clean.args(dots, f)	
## 	do.call(f, args)	
## } 
## 
## foo(rnorm, n=10)
## foo(rnorm, n=10, mean=2, sd=5)
## foo(runif, n=10, min=-4, nonsense=10) 
## 
## 
## library(plyr)
## 
## eval_function_call <- function(f){
##   args <- eval.quoted(f)
##   fun <- args[[1]]
##   if (!is.function(fun))
##     stop("first argument must be a valid R function")
##   args.cleaned <- clean.args(args[-1], fun)	
## 	do.call(fun, args.cleaned)  
## }
## 
## foo <- function(f1, f2){    
##   list(fun1=eval_function_call(f1),
##        fun2=eval_function_call(f2))
## } 
## 
## foo(.(rnorm, n=10, mean=5), .(runif, n=5, min=-4) ) 
## 
## 
## 


