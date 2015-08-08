
## ----echo=FALSE, warning=FALSE, message=FALSE----------------------------
library(knitr)
library(mat2tex)
library(scales)  # for percent
library(plotrix)


## ----gen-data, eval=FALSE, echo=FALSE------------------------------------
## cn <- c("Oak-type", "fruity1", "woody1", "coffee1", "redfruit2", "roasted2", "vanillin2", "woody2", "fruity3", "butter3", "woody3")
## 
## x1 <- matrix(c(
## 1, 6, 7,
## 5, 3, 2,
## 6, 1, 1,
## 7, 1, 2,
## 2, 5, 4,
## 3, 4, 4), byrow=T, nrow=6)
## colnames(x1) <- cn[2:4]
## 
## x2 <- matrix(c(
## 2, 5, 7, 6,
## 4, 4, 4, 2,
## 5, 2, 1, 1,
## 7, 2, 1, 2,
## 3, 5, 6, 5,
## 3, 5, 4, 5), byrow=T, nrow=6)
## colnames(x2) <- cn[5:8]
## 
## x3 <- matrix(c(
## 3, 6, 7,
## 4, 4, 3,
## 7, 1, 1,
## 2, 2, 2,
## 2, 6, 6,
## 1, 7, 5), byrow=T, nrow=6)
## colnames(x3) <- cn[9:11]
## 
## oaktype <- c(1,2,2,2,1,1)
## expert <- c("", 1,1,1,2,2,2,2,3,3,3)
## 
## x <- cbind(oaktype, x1, x2, x3)
## x <- rbind(expert, x)
## 
## rownames(x)[2:7] <- paste("wine", 1:6)
## write.csv(x, "../data/mfa.csv", row.names=T)


## ----echo=FALSE----------------------------------------------------------
normalize <- function (v) {
  v <- v - mean(v)
  v / sqrt(sum(v^2))
}

x <- read.csv("../data/mfa.csv", header = TRUE)

rownames(x) <- x[, 1]
x <- x[, -1]

R1 <- x[-1, 2:4]
R2 <- x[-1, 5:8]
R3 <- x[-1, 9:11]

X1 <- apply(R1, 2, normalize)
X2 <- apply(R2, 2, normalize)
X3 <- apply(R3, 2, normalize)

I <- ncol(X1) + ncol(X2) + ncol(X3)
I <- nrow(X1)


## ----echo=FALSE, results='asis'------------------------------------------
kable(x, format = "markdown")


## ----echo=FALSE, results='asis'------------------------------------------
xx("X_{1}=", X1)


## ----echo=FALSE, results='asis'------------------------------------------
xx("X_{2}=", X2)


## ----echo=FALSE, results='asis'------------------------------------------
xx("X_{3}=", X3)


## ------------------------------------------------------------------------
M <- diag(I) / I


## ----echo=FALSE, results='asis'------------------------------------------
xx("\\mathbf{M} = ", M)


## ------------------------------------------------------------------------
standardize_matrix <- function(x) x / svd(x)$d[1] 
Z1 <- standardize_matrix(X1)
Z2 <- standardize_matrix(X2)
Z3 <- standardize_matrix(X3)


## ----echo=FALSE, results='asis'------------------------------------------
xx("\\mathbf{Z_1} = \\varphi_1^{-1} \\; \\mathbf{X_1} = ", Z1)


## ------------------------------------------------------------------------
Z <- cbind(Z1, Z2, Z3)


## ----echo=FALSE, results='asis'------------------------------------------
xx("\\mathbf{Z} = (\\mathbf{Z_1} \\; \\mathbf{Z_2} \\; \\mathbf{Z_3}) = ", lb(), Z)


## ------------------------------------------------------------------------
dec <- svd(Z)
U <- dec$u     # or as eigendecomposition  eigen(Z %*% t(Z))
d <- dec$d
D <- diag(d)
V <- dec$v


## ----echo=FALSE, results='asis'------------------------------------------
xx("\\mathbf{U} =", U)


## ----echo=FALSE, results='asis'------------------------------------------
xx("diag \\{ \\mathbf{\\Delta} \\} = ", t(diag(D)))


## ----echo=FALSE, results='asis'------------------------------------------
xx("diag \\{ \\mathbf{\\Delta^2} \\} = ", t(diag(D))^2)


## ----echo=FALSE, results='asis'------------------------------------------
xx("\\mathbf{V} =", V)


## ------------------------------------------------------------------------
S = diag(diag(M)^(-1/2)) %*% U %*% D
colnames(S) <- paste0("GPC", 1:length(d))   # names global PC
S <- S[, -6]        # kill zero column


## ----echo=FALSE, results='asis'------------------------------------------
xx("\\mathbf{S} = \\mathbf{M^{-\\frac{1}{2}}U \\Delta} = ", S)


## ------------------------------------------------------------------------
con <- round(d^2 / sum(d^2), 5)
k <- data.frame(PC=seq_along(d), contribution=percent(con))


## ----, echo=FALSE, results='asis'----------------------------------------
kable(k)


## ----global-space--------------------------------------------------------
plot(S, xlab = "Global PC1", ylab = "Global PC2", pch=16, col="blue", 
     asp=1, cex=1, main="Six wines in the global space")
abline(h=0, v=0, col="grey")
text(S, labels=1:6, pos=4)


## ------------------------------------------------------------------------
P = diag(diag(M)^(-1/2)) %*% U %*% diag(diag(D)^-1)[,-6]  # projection matrix, kill zero column


## ----, echo=FALSE, results='asis'----------------------------------------
xx("\\mathbf{P} = ", P)


## ------------------------------------------------------------------------
N <- 3
S1 = N * (Z1 %*% t(Z1)) %*% P
S2 = N * (Z2 %*% t(Z2)) %*% P
S3 = N * (Z3 %*% t(Z3)) %*% P
colnames(S1) <- colnames(S2) <- colnames(S3) <- paste0("PC", 1:5)


## ----echo=FALSE, results='asis'------------------------------------------
xx("\\mathbf{S1} = ", S1)


## ----echo=FALSE, results='asis'------------------------------------------
xx("\\mathbf{S2} = ", S2)


## ----echo=FALSE, results='asis'------------------------------------------
xx("\\mathbf{S3} = ", S3)


## ----global-space-plus-experts-------------------------------------------
S.all <- rbind(S, S1, S2, S3)
plot(S.all, xlab = "Global PC1", ylab = "Global PC2", type="n",
     asp=1, main="Experts projected into the global space")
abline(h=0, v=0, col="grey")

points(S, pch=16, col="blue", cex=1.5)
text(S, labels=1:6, pos=4)

ss <- list(S1, S2, S3)
for (i in seq_along(ss)) {
  s <- ss[[i]]
  points(s, pch=18, col=i)
  for (j in 1:6)
    segments(S[j,1], S[j,2], s[j,1], s[j,2], col=i)
}


## ------------------------------------------------------------------------
R.all = cor(cbind(S[, 1:3], X1, X2, X3))    # calc all loadings
L = t(R.all[4:13, 1:3])                     # variable loadings on first 3 GPCs

# or a bit simpler to calculate using matrix multiplication 
# (as scores in U are already normalized)
L = t(U[, 1:3]) %*% cbind(X1, X2, X3)
rownames(L) <- paste0("GPC", 1:3) 


## ----echo=FALSE, results='asis'------------------------------------------
kable(L, digits=2)


## ------------------------------------------------------------------------
# compute factor scores for PCA of every study
d1 <- svd(Z1)
d2 <- svd(Z2)
d3 <- svd(Z3)
P1 = d1$u %*% diag(d1$d)
P2 = d2$u %*% diag(d2$d)
P3 = d3$u %*% diag(d3$d)

# compute loadings, i.e. correlations with global factor scores
L2 = cor(cbind(S[, 1:3], P1[, 1:2], P2[, 1:2], P3[, 1:2]))
L2 = L2[1:3, -(1:3)]

# as the multiplication of U with the singular values is a linear 
# transormation that will be reverted when calculating a correlations, 
# we may simply use the unscaled values of U directly instead of UD

tUZ = t(svd(Z)$u[, 1:3])   # overall unscaled values for GPC 1-3
L2 = tUZ %*% cbind(d1$u[, 1:2], d2$u[, 1:2], d3$u[, 1:2])
L2 = L2 * -1               # see note 2
rownames(L2) <- paste0("GPC", 1:3)
colnames(L2) <- paste0("Exp-", rep(1:3, each=2), "-PC", rep(1:2, 3))


## ----echo=FALSE, results='asis'------------------------------------------
kable(L2, digits=2)


## ----circle-of-correlations, fig.width=7, fig.height=2.5-----------------
par(mfrow=c(1,3), mar=c(0,0,1,3))
for (i in 1:3) {
  plot(c(-1,1), c(-1,1), type="n", asp=1, frame.plot = F, 
       xaxt="n", yaxt="n", xlab="", ylab="", main=paste("Study", i))
  draw.circle(0,0,1, lwd=2)
  
  # add global PCs
  segments(c(-1,0), c(0,-1), c(1,0), c(0,1), col="blue", lwd=2)
  text(0, .8, "GPC1", col="blue", cex=.9, pos=3)
  text(.8, 0, "GPC2", col="blue", cex=.9, pos=3)
  
  # add variable loadings in global space
  P = t(L[1:2, grep(i, colnames(L))])
  points(P, pch=16, col="red", cex=1.3)
  text(P, labels=rownames(P), cex=.9, pos=4, xpd=T)
  
  # add study PC loadings
  G = t(L2[1:2, grep(paste0("-", i), colnames(L2))])
  points(G, pch=15, col="darkgreen", cex=1.3)
  text(G, labels=paste0("PC", 1:2), cex=.9, pos=2, col="darkgreen")
}


## ------------------------------------------------------------------------
In = V^2 %*% D^2                  # sum squared values for each study multiplied by eigenvalue
e <- rep(1:3, c(3,4,3))           # experts
colnames(In) <- paste("Axis", 1:6)
a <- by(In, e, colSums)
a <- do.call(rbind, a)
a <- rbind(a, colSums(a))[, -6]   # remoce axis 6 as eigenvalue is zero
rownames(a) <- c(paste("Expert", 1:3), "Eigenvalues (Lamda)")


## ----echo=FALSE, results='asis'------------------------------------------
kable(a, digits=2)


