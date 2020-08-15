library(mcvis)
library(ggplot2)
set.seed(1)
p = 6
n = 100

X = matrix(rnorm(n*p), ncol = p)
X[,1] = X[,2] + X[,3] + rnorm(n, 0, 1)

# r2 = vector("numeric", p)
# 
# for(j in 1:p){
#   y = X[,j,drop = FALSE]
#   r2[j] = summary(lm(y ~ X[,-j]))$r.squared
# }
# names(r2) = colnames(X)
# (vif_mcvis = 1/(1-r2))

Z = scale(X)
crossprodZ = t(Z) %*% Z
svd_obj = svd(crossprodZ)
tau = 1/svd_obj$d
lambda = svd_obj$d
g = svd_obj$u
all.equal(t(g) %*% t(Z) %*% Z %*% g, diag(lambda)) ## Expect true
all.equal(vif_mcvis, 
          as.vector((n - 1) * ((g^2) %*% tau))) ## Expect true

det((g^2)) ## determinant is usually a very small value that implies this g^2 matrix is not invertible
