set.seed(123)

nExp = 1000
p = 4
n = 200
gamma = 0

taudf = matrix(0, nrow = nExp, ncol = p)
vifdf = matrix(0, nrow = nExp, ncol = p)

for (l in 1:nExp){
  w = matrix(rnorm(p*n,10,100),n,p)
  x = sqrt(1-gamma^2)*w + gamma*w[,p]
  z = apply(x, 2, function(col){
    # res = (col - mean(col))/sd(col)
    tmp = col - mean(col)
    res = tmp/sqrt(sum(tmp^2))
    return(res)
  })
  apply(z, 2, mean)
  apply(z, 2, var)
  ztz_inv = solve(t(z) %*% z)
  (vif = diag(ztz_inv))
  svd_obj = svd(ztz_inv)
  (taus = svd_obj$d)
  (svd_obj$u^2 %*% taus) ## Equal to vif by theory
  
  taudf[l,] = sort(taus)
  vifdf[l,] = sort(vif)
}


plot(taudf[, 4], vifdf[, 4])
cor(taudf[, 4], vifdf[, 4])^2
