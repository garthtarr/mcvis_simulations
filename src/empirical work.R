  v1=vif1=NULL
  n=200
  p=4
  for (l in 1:1000){
    
    w=matrix(rnorm(p*n,10,100),n,p)
    gamma=0.5

        x=sqrt(1-gamma^2)*w+gamma*w[,p]
        n1=as.matrix(rep(1,n))
        X2<-x-n1%*%colMeans(x)
        s<-as.matrix(sqrt(diag(t(X2)%*%X2)))
        x.norm<-as.matrix(sqrt(diag(t(x)%*%x)))
        Z<-X2[,1]/s[1,]
        for (j in 2:p)   { Z<-as.matrix(cbind(Z,X2[,j]/s[j,])) }

        v<-as.vector(s/x.norm)
        D<-diag(v)
        Z1<-Z%*%D
        v1p=1/eigen(crossprod(Z1,Z1))$values
        vifp=diag(solve(t(Z1)%*%Z1))
        vifp=vifp[order(vifp)]

        v1<-cbind(v1,v1p)
        vif1<-cbind(vif1,vifp)
  }

  par(mfrow = c(p, p),mar = c(4, 4, 0.5, 0.5),mgp=c(2,0.5,0))

    for (i in 1:p){
      for (j in 1:p) {
        plot(v1[i,],vif1[j,],xlab = paste("??",i,sep=""),ylab = paste("vif(",j,")",sep=""))
      }
    }
