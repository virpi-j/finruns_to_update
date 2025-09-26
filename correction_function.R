
# quantile-quantile correction function
# 2021 functions are in Allas:
# https://a3s.fi/2000994-virpi-BioClima/quantile_data_2021.rdata


correction_f <- function(xi,ij,ecdfx=qMSNFI[[ij]],ecdfz=qFC[[ij]],
                         vari = names(qMSNFI)[ij],FIG=F){
  if(!is.na(xi)){
    ecdfy1 <- as.numeric(ecdfx$ecdf)
    ecdfy2 <- as.numeric(ecdfz$ecdf)
    ecdfx1 <- as.numeric(ecdfx$x)
    ecdfx2 <- as.numeric(ecdfz$x)
    if(FIG){
      xlims <- c(min(ecdfx1[1],ecdfx1[2]),max(c(ecdfx1,ecdfx2)))
      ylims <- c(-0.05,max(c(ecdfy1,ecdfy2)))
      plot(ecdfx1,ecdfy1,col="black",xlim=xlims, ylim=ylims,type="l",
           main=vari, xlab="x", ylab="ecdf")
      points(ecdfx2,ecdfy2,col="blue",pch=19,cex=.5)
      lines(ecdfx2,ecdfy2,col="blue")
      lines(xlims,c(0,0),col="black")
      lines(c(0,0),ylims,col="black")
    }
    # quantile value of xi in original MS-NFI data
    x0 <- ecdfx1[max(which(ecdfx1<=xi))]
    x1 <- ecdfx1[min(which(ecdfx1>xi))]
    r0 <- ecdfy1[max(which(ecdfx1<=xi))]
    r1 <- ecdfy1[min(which(ecdfx1>xi))]
    x1 <- x1[min(which(x1>xi))]
    r <- r0 + (xi-x0)/(x1-x0)*(r1-r0)
    if(x0==0){ 
      x1<-0
      r <- r1 <- r0*runif(1)
    }
    if(FIG){
      points(xi, r, col="green",pch=1)    
      points(xi,0,pch=19,col="green")
      lines(c(xi,xi),c(0,r),col="green")
    } 
    if(r==1){ n0 <- n1 <- length(ecdfx2)
    } else {
      n0 <- max(1, which(ecdfy2>r)[1]-1)
      nsame <- which(ecdfy2[n0]==ecdfy2) 
      n0 <- nsame[1]
      n1 <- n0 + length(nsame)
    }
    #      print(paste(xi,ij,n0,n1))
    y0 <- ecdfx2[n0]
    y1 <- ecdfx2[n1]
    if(n0==1){ yz <- y0
    } else if(n1==length(ecdfx2)){#nout){
      yz <- y1
    } else {
      yz <-y0+(r - ecdfy2[n0])/(ecdfy2[n1]-ecdfy2[n0])*(y1-y0)
    } 
    if(FIG){
      points(yz, r, col="red",pch=1)    
      lines(c(xi,yz),c(r,r),col="red")
      lines(c(yz,yz),c(0,r),col="red")
      points(yz,0,col="red",pch=19)
    } 
    
    if(ij==5) yz <- ceiling(yz)
  } else { yz=NA}
  return(yz)
}
