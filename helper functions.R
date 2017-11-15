library(lattice) 


coh2arr <- function(object){
  # dimensions and co
  dobj <- dim(object)
  dflq <- dobj[1:2]
  dflq[2] <- dobj[2]-dobj[1]+1
  flq <- array(NA, dim=dflq)
  for(i in 1:dflq[1]) flq[i,] <- object[i,(dobj[1]-i+1):(dobj[2]-i+1)]
  return(flq)
}
 
arr2coh <- function(object){ 
  # dimensions and co
  dobj <- dim(object)
  dflq <- dobj
  dflq[2] <- dobj[2] + dobj[1] - 1
  flq <- array(NA, dim = dflq)
  for(i in 1:dflq[1]) flq[i,(dobj[1] - i + 1):(dflq[2] + 1 - i)] <- object[i,]
  return(flq)
} 

bubbles <- function(x, size=1){
  print(xyplot(as.numeric(as.character(age))~ind, cex=size*abs(x/max(x)),data=cbind(stack(data.frame(x)),age= rep(dimnames(x)$age,ncol(x))),pch=20,col=(x < 0)+1, xlab="Year", Ylab="Age"))
}

cc <- function(x, xlim=range(as.numeric(dimnames(x)$year))){
  flq <- arr2coh(x)
  dimnames(flq)<- list(age=dimnames(x)$age, year=(min(as.numeric(dimnames(x)$year))-length(dimnames(x)$age)+1):max(dimnames(x)$year)) 
  flqdf <- cbind(stack(data.frame(flq)) ,age= rep(dimnames(x)$age,ncol(flq)))
  flqdf <- cbind(flqdf , year=rep(as.numeric(dimnames(x)$age),ncol(flq)) + as.numeric(substr(flqdf$ind,2,5)))
  print(xyplot(log(values)~year, group=ind, data=flqdf, type="b", xlim=xlim))
} 
 
internal <- function(x){
  oldpar <- par()
  par(mfrow=c(4,4),mar=c(4,4,0,0.1), mgp=c(2,0.5,0),tck=-0.02)
  flq <- log(arr2coh(x))
  dimnames(flq) <- list(age=dimnames(x)$age, year=(min(as.numeric(dimnames(x)$year))-length(dimnames(x)$age)+1):max(dimnames(x)$year)) 
  for (ii in 1:14){
    plot(flq[ii,],flq[ii+1,], xlab=paste("Age ", ii),ylab=paste("Age ", ii+1))
    minflq1 <- min(flq[ii,], na.rm=T);maxflq1 <- max(flq[ii,], na.rm=T);minflq2 <- min(flq[ii+1,], na.rm=T);maxflq2 <- max(flq[ii+1,], na.rm=T); 
    text(minflq1+0.1*(maxflq1 - minflq1),minflq2+0.9*(maxflq2 - minflq2),round(cor(flq[ii,],flq[ii+1,], use= "complete.obs"),2)) }
  par <- oldpar  
}
