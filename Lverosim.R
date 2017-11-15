
 rm(list=ls(all=TRUE)); ls()
 
 load('~/Documents/IFOP/Rworks/Verosim/Lverosim.RData')
 
 dir.1='~/Documents/IFOP/Rworks/Verosim'; dir(dir.1)
 dir.2='~/Documents/IFOP/Rworks/Functions_Work'; dir(dir.2)
 #str.1 <- 'mcomun';

 source(paste(dir.2, 'functions.R', sep='/'))
 library(stringr)
 library(R2admb)

 dat.file=  'modbentoLL.dat'
 model='modbentoLL'
 data.0 <- lisread(paste(dir.1, 1, dat.file, sep='/'))
 names(data.0) <-  str_trim(names(data.0), side="right")
 data.1 <- data.0
 names(data.1)

 #ctl.file='ctrl_t.dat'
 #ctl.0 <- lisread(paste(dir.1, 1, ctl.file, sep='/'))
 #str.ctl=basename(file_path_sans_ext(ctl.file))
 #names(ctl.0) <-  str_trim(names(ctl.0), side="right")
 #ctl.1 <- ctl.0
 #names(ctl.1)

 setwd(paste(dir.1, 1, sep='/'));  getwd()
 run_admb(model)
 results <- reptoRlist(paste(substr(model, start=1, stop=10) ,'.rep', sep=''))
 names(results)
 #frac <- c(.2, .4, .6, .8, 1, 1.2, 1.4, 1.6, 1.8)
 #data.1[[12]]
 #log.Ro <- c(log.Ro[1],log.Ro)
 step <- c(0.5, 0.6, 0.7, 0.8)
 h <- c(data.1$`h`,step)
 casos <- length(h)
 
 #ro<-1*step
 #ro<-c(ro[1],ro)
 
 
 #casos <- length(log.Ro)
 #yrs <- dim(data.0$indices)[1]
 #ll <- length(ctl.0$lambda)
 #phase_R <- c(ctl.0$phase_R, rep(-1*ctl.0$phase_R, casos-1))

 #phase_R <- c(1, rep(-1*1, casos-1))

for(i2 in 2:casos){

 #ctl.1 <- ctl.0
 data.1 <- data.0

        system(paste('mkdir', paste(dir.1, i2, sep='/'), sep=' '))
        system(paste('cp', paste(dir.1, 1, model, sep='/'), paste(dir.1,i2, sep='/'), sep=' '))

 data.1$`h` <- h[i2]
 #ctl.1$phase_R <- phase_R[i2]


 #writeData(paste(dir.1, i2, ctl.file, sep='/'), ctl.1, append=FALSE)
 writeData(paste(dir.1, i2, dat.file, sep='/'), data.1, append=FALSE)

}

 save.image('~/Documents/IFOP/Rworks/Verosim/Lverosim.RData')


for(i2 in 2:casos){

#lon.like <- 30
if(i2 == 2){

 BD <- mat.or.vec(nr=1,nc=casos)
 objF <- mat.or.vec(nr=1,nc=casos)
 #Like <- mat.or.vec(nr=lon.like, nc=casos)
 #Rcero <- mat.or.vec(nr=1, nc=casos)
 }

 print(paste('Running case: ', i2,sep=''))
 setwd(paste(dir.1, i2, sep='/'));
 run_admb(model)
 results <- reptoRlist(paste(substr(model, start=1, stop=10) ,'.rep', sep=''))
 #print(paste('Maximum gradient = ', results$gmax ,sep=''))

 BD[i2] <- results$BD
 #Like[,i2] <- results$logLike
 objF[i2] <- results$likeval
 #Rcero[i2] <- exp(results$log_Ro)
}


# almacenamiento de resultados

 outp <- list()
 outp$Like  <- Like
 outp$gMax  <- gMax
 outp$objF  <- objF
 outp$Rcero  <- Rcero

 save(outp, file=paste(dir.1,'verosim_2.Rda', sep='/'))
 load(paste(dir.1,'verosim_2.Rda', sep='/'));


# Tabla
 options(scipen=999)
 tab1 <- round(t(Like), 2)
 write.table(tab1, file=paste(dir.1,'tabla1.txt', sep='/'), sep=',')




                                        #GRAPHICS
dim(outp$Like)

colores <- c('#000000', '#6495ED', '#EE7621', '#458B00', '#483D8B', '#FFB90F', '#00008B','#9AFF9A', '#EE0000', '#6E7B8B', '#EEE685', '#1874CD', '#00FFFF', '#8B0A50', '#CD8500', '#4A708B','#FF00FF','#8B4726','#32CD32', '#CD5555' ,'#CDCDC1', '#668B8B')

plot(2:casos,outp$Like[1,2:10]-min(outp$Like[1,2:10]), type='l')
for(i2 in 3:casos){
    lines(2:casos,outp$Like[i2,2:10]-min(outp$Like[i2,2:10]), type='l', lty=1, col=colores[i2])
}


