## Graph BCI climate data
setwd("~/symlinks/git/tRack/")

## Test whether climate data was downloaded and cleaned
if(readRDS("./tests/DataDownLoad.rds")){

  png("./figures/BCIclimate.png",width=800,height=1200)
  ## load climate data
  if(!exists("BCIRain_historic")){
    load("./data/ClimateData.rdata") 
  }

  
  filenames <- list(
               "LutzTemp20m",
               "LutzRunoff",
               "LutzWindDirect",
               "BCIRain",
	       "BCIEvap"
              )
  ## Make a plot of the current and historic rainfall
  par(mfrow=c(3,2),mar=c(4,4,1,1),las=1,bg="grey10",cex.axis=1.4,
  cex.lab=1.4,fg="white",col.axis="white",col.lab="white")

  ## Combine data with historic rainfall

  temp <- get(filenames[[4]])

  days <- as.Date(temp[,1])
  daysHist <- as.Date(BCIRain_historic[,1],"%d/%m/%Y")

  daysFull <- c(daysHist,days[days>as.Date("1971-12-31")])
  rainFull <- c(BCIRain_historic[,2],temp[,2][days>as.Date("1971-12-31")])

  rain <- tapply(rainFull, format(daysFull, '%m-%Y'),sum)

  ## monthly rainfall
  days <- as.Date(paste(1,names(rain),sep="-"),"%d-%m-%Y")
  ord <- order(days)

  plot(days[ord],rain[ord],type="l",col=rgb(0,0,1,alpha=0.4),
       ,ylab="mm",xlab="Date",lwd=2,bty="l")
  coords <- par("usr")
  trend <- loess(rain~as.numeric(days))
  y <- predict(trend,se=TRUE)
  lines(days[ord],y$fit[ord],col="green",lwd=2)
  lines(days[ord],y$fit[ord]-2*y$se.fit[ord],col="green",lwd=1.5,lty=2)
  lines(days[ord],y$fit[ord]+2*y$se.fit[ord],col="green",lwd=1.5,lty=2)

  text(coords[2]*.15,coords[4]*0.5,"Monthly Rainfall",col=rgb(1,1,1,alpha=0.5),
       cex=3.5)

  text(coords[2]*.15,coords[4]*0.75,
       bquote(mu == .(round(mean(rain,na.rm=TRUE),2)))
       ,col=rgb(1,1,0,alpha=0.85), cex=2)
  abline(h=mean(rain,na.rm=TRUE),col=rgb(1,1,0,alpha=0.85),lty=2)

  grid(col="grey70")

  ## annuall rainfall
  rain <- tapply(rainFull, format(daysFull, '%Y'),sum)

  days <- as.numeric(names(rain))
  ord <- order(days)

  plot(days[ord],rain[ord],type="l",col=rgb(0,0,1,alpha=0.4),
       ,ylab="mm",xlab="Date",lwd=2,bty="l")
  coords <- par("usr")
  trend <- loess(rain~as.numeric(days))
  y <- predict(trend,se=TRUE)
  lines(days[ord],y$fit[ord],col="green",lwd=2)
  lines(days[ord],y$fit[ord]-2*y$se.fit[ord],col="green",lwd=1.5,lty=2)
  lines(days[ord],y$fit[ord]+2*y$se.fit[ord],col="green",lwd=1.5,lty=2)

  text(coords[2]-50,coords[4]*0.25,"Annual Rainfall",col=rgb(1,1,1,alpha=0.5),
       cex=3.5)

  text(coords[2]-50,coords[4]*0.75,
        bquote(mu == .(round(mean(rain[-length(rain)],na.rm=TRUE),2)))
       ,col=rgb(1,1,0,alpha=0.85), cex=2)
  abline(h=mean(rain[-length(rain)],na.rm=TRUE),col=rgb(1,1,0,alpha=0.85),lty=2)
  
  grid(col="grey70")


  ## Monthly Temp
  temp <- get(filenames[[1]])

  days <- as.Date(temp[,1])

  Temp <- tapply(temp[,2], format(days, '%m-%Y'),max)

  days <- as.Date(paste(1,names(Temp),sep="-"),"%d-%m-%Y")

  ord <- order(days)

  plot(days[ord],Temp[ord],type="l",col=rgb(0,0,1,alpha=0.4),
       ,ylab=degree ~ C,xlab="Date",lwd=2,bty="l")
  coords <- par("usr")
  trend <- loess(Temp~as.numeric(days))
  y <- predict(trend,se=TRUE)
  lines(days[ord],y$fit[ord],col="green",lwd=2)
  lines(days[ord],y$fit[ord]-2*y$se.fit[ord],col="green",lwd=1.5,lty=2)
  lines(days[ord],y$fit[ord]+2*y$se.fit[ord],col="green",lwd=1.5,lty=2)

  text(coords[2]*.65,coords[4]*0.95,"Monthly Max Temp",col=rgb(1,1,1,alpha=0.5),
       cex=3.5)
  text(coords[2]*.65,coords[4]*0.8,
        bquote(mu == .(round(mean(Temp,na.rm=TRUE),2)))
       ,col=rgb(1,1,0,alpha=0.85), cex=2)
  abline(h=mean(Temp,na.rm=TRUE),col=rgb(1,1,0,alpha=0.85),lty=2)
 
  grid(col="grey70")

  ## Annual max Temp
  temp <- get(filenames[[1]])

  days <- as.Date(temp[,1])

  Temp <- tapply(temp[,2], format(days, '%Y'),max)

  days <- as.numeric(names(Temp))

  ord <- order(days)

  plot(days[ord],Temp[ord],type="l",col=rgb(0,0,1,alpha=0.4),
       ,ylab=degree ~ C,xlab="Date",lwd=2,bty="l")
  coords <- par("usr")
  trend <- loess(Temp~as.numeric(days))
  y <- predict(trend,se=TRUE)
  lines(days[ord],y$fit[ord],col="green",lwd=2)
  lines(days[ord],y$fit[ord]-2*y$se.fit[ord],col="green",lwd=1.5,lty=2)
  lines(days[ord],y$fit[ord]+2*y$se.fit[ord],col="green",lwd=1.5,lty=2)

  text(coords[2]-15,coords[4]*0.95,"Annaul Max Temp",col=rgb(1,1,1,alpha=0.5),
       cex=3.5)

  text(coords[2]-15,coords[4]*0.9,
          bquote(mu == .(round(mean(Temp,na.rm=TRUE),2)))
       ,col=rgb(1,1,0,alpha=0.85), cex=2)

  abline(h=mean(Temp,na.rm=TRUE),col=rgb(1,1,0,alpha=0.85),lty=2)

  grid(col="grey70")

## Monthly Net rainfall
  temp <- get(filenames[[5]])

  days <- as.Date(temp[,1])

  Evap <- tapply(temp[,2], format(days, '%m-%Y'),max)
  rain<-rainFull 
  rain <- rain[daysFull<=max(days)&daysFull>=min(days)]
  rain <- tapply(rain, format(daysFull[daysFull<=max(days)&daysFull>=min(days)],
  '%m-%Y'),sum)
  
  days <- as.Date(paste(1,names(Evap),sep="-"),"%d-%m-%Y")
  raindays <- as.Date(paste(1,names(rain),sep="-"),"%d-%m-%Y")
  netRain<-data.frame(rain=rain,days=raindays)
  Evap<-data.frame(evap=Evap*10,days=days) 
  netRain <- merge(netRain,Evap,by="days")
  
  netRain <- netRain[order(netRain$days),]
  netRain$nr<-netRain$rain-netRain$evap

  plot(netRain$days,netRain$nr,type="l",col=rgb(0,0,1,alpha=0.4),
       ,ylab="mm",xlab="Date",lwd=2,bty="l")
  coords <- par("usr")
  trend <- loess(nr~as.numeric(days),data=netRain)
  y <- predict(trend,se=TRUE)
  
  lines(netRain$days,y$fit,col="green",lwd=2)
  lines(netRain$days,y$fit-2*y$se.fit,col="green",lwd=1.5,lty=2)
  lines(netRain$days,y$fit+2*y$se.fit,col="green",lwd=1.5,lty=2)

  text(coords[2]*.65,coords[4]*0.75,"Net rain",col=rgb(1,1,1,alpha=0.5),
       cex=3.5)
  text(coords[2]*.65,coords[4]*0.6,
        bquote(mu == .(round(mean(netRain$nr,na.rm=TRUE),2)))
       ,col=rgb(1,1,0,alpha=0.85), cex=2)
  abline(h=mean(netRain$nr,na.rm=TRUE),col=rgb(1,1,0,alpha=0.85),lty=2)
  abline(h=0,col=rgb(1,0,0,alpha=0.85),lty=2)

  grid(col="grey70")

## Net annual drought length
  netRain$D<-netRain$nr<=0
  Dry <- tapply(netRain$D, format(netRain$days, '%Y'),sum)
  days<-as.numeric(names(Dry))
  plot(days,Dry,type="l",col=rgb(0,0,1,alpha=0.4),
       ,ylab="months",xlab="Date",lwd=2,bty="l")
  coords <- par("usr")
  trend <- loess(Dry~as.numeric(days))
  y <- predict(trend,se=TRUE)
  
  lines(days,y$fit,col="green",lwd=2)
  lines(days,y$fit-2*y$se.fit,col="green",lwd=1.5,lty=2)
  lines(days,y$fit+2*y$se.fit,col="green",lwd=1.5,lty=2)

  text(coords[2]-15,coords[4]*0.75,"Dry months",col=rgb(1,1,1,alpha=0.5),
       cex=3.5)
  text(coords[2]-15,coords[4]*0.6,
        bquote(mu == .(round(mean(Dry,na.rm=TRUE),2)))
       ,col=rgb(1,1,0,alpha=0.85), cex=2)
  abline(h=mean(Dry,na.rm=TRUE),col=rgb(1,1,0,alpha=0.85),lty=2)
  abline(h=0,col=rgb(1,0,0,alpha=0.85),lty=2)

  grid(col="grey70")

dev.off()  
} else {error("Get Climate Process Failed")}


################################################################################
## Seasonal rhythems
################################################################################


if(readRDS("./tests/DataDownLoad.rds")){

  png("./figures/BCIseasons.png",width=1000,height=1000)
  ## load climate data
  if(!exists("BCIRain_historic")){
    load("./data/ClimateData.rdata") 
  }

  
  filenames <- list(
               "LutzTemp20m",
               "LutzRunoff",
               "LutzWindDirect",
               "BCIRain",
	       "BCIEvap"
              )
  ## Make a plot of the current and historic rainfall
  par(mfrow=c(2,2),mar=c(4,4,1,1),las=1,bg="grey20",fg="white",
  col.lab="white",col.axis="white",cex.lab=1.4,cex.axis=1.4)

  ## Combine data with historic rainfall

  temp <- get(filenames[[4]])

  days <- as.Date(temp[,1])
  daysHist <- as.Date(BCIRain_historic[,1],"%d/%m/%Y")

  daysFull <- c(daysHist,days[days>as.Date("1971-12-31")])
  rainFull <- c(BCIRain_historic[,2],temp[,2][days>as.Date("1971-12-31")])

  rain <- tapply(rainFull, format(daysFull, '%m'),mean)
  ndays <- tapply(format(daysFull, '%d-%m'), format(daysFull, '%m'),function(X)
   	   			   	     length(unique(X)))
  rain <- rain*ndays

  ## monthly rainfall
  days <- as.numeric(names(rain))
  ord <- order(days)

  plot(days[ord],rain[ord],type="l",col=rgb(0,0,1,alpha=0.4),
       ,ylab="mm",xlab="Month",lwd=2,bty="l")
  coords <- par("usr")
  trend <- loess(rain~as.numeric(days))
  y <- predict(trend,se=TRUE)
  lines(days[ord],y$fit[ord],col="green",lwd=2)
  lines(days[ord],y$fit[ord]-2*y$se.fit[ord],col="green",lwd=1.5,lty=2)
  lines(days[ord],y$fit[ord]+2*y$se.fit[ord],col="green",lwd=1.5,lty=2)

  text(coords[2]*.5,coords[4]*0.5,"Monthly Rainfall",col=rgb(1,1,1,alpha=0.5),
       cex=3.5)

  text(coords[2]*.15,coords[4]*0.75,
       bquote(mu == .(round(mean(rain,na.rm=TRUE),2)))
       ,col=rgb(1,1,0,alpha=0.85), cex=2)
  abline(h=mean(rain,na.rm=TRUE),col=rgb(1,1,0,alpha=0.85),lty=2)

  grid(col="grey70")

  ## Wind direction
  temp <- get(filenames[[3]])

  days <- as.Date(temp[,1])

  deg <- tapply(temp[,2], format(days, '%m'),median,na.rm=TRUE)

  ## monthly degfall
  days <- as.numeric(names(deg))
  ord <- order(days)

  plot(days[ord],deg[ord],type="l",col=rgb(0,0,1,alpha=0.4),
       ,ylab="mm",xlab="Month",lwd=2,bty="l")
  coords <- par("usr")
  trend <- loess(deg~as.numeric(days))
  y <- predict(trend,se=TRUE)
  lines(days[ord],y$fit[ord],col="green",lwd=2)
  lines(days[ord],y$fit[ord]-2*y$se.fit[ord],col="green",lwd=1.5,lty=2)
  lines(days[ord],y$fit[ord]+2*y$se.fit[ord],col="green",lwd=1.5,lty=2)

  text(coords[2]*.5,coords[4]*0.95,"Median Monthly Wind Direction",col=rgb(1,1,1,alpha=0.5),
       cex=3.5)

  text(coords[2]*.15,coords[4]*0.75,
       bquote(mu == .(round(mean(deg,na.rm=TRUE),2)))
       ,col=rgb(1,1,0,alpha=0.85), cex=2)
  abline(h=mean(deg,na.rm=TRUE),col=rgb(1,1,0,alpha=0.85),lty=2)

  grid(col="grey70")

  ## Monthly Temp
  temp <- get(filenames[[1]])

  days <- as.Date(temp[,1])

  Temp <- tapply(temp[,2], format(days, '%m'),max)

  days <- as.numeric(names(Temp))
  
  ord <- order(days)

  plot(days[ord],Temp[ord],type="l",col=rgb(0,0,1,alpha=0.4),
       ,ylab=degree ~ C,xlab="Date",lwd=2,bty="l")
  coords <- par("usr")
  trend <- loess(Temp~as.numeric(days))
  y <- predict(trend,se=TRUE)
  lines(days[ord],y$fit[ord],col="green",lwd=2)
  lines(days[ord],y$fit[ord]-2*y$se.fit[ord],col="green",lwd=1.5,lty=2)
  lines(days[ord],y$fit[ord]+2*y$se.fit[ord],col="green",lwd=1.5,lty=2)

  text(coords[2]*.65,coords[4]*0.95,"Monthly Max Temp",col=rgb(1,1,1,alpha=0.5),
       cex=3.5)
  text(coords[2]*.65,coords[4]*0.8,
        bquote(mu == .(round(mean(Temp,na.rm=TRUE),2)))
       ,col=rgb(1,1,0,alpha=0.85), cex=2)
  abline(h=mean(Temp,na.rm=TRUE),col=rgb(1,1,0,alpha=0.85),lty=2)
 
  grid(col="grey70")

  ## Monthly Net rainfall
  temp <- get(filenames[[5]])

  days <- as.Date(temp[,1])

  Evap <- tapply(temp[,2], format(days, '%m-%Y'),max)
  rain<-rainFull 
  rain <- rain[daysFull<=max(days)&daysFull>=min(days)]
  rain <- tapply(rain, format(daysFull[daysFull<=max(days)&daysFull>=min(days)],
  '%m-%Y'),sum)
  
  days <- as.Date(paste(1,names(Evap),sep="-"),"%d-%m-%Y")
  raindays <- as.Date(paste(1,names(rain),sep="-"),"%d-%m-%Y")
  netRain<-data.frame(rain=rain,days=raindays)
  Evap<-data.frame(evap=Evap*10,days=days) 
  netRain <- merge(netRain,Evap,by="days")
  
  netRain <- netRain[order(netRain$days),]
  netRain$nr<-netRain$rain-netRain$evap

  ## to months
  nr<- tapply(netRain$nr, format(netRain$days, '%m'),mean)
  
  days <- as.numeric(names(nr))
  
  plot(days,nr,type="l",col=rgb(0,0,1,alpha=0.4),
       ,ylab="mm",xlab="Date",lwd=2,bty="l")
  coords <- par("usr")
  trend <- loess(nr~as.numeric(days))
  y <- predict(trend,se=TRUE)
  
  lines(days,y$fit,col="green",lwd=2)
  lines(days,y$fit-2*y$se.fit,col="green",lwd=1.5,lty=2)
  lines(days,y$fit+2*y$se.fit,col="green",lwd=1.5,lty=2)

  text(coords[2]*.65,coords[4]*0.75,"Net rain",col=rgb(1,1,1,alpha=0.5),
       cex=3.5)
  text(coords[2]*.35,coords[4]*0.6,
        bquote(mu == .(round(mean(nr,na.rm=TRUE),2)))
       ,col=rgb(1,1,0,alpha=0.85), cex=2)
  abline(h=mean(nr,na.rm=TRUE),col=rgb(1,1,0,alpha=0.85),lty=2)
  abline(h=0,col=rgb(1,0,0,alpha=0.85),lty=2)

  grid(col="grey70")

dev.off()  
} else {error("Get Climate Process Failed")}
        

## temperature animation
if(readRDS("./tests/DataDownLoad.rds")){

  ## load climate data
  if(!exists("BCIRain_historic")){
    load("./data/ClimateData.rdata") 
  }

  
  filenames <- list(
               "LutzTemp20m",
               "LutzRunoff",
               "LutzWindDirect",
               "BCIRain",
	       "BCIEvap"
              )
  
  require(animation)
 
  PlotClimate <- function(y=temp[,2],Ts=days,func=max,ylb=degree ~ C,
                          ylm=c(25,37),delay=0.1){

    ## Make a plot 
  par(mfrow=c(1,1),mar=c(4,4,1,1),las=1,bg="grey20",fg="white",
  col.lab="white",col.axis="white",cex.lab=1.4,cex.axis=1.4)

  Y <- tapply(y, format(Ts, '%d-%m-%Y'),func)
  Days <- as.Date(names(Y),'%d-%m-%Y')
  years <- as.numeric((format(Days, '%Y')))

  fulldat <- vector("list",length(unique(years)))

 
  for(j in 1:length(unique(years))) {
    i <- sort(unique(years))[j]
    days <- Days[years==i]
    Yi <- tapply(Y[years==i], format(days, '%m'),func,na.rm=TRUE)
    Months <- as.numeric(names(Yi))
    dati <- data.frame(Yi,Months)
    dati <- dati[order(Months),]
    fulldat[[j]] <- dati

    plot(0,0,type="n",col=rgb(1,0,0,alpha=0.99),
         ,ylab=ylb,xlab="Month",lwd=2,bty="l",xlim=c(1,12),
         ylim=ylm,lty=2)
   
    grid(col="grey70")
    
    opaci <- seq(0.1,.7,length.out=length(unique(years)))
    for(z in 1:j){
      lines(fulldat[[z]]$Yi~fulldat[[z]]$Months,col=rgb(1,1,1,alpha=opaci[z]),
            lwd=2)
    }
      lines(dati$Yi~dati$Months,col=rgb(1,0,0,alpha=.99),
            lwd=2)
    text(6,ylm[2]*0.97,i,col=rgb(1,1,1,alpha=0.5),cex=3.5)
    abline(h=mean(do.call(rbind,fulldat)$Yi,na.rm=TRUE),col='red',lty=3)
      Sys.sleep(delay)
    }
  
}

  temp <- get(filenames[[1]])

  days <- as.Date(temp[,1])

  setwd("./figures")
  saveGIF(PlotClimate(delay=0.07), movie.name = "BCItemp.gif")
  
  ## Rain animation
  temp <- get(filenames[[4]])

  days <- as.Date(temp[,1])
  daysHist <- as.Date(BCIRain_historic[,1],"%d/%m/%Y")

  daysFull <- c(daysHist,days[days>as.Date("1971-12-31")])
  rainFull <- c(BCIRain_historic[,2],temp[,2][days>as.Date("1971-12-31")])


  rain <- tapply(rainFull, format(daysFull, '%m'),mean)
  ndays <- tapply(format(daysFull, '%d-%m'), format(daysFull, '%m'),function(X)
   	   			   	     length(unique(X)))
  saveGIF(PlotClimate(y=rainFull,Ts=daysFull,delay=0.01,func=sum,
                      ylm=c(0,1500),ylb="mm rain"),
          movie.name = "BCIrain.gif")
  setwd("../")

  
          dev.off()  
} else {error("Get Climate Process Failed")}
