 ## Graph BCI climate data
setwd("~/symlinks/git/tRack/")

## Test whether climate data was downloaded and cleaned
if(readRDS("./tests/DataDownLoad.rds")){

  png("./figures/BCIclimate.png",width=800,height=800)
  ## load climate data
  if(!exists("BCIRain_historic")){
    load("./data/ClimateData.rdata") 
  }

  filenames <- list(
                 "LutzTemp20m",
                 "LutzRunoff",
                 "LutzWindDirect",
                 "BCIRain"
                 )

  ## Make a plot of the current and historic rainfall
  par(mfrow=c(2,2),mar=c(4,4,1,1),las=1,bg="grey40")

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

  grid(col="grey10")

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
  
  grid(col="grey10")


  ## Monthly Temp
  temp <- get(filenames[[1]])

  days <- as.Date(temp[,1])

  Temp <- tapply(temp[,2], format(days, '%m-%Y'),max)

  days <- as.Date(paste(1,names(Temp),sep="-"),"%d-%m-%Y")

  ord <- order(days)

  plot(days[ord],Temp[ord],type="l",col=rgb(0,0,1,alpha=0.4),
       ,ylab="mm",xlab="Date",lwd=2,bty="l")
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
 
  grid(col="grey10")

  ## Annual max Temp
  temp <- get(filenames[[1]])

  days <- as.Date(temp[,1])

  Temp <- tapply(temp[,2], format(days, '%Y'),max)

  days <- as.numeric(names(Temp))

  ord <- order(days)

  plot(days[ord],Temp[ord],type="l",col=rgb(0,0,1,alpha=0.4),
       ,ylab="mm",xlab="Date",lwd=2,bty="l")
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

  grid(col="grey10")

dev.off()  
} else {error("Get Climate Process Failed")}
        
