## Get STRI climate data
setwd("~/symlinks/git/tRack/")

## get basic scrape tools
source("./R/scRape_tools.R")

## Test whether we are allowed to scrape the STRI webpages
site <- "http://biogeodb.stri.si.edu"
target <- "physical_monitoring/research/barrocolorado"

filelist <- list(
              "/physical_monitoring/downloads/bci_elect_20m_at.zip",
              "/physical_monitoring/downloads/bci_elect_lutzweir.zip",
              "/physical_monitoring/downloads/bci_elect_48m_wd.zip",
              "/physical_monitoring/downloads/bci_manual_cl_ra.zip",
	      "/physical_monitoring/downloads/bci_manual_cl_evap.zip"
                 )

filenames <- list(
               "LutzTemp20m",
               "LutzRunoff",
               "LutzWindDirect",
               "BCIRain",
	       "BCIEvap"
              )

.OldObjects <- ls()

if(readRDS("./tests/scrapeTest.rds")){

  Delay <- readRDS("./tests/scrapeDelay.rds")

  ## First assume download fails
  saveRDS(FALSE,"./tests/DataDownLoad.rds")

  for(i in 1:length(filelist)){
    temp <- tempfile()
    temp2 <- tempfile()
    download.file(paste0(site,filelist[[i]]),temp)
    zipfiles <- unzip(temp,list=TRUE)
    filenum <- grepl(".csv",zipfiles$Name)&!grepl("mx|mn|sd|1929",zipfiles$Name)

    tmpdat<-tryCatch(read.csv(unz(temp, zipfiles$Name[filenum]),header=TRUE),
    error=function(e) NULL)
    ## alternative extraction
    if(is.null(tmpdat)){
    system(paste0("unzip -o ", temp," -d /tmp/"))
    tmpdat<-tryCatch(read.csv(paste0("/tmp/", zipfiles$Name[filenum]),header=TRUE),
    error=function(e) NULL)
     }
         
    assign(filenames[[i]],tmpdat)
    rm(tmpdat)
    ## add historic rain data
    if(filenames[[i]]=="BCIRain"){
      filenum <- grepl(".csv",zipfiles$Name)&!grepl("mx|mn|sd|man",zipfiles$Name)
      assign("BCIRain_historic",
           read.csv(unz(temp, zipfiles$Name[filenum]),header=TRUE))


      ## lag script to conform to potential robots.txt delay
      Sys.sleep(Delay)
    }
    unlink(temp)
}

  ## remove clutter
  rm(list=.OldObjects)

  ## save climate data
  save.image("./data/ClimateData.rdata")
  saveRDS(TRUE,"./tests/DataDownLoad.rds")
} else{
error("scrape test failed")
}



