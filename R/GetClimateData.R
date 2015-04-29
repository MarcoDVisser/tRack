## Get STRI climate data
setwd("~/symlinks/git/tRack/")

## get basic scrape tools
source("./R/scRape_tools.R")

## Test whether we are allowed to scrape the STRI webpages
site <- "http://biogeodb.stri.si.edu"
target <- "physical_monitoring/research/barrocolorado"

if(readRDS("./tests/scrapeTest.rds")){


} else{
error("scrape test failed")
}
