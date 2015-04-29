## Test whether we are allowed to scrape the STRI webpages
##
setwd("~/symlinks/git/tRack/")

## get basic scrape tools
source("./R/scRape_tools.R")

site <- "http://biogeodb.stri.si.edu"
target <- "physical_monitoring/research/barrocolorado"

scrapeStatus <- check_scrape(site,target)

if(scrapeStatus$scrape){
saveRDS(TRUE,"./tests/scrapeTest.rds")
} else{
saveRDS(FALSE,"./tests/scrapeTest.rds")
}
