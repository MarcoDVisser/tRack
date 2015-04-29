####################
## Makefile for automated climate data download and analysis
## Using a test driven analysis
####################

## Setup 
DATE= $(shell date +"%m-%d-%Y")
CV_DIR = .
TESTSCRAPE= ./R/scrapeTest.R
TESTRESULT = ./tests/scrapeTest.rds
GETDATA = ./R/GetClimateData.R
CLIMATEDATA = ./data/ClimateData.rdata


## functions
RNAME = cp '$<' '$@' 
PDOC = pandoc -s --smart '$<' -o '$@'
RBATCH = R CMD BATCH '$<'

all: $(TESTRESULT) $(CLIMATEDATA)

#########################
## main markdownx

$(TESTRESULT):$(TESTSCRAPE)
	$(RBATCH)

$(PDF):$(MASTER)
	$(PDOC)

$(WORD):$(MASTER)
	$(PDOC)
