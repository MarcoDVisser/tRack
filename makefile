####################
## Makefile for automated climate data download and analysis
## Using a test driven analysis
####################

## Setup 
DATESECONDS= $(shell date +%s)
LASTUPDATE = $(shell stat -L --format %Y ./data/ClimateData.rdata)
#LASTUPDATE = $(shell stat -L --format %Y makefile)
DELTA = $(shell echo ${DATESECONDS}-${LASTUPDATE} | bc)
UPDATEINTERVAL = $(shell echo "30*24*60*60"  | bc)

## Product-process variables
CV_DIR = .
TESTSCRAPE= ./R/scrapeTest.R
TESTRESULT = ./tests/scrapeTest.rds
GETDATA = ./R/GetClimateData.R
CLIMATEDATA = ./data/ClimateData.rdata
PLOTDATA = ./R/GraphData.R
FINALGRAPH = ./figures/BCItemp.gif

## functions
NEWFILE = touch '$<' 
RNAME = cp '$<' '$@' 
PDOC = pandoc -s --smart '$<' -o '$@'
RBATCH = Rscript --vanilla '$<'

ifeq ($(DELTA),UPDATEINTERVAL)
all: UPDATE $(TESTRESULT) $(CLIMATEDATA) $(PLOTDATA)
endif
#########################
## main markdownx

UPDATE:
	$(rm -rf ./tests ./data)

$(TESTRESULT):$(TESTSCRAPE)
	$(RBATCH)

$(CLIMATEDATA):$(GETDATA)
	$(RBATCH)

$(PLOTDATA):$(FINALGRAPH)
	$(RBATCH)
