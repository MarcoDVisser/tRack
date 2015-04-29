# tRack

A reprository built to automatically track and graph the Barro Colorado Island climate data. The reprository automatically downloads and extracts BCI climate data from the [Smithsonian Tropical Research Institute website](http://biogeodb.stri.si.edu/physical_monitoring/research/barrocolorado), and graphs it below.

It is build on a test-driven platform, and uses a makefile to initiate webscraping, analysis and graphing. It has a refresh rate of once a month (see the makefile). This github version will however be updated less frequently as I intend to maintain this manually. The data will not be included here, as it is hosted by the Smithsonian.

## Results

![bci climate](https://raw.githubusercontent.com/MarcoDVisser/tRack/master/figures/BCIclimate.png)

### Acknowledgements
Cudo's to @trinker for inspiring me to move to R for web scraping
