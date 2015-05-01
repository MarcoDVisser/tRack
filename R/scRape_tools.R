## Some basic webscraping tools
check_scrape <- function(site="http://talkstats.com",
                         target="/login", delay = 0.01, ...) {
    
    if (!grepl("^/", target)) target <- paste0("/", "target")
    robots <- readLines(paste0(site,"/robots.txt"), warn=FALSE)

    if(any(grepl(target, robots))) {
        scrape <- FALSE
    } else {
        scrape <- TRUE
    }
    
    if(any(grepl("Crawl-delay", robots))) {
        Allscrapers <- robots[grep("\\*", robots) + 1]
        robotsdelay <- gsub("[^0-9]", "", Allscrapers[grepl("Crawl-delay", Allscrapers)])
        delay <- as.numeric(robotsdelay)
    }

    list(scrape = scrape, delay = delay) #Use: `Sys.sleep(delay)`
}

