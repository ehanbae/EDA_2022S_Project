setwd("C:/Users/ehone/Dropbox/SKKU_QAE/5016_Analysis/Assignment3/git/EDA_2022S_Project")

library(RSelenium)
library(dplyr)
library(stringr)
library(rvest)
library(xml2)
library(xlsx)


# S&P500 ticker list
URL <- "https://www.slickcharts.com/sp500"
res <- read_html(URL)

pattern <- "body > div.container-fluid.maxWidth > div:nth-child(3) > div.col-lg-7 > div > div > table > tbody > tr > td:nth-child(3) > a"

Ticker <- res %>%
  html_nodes(pattern) %>%
  html_text()

TickerNo <- length(Ticker)

write.csv(as.data.frame(Ticker),"Ticker.csv")

tab <- NULL

Stack <- NULL

portNo = 10101







# Need to change portNo each time entering into different links, because of robot blocker

for (i in 119:TickerNo) {

  rank = i
stock = Ticker[i]

print(str_c(rank,". ", stock))

#portNo = portNo + 2
#rD <- rsDriver(port= as.integer(portNo), chromever="102.0.5005.61", geckover = NULL)
#remDr <- rD$client

### Try function in case of using portNo that are already in use.
tryCatch({
  portNo = portNo + 2
rD <- rsDriver(port= as.integer(portNo), chromever="102.0.5005.61", geckover = NULL)
} , error = function(e) {
  portNo = portNo + 1
  print(str_c("error for",character(rank)))
})
remDr <- rD$client

###


# EPS
URL <- str_c("https://seekingalpha.com/symbol/",stock,"/earnings")
remDr$navigate(URL)

Sys.sleep(15)

txt <- remDr$getPageSource()[[1]]
res <- read_html(txt)

### The pattern in SeekingAlpha is changed in daily basis. Must update each day.

pattern <- "#content > div.hA.bgA.bgR.bgU.cardsLayout > div > div > div:nth-child(3) > div > div.mqB8.mqDX > div:nth-child(3) > div:nth-child(3) > section > div > div.itM > div.hzA.hzD > div > table > tbody > tr > th"


EPSPeriod <- res %>%
  html_nodes(pattern) %>%
  html_text()


pattern <- "#content > div.hA.bgA.bgR.bgU.cardsLayout > div > div > div:nth-child(3) > div > div.mqB8.mqDX > div:nth-child(3) > div:nth-child(3) > section > div > div.itM > div.hzA.hzD > div > table > tbody > tr > td:nth-child(2)"

EPS <- res %>%
  html_nodes(pattern) %>%
  html_text()

pattern <- "#content > div.hA.bgA.bgR.bgU.cardsLayout > div > div > div:nth-child(3) > div > div.mqB8.mqDX > div:nth-child(3) > div:nth-child(3) > section > div > div.itM > div.hzA.hzD > div > table > tbody > tr > td:nth-child(6)"

EPSAnalystNo <- res %>%
  html_nodes(pattern) %>%
  html_text()


# Rev

pattern <- "#content > div.hA.bgA.bgR.bgU.cardsLayout > div > div > div:nth-child(3) > div > div.mqB8.mqDX > div:nth-child(3) > div:nth-child(4) > section > div > div.itM > div.hzA.hzD > div > table > tbody > tr > th"

SalesPeriod <- res %>%
  html_nodes(pattern) %>%
  html_text()

pattern <- "#content > div.hA.bgA.bgR.bgU.cardsLayout > div > div > div:nth-child(3) > div > div.mqB8.mqDX > div:nth-child(3) > div:nth-child(4) > section > div > div.itM > div.hzA.hzD > div > table > tbody > tr > td:nth-child(2)"

Sales <- res %>%
  html_nodes(pattern) %>%
  html_text()

pattern <- "#content > div.hA.bgA.bgR.bgU.cardsLayout > div > div > div:nth-child(3) > div > div.mqB8.mqDX > div:nth-child(3) > div:nth-child(4) > section > div > div.itM > div.hzA.hzD > div > table > tbody > tr > td:nth-child(6)"

SalesAnalystNo <- res %>%
  html_nodes(pattern) %>%
  html_text()


# Sector & Industry

pattern <- "#content > div.hA.bgA.bgR.bgU.cardsLayout > div > div > div:nth-child(3) > div > div.mqBH.mqDX > div > div > div > section > div.itO.bgA.bgN > div.itM > div > div:nth-child(1) > div.bkmI > div > a"

Sector <- res %>%
  html_nodes(pattern) %>%
  html_text()

pattern <- "#content > div.hA.bgA.bgR.bgU.cardsLayout > div > div > div:nth-child(3) > div > div.mqBH.mqDX > div > div > div > section > div.itO.bgA.bgN > div.itM > div > div:nth-child(2) > div.bkmI > div > a"

Industry <- res %>%
  html_nodes(pattern) %>%
  html_text()

pattern <- "#content > div.hA.bgA.bgR.bgU.cardsLayout > div > div > div:nth-child(3) > div > div.mqBH.mqDX > div > div > div > section > div.itO.bgA.bgN > div.itM > div > div:nth-child(5) > div.bkmI > div > a > span:nth-child(1)"

RankInIndustry <- res %>%
  html_nodes(pattern) %>%
  html_text()

pattern <- "#content > div.hA.bgA.bgR.bgU.cardsLayout > div > div > div:nth-child(3) > div > div.mqBH.mqDX > div > div > div > section > div.itO.bgA.bgN > div.itM > div > div:nth-child(5) > div.bkmI > div > a > span:nth-child(2)"

TotalInIndustry <- res %>%
  html_nodes(pattern) %>%
  html_text()

pattern <- "#content > div.hA.bgA.bgR.bgU.cardsLayout > div > div > div:nth-child(3) > div > div.mqBH.mqDX > div > div > div > section > div.itO.bgA.bgN > div.itM > div > div:nth-child(4) > div.bkmI > div > a > span:nth-child(1)"

RankInSector <- res %>%
  html_nodes(pattern) %>%
  html_text()

pattern <- "#content > div.hA.bgA.bgR.bgU.cardsLayout > div > div > div:nth-child(3) > div > div.mqBH.mqDX > div > div > div > section > div.itO.bgA.bgN > div.itM > div > div:nth-child(4) > div.bkmI > div > a > span:nth-child(2)"

TotalInSector <- res %>%
  html_nodes(pattern) %>%
  html_text()

pattern <- "#content > div.hA.bgA.bgR.bgU.cardsLayout > div > div > div:nth-child(3) > div > div.mqBH.mqDX > div > div > div > section > div.itO.bgA.bgN > div.itM > div > div:nth-child(3) > div.bkmI > div > a > span:nth-child(1)"

RankInAll <- res %>%
  html_nodes(pattern) %>%
  html_text()

pattern <- "#content > div.hA.bgA.bgR.bgU.cardsLayout > div > div > div:nth-child(3) > div > div.mqBH.mqDX > div > div > div > section > div.itO.bgA.bgN > div.itM > div > div:nth-child(3) > div.bkmI > div > a > span:nth-child(2)"

TotalInAll <- res %>%
  html_nodes(pattern) %>%
  html_text()

print(str_c(rank," ",stock, " EPS, Sales, and Quant Ranking."))

Sys.sleep(46)

remDr$close()

rD$server$stop()


Sys.sleep(3)

# Valuation Metrics
tryCatch({
  portNo = portNo + 2
  rD <- rsDriver(port= as.integer(portNo), chromever="102.0.5005.61", geckover = NULL)
} , error = function(e) {
  portNo = portNo + 2
  print(str_c("error for",character(rank)))
})
remDr <- rD$client


URL <- str_c("https://seekingalpha.com/symbol/",stock,"/valuation/metrics")
remDr$navigate(URL)

Sys.sleep(15)

txt <- remDr$getPageSource()[[1]]
res <- read_html(txt)

# PE Ratio

pattern <- "#content > div.hA.bgA.bgR.bgU.cardsLayout > div > div > div:nth-child(3) > div > div.mqB8.mqDX > section:nth-child(2) > div > div.itM > div.hzA.hzD > div > table > tbody > tr:nth-child(1) > td:nth-child(3) > div"

TrailPE <- res %>%
  html_nodes(pattern) %>%
  html_text()

pattern <- "#content > div.hA.bgA.bgR.bgU.cardsLayout > div > div > div:nth-child(3) > div > div.mqB8.mqDX > section:nth-child(2) > div > div.itM > div.hzA.hzD > div > table > tbody > tr:nth-child(2) > td:nth-child(3) > div"

FWDPE <- res %>%
  html_nodes(pattern) %>%
  html_text()


# PS Ratio

pattern <- "#content > div.hA.bgA.bgR.bgU.cardsLayout > div > div > div:nth-child(3) > div > div.mqB8.mqDX > section:nth-child(2) > div > div.itM > div.hzA.hzD > div > table > tbody > tr:nth-child(13) > td:nth-child(3) > div"

TrailPS <- res %>%
  html_nodes(pattern) %>%
  html_text()

pattern <- "#content > div.hA.bgA.bgR.bgU.cardsLayout > div > div > div:nth-child(3) > div > div.mqB8.mqDX > section:nth-child(2) > div > div.itM > div.hzA.hzD > div > table > tbody > tr:nth-child(14) > td:nth-child(3) > div"

FWDPS <- res %>%
  html_nodes(pattern) %>%
  html_text()


# Dividend Yield

pattern <- "#content > div.hA.bgA.bgR.bgU.cardsLayout > div > div > div:nth-child(3) > div > div.mqB8.mqDX > section:nth-child(2) > div > div.itM > div.hzA.hzD > div > table > tbody > tr:nth-child(19) > td:nth-child(3) > div"

DivYield <- res %>%
  html_nodes(pattern) %>%
  html_text()

print(str_c(rank," ",stock, " Valuation Metrics"))

Sys.sleep(46)

remDr$close()

rD$server$stop()


Sys.sleep(3)

# Dividend Estimates

tryCatch({
  portNo = portNo + 2
  rD <- rsDriver(port= as.integer(portNo), chromever="102.0.5005.61", geckover = NULL)
} , error = function(e) {
  portNo = portNo + 2
  print(str_c("error for",character(rank)))
})
remDr <- rD$client



URL <- str_c("https://seekingalpha.com/symbol/",stock,"/dividends/estimates")
remDr$navigate(URL)

Sys.sleep(15)

txt <- remDr$getPageSource()[[1]]
res <- read_html(txt)


pattern <- "#content > div.hA.bgA.bgR.bgU.cardsLayout > div > div > div:nth-child(3) > div > div.mqB8.mqDX > div:nth-child(2) > section:nth-child(1) > div > div.itM > div.hzA.hzD > div > table > tbody > tr > th"

DivPeriod <- res %>%
  html_nodes(pattern) %>%
  html_text()

pattern <- "#content > div.hA.bgA.bgR.bgU.cardsLayout > div > div > div:nth-child(3) > div > div.mqB8.mqDX > div:nth-child(2) > section:nth-child(1) > div > div.itM > div.hzA.hzD > div > table > tbody > tr > td:nth-child(2)"

Div <- res %>%
  html_nodes(pattern) %>%
  html_text()

pattern <- "#content > div.hA.bgA.bgR.bgU.cardsLayout > div > div > div:nth-child(3) > div > div.mqB8.mqDX > div:nth-child(2) > section:nth-child(1) > div > div.itM > div.hzA.hzD > div > table > tbody > tr > td:nth-child(6)"

DivAnalystNo <- res %>%
  html_nodes(pattern) %>%
  html_text()

print(str_c(rank," ",stock, " Dividend Estimates"))

Sys.sleep(46)

remDr$close()

rD$server$stop()

system("taskkill /im java.exe /F")

Sys.sleep(3)

# Match out the length of each rows
n <- max(length(EPSPeriod), length(EPS), length(EPSAnalystNo), length(SalesPeriod),length(Sales),length(SalesAnalystNo),length(DivPeriod),length(Div),length(DivAnalystNo))
length(EPSPeriod) <- n
length(EPS) <- n
length(EPSAnalystNo) <- n
length(SalesPeriod) <- n
length(Sales) <- n
length(SalesAnalystNo) <- n
length(DivPeriod) <- n
length(Div) <- n
length(DivAnalystNo) <- n

tab <- cbind(stock, rank, EPSPeriod, EPS, EPSAnalystNo, TrailPE, FWDPE, SalesPeriod, Sales, SalesAnalystNo, TrailPS, FWDPS, DivPeriod, Div, DivAnalystNo, DivYield, Sector, Industry, RankInIndustry, TotalInIndustry, RankInSector, TotalInSector, RankInAll, TotalInAll) %>%  as_tibble()

?try
try(Stack <- rbind(Stack, tab))


write.csv(Stack, str_c(gsub("-", "",as.character(Sys.Date())),"_Stock_Information.csv"))

print(str_c(rank," ",stock, "saved in the csv file."))

Sys.sleep(3)

}


