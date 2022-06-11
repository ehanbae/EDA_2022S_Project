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

for (i in 51:TickerNo) {

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


pattern <- "#content > div.dA.zA.zR.zU.cardsLayout > div > div > div:nth-child(1) > div > div.bjoB.zA.bhF8 > div > h1 > span.bjoG.bvL.bv0.bvA.bvB2.bvDH.bvEE.bgC"

NameData <- res %>%
  html_nodes(pattern) %>%
  html_text()

Name <- gsub("- ","",NameData)

pattern <- "#content > div.dA.zA.zR.zU.cardsLayout > div > div > div:nth-child(3) > div > div.jaB8.jaDX > div:nth-child(3) > div:nth-child(3) > section > div > div.isM > div.ipA.ipD > div > table > tbody > tr > th"


EPSPeriod <- res %>%
  html_nodes(pattern) %>%
  html_text()

EPSYear <- as.numeric(gsub(".*?([0-9]+).*", "\\1", EPSPeriod))

pattern <- "#content > div.dA.zA.zR.zU.cardsLayout > div > div > div:nth-child(3) > div > div.jaB8.jaDX > div:nth-child(3) > div:nth-child(3) > section > div > div.isM > div.ipA.ipD > div > table > tbody > tr > td:nth-child(2)"



EPS <- res %>%
  html_nodes(pattern) %>%
  html_text()

pattern <- "#content > div.dA.zA.zR.zU.cardsLayout > div > div > div:nth-child(3) > div > div.jaB8.jaDX > div:nth-child(3) > div:nth-child(3) > section > div > div.isM > div.ipA.ipD > div > table > tbody > tr > td:nth-child(6)"

EPSAnalystNo <- res %>%
  html_nodes(pattern) %>%
  html_text()


# Rev

pattern <- "#content > div.dA.zA.zR.zU.cardsLayout > div > div > div:nth-child(3) > div > div.jaB8.jaDX > div:nth-child(3) > div:nth-child(4) > section > div > div.isM > div.ipA.ipD > div > table > tbody > tr > th"

SalesPeriod <- res %>%
  html_nodes(pattern) %>%
  html_text()


SalesYear <- as.numeric(gsub(".*?([0-9]+).*", "\\1", SalesPeriod))

pattern <- "#content > div.dA.zA.zR.zU.cardsLayout > div > div > div:nth-child(3) > div > div.jaB8.jaDX > div:nth-child(3) > div:nth-child(4) > section > div > div.isM > div.ipA.ipD > div > table > tbody > tr > td:nth-child(2)"

SalesS <- res %>%
  html_nodes(pattern) %>%
  html_text()


if (grepl("T", SalesS, fixed = TRUE)){
  SalesN <- as.numeric(gsub("T","",SalesS))*1000000000000 
} else if (grepl("B", SalesS, fixed = TRUE)){
  SalesN <- as.numeric(gsub("B","",SalesS))*1000000000
} else if (grepl("M", SalesS, fixed = TRUE)){
  SalesN <- as.numeric(gsub("M","",SalesS))*1000000
} else {
  SalesN <- as.numeric(SalesS)
}


pattern <- "#content > div.dA.zA.zR.zU.cardsLayout > div > div > div:nth-child(3) > div > div.jaB8.jaDX > div:nth-child(3) > div:nth-child(4) > section > div > div.isM > div.ipA.ipD > div > table > tbody > tr > td:nth-child(6)"

SalesAnalystNo <- res %>%
  html_nodes(pattern) %>%
  html_text()


# Sector & Industry

pattern <- "#content > div.dA.zA.zR.zU.cardsLayout > div > div > div:nth-child(3) > div > div.jaBH.jaDX > div > div > div > section > div.isO.zA.zN > div.isM > div > div:nth-child(1) > div.blwI > div > a"

Sector <- res %>%
  html_nodes(pattern) %>%
  html_text()

pattern <- "#content > div.dA.zA.zR.zU.cardsLayout > div > div > div:nth-child(3) > div > div.jaBH.jaDX > div > div > div > section > div.isO.zA.zN > div.isM > div > div:nth-child(2) > div.blwI > div > a"

Industry <- res %>%
  html_nodes(pattern) %>%
  html_text()

pattern <- "#content > div.dA.zA.zR.zU.cardsLayout > div > div > div:nth-child(3) > div > div.jaBH.jaDX > div > div > div > section > div.isO.zA.zN > div.isM > div > div:nth-child(5) > div.blwI > div > a > span:nth-child(1)"

RankInIndustry <- res %>%
  html_nodes(pattern) %>%
  html_text()

pattern <- "#content > div.dA.zA.zR.zU.cardsLayout > div > div > div:nth-child(3) > div > div.jaBH.jaDX > div > div > div > section > div.isO.zA.zN > div.isM > div > div:nth-child(5) > div.blwI > div > a > span:nth-child(2)"

TotalInIndustry <- res %>%
  html_nodes(pattern) %>%
  html_text()

pattern <- "#content > div.dA.zA.zR.zU.cardsLayout > div > div > div:nth-child(3) > div > div.jaBH.jaDX > div > div > div > section > div.isO.zA.zN > div.isM > div > div:nth-child(4) > div.blwI > div > a > span:nth-child(1)"

RankInSector <- res %>%
  html_nodes(pattern) %>%
  html_text()

pattern <- "#content > div.dA.zA.zR.zU.cardsLayout > div > div > div:nth-child(3) > div > div.jaBH.jaDX > div > div > div > section > div.isO.zA.zN > div.isM > div > div:nth-child(4) > div.blwI > div > a > span:nth-child(2)"

TotalInSector <- res %>%
  html_nodes(pattern) %>%
  html_text()

pattern <- "#content > div.dA.zA.zR.zU.cardsLayout > div > div > div:nth-child(3) > div > div.jaBH.jaDX > div > div > div > section > div.isO.zA.zN > div.isM > div > div:nth-child(3) > div.blwI > div > a > span:nth-child(1)"

RankInAll <- res %>%
  html_nodes(pattern) %>%
  html_text()

pattern <- "#content > div.dA.zA.zR.zU.cardsLayout > div > div > div:nth-child(3) > div > div.jaBH.jaDX > div > div > div > section > div.isO.zA.zN > div.isM > div > div:nth-child(3) > div.blwI > div > a > span:nth-child(2)"

TotalInAll <- res %>%
  html_nodes(pattern) %>%
  html_text()

print(str_c(rank," ",stock, " 33%"))

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

pattern <- "#content > div.bA.baA.baR.baU.cardsLayout > div > div > div:nth-child(3) > div > div.jzB8.jzDX > section:nth-child(2) > div > div.iuM > div.ieA.ieD > div > table > tbody > tr:nth-child(1) > td:nth-child(3) > div"

TrailPE <- res %>%
  html_nodes(pattern) %>%
  html_text()

pattern <- "#content > div.bA.baA.baR.baU.cardsLayout > div > div > div:nth-child(3) > div > div.jzB8.jzDX > section:nth-child(2) > div > div.iuM > div.ieA.ieD > div > table > tbody > tr:nth-child(2) > td:nth-child(3) > div"

FWDPE <- res %>%
  html_nodes(pattern) %>%
  html_text()


# PS Ratio

pattern <- "#content > div.bA.baA.baR.baU.cardsLayout > div > div > div:nth-child(3) > div > div.jzB8.jzDX > section:nth-child(2) > div > div.iuM > div.ieA.ieD > div > table > tbody > tr:nth-child(13) > td:nth-child(3) > div"

TrailPS <- res %>%
  html_nodes(pattern) %>%
  html_text()

pattern <- "#content > div.bA.baA.baR.baU.cardsLayout > div > div > div:nth-child(3) > div > div.jzB8.jzDX > section:nth-child(2) > div > div.iuM > div.ieA.ieD > div > table > tbody > tr:nth-child(14) > td:nth-child(3) > div"

FWDPS <- res %>%
  html_nodes(pattern) %>%
  html_text()


# Dividend Yield

pattern <- "#content > div.bA.baA.baR.baU.cardsLayout > div > div > div:nth-child(3) > div > div.jzB8.jzDX > section:nth-child(2) > div > div.iuM > div.ieA.ieD > div > table > tbody > tr:nth-child(19) > td:nth-child(3) > div"

DivYield <- res %>%
  html_nodes(pattern) %>%
  html_text()

DivYieldN <- as.numeric(gsub("%","",fixed=TRUE,DivYield))

pattern <- "#content > div.bA.baA.baR.baU.cardsLayout > div > div > div:nth-child(3) > div > div.jzB8.jzDX > section:nth-child(3) > div > div.iuM > div > div:nth-child(1) > div.blgI > div"

MCapS <- res %>%
  html_nodes(pattern) %>%
  html_text() 

MCapS2 <- gsub(pattern="$", "",fixed=TRUE, MCapS) 

if (grepl("T", MCapS2, fixed = TRUE)){
  MCapN <- as.numeric(gsub("T","",MCapS2))*1000000000000 
} else if (grepl("B", MCapS2, fixed = TRUE)){
  MCapN <- as.numeric(gsub("B","",MCapS2))*1000000000
} else if (grepl("M", MCapS2, fixed = TRUE)){
  MCapN <- as.numeric(gsub("M","",MCapS2))*1000000
} else {
  MCapN <- as.numeric(MCapS2)
}

print(str_c(rank," ",stock, " 66%"))

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


pattern <- "#content > div.bA.baA.baR.baU.cardsLayout > div > div > div:nth-child(3) > div > div.jzB8.jzDX > div:nth-child(2) > section:nth-child(1) > div > div.iuM > div.ieA.ieD > div > table > tbody > tr > th"

DivPeriod <- res %>%
  html_nodes(pattern) %>%
  html_text()

DivYear <- as.numeric(gsub(".*?([0-9]+).*", "\\1", DivPeriod))


pattern <- "#content > div.bA.baA.baR.baU.cardsLayout > div > div > div:nth-child(3) > div > div.jzB8.jzDX > div:nth-child(2) > section:nth-child(1) > div > div.iuM > div.ieA.ieD > div > table > tbody > tr > td:nth-child(2)"

Div <- res %>%
  html_nodes(pattern) %>%
  html_text()

DivN <- as.numeric(gsub("$","",fixed=TRUE,Div))

pattern <- "#content > div.bA.baA.baR.baU.cardsLayout > div > div > div:nth-child(3) > div > div.jzB8.jzDX > div:nth-child(2) > section:nth-child(1) > div > div.iuM > div.ieA.ieD > div > table > tbody > tr > td:nth-child(6)"

DivAnalystNo <- res %>%
  html_nodes(pattern) %>%
  html_text()

print(str_c(rank," ",stock, " 99%"))

Sys.sleep(46)

remDr$close()

rD$server$stop()

system("taskkill /im java.exe /F")

Sys.sleep(3)

# Match out the length of each rows
n <- max(length(EPSPeriod), length(EPS), length(EPSAnalystNo), length(SalesPeriod),length(SalesN),length(SalesAnalystNo),length(DivPeriod),length(Div),length(DivAnalystNo))
length(EPSPeriod) <- n
length(EPSYear) <- n
length(EPS) <- n
length(EPSAnalystNo) <- n
length(SalesPeriod) <- n
length(SalesYear) <- n
length(SalesN) <- n
length(SalesS) <- n
length(SalesAnalystNo) <- n
length(DivPeriod) <- n
length(DivYear) <- n
length(Div) <- n
length(DivN) <- n
length(DivAnalystNo) <- n

tab <- cbind(stock, rank, Name, EPSPeriod, EPSYear, EPS, EPSAnalystNo, TrailPE, FWDPE, SalesPeriod, SalesYear, SalesS, SalesN, SalesAnalystNo, TrailPS, FWDPS, DivPeriod, DivYear, Div, DivN, DivAnalystNo, DivYield, DivYieldN, MCapS, MCapS2, MCapN, Sector, Industry, RankInIndustry, TotalInIndustry, RankInSector, TotalInSector, RankInAll, TotalInAll) %>%  as_tibble()


try(Stack <- rbind(Stack, tab))


write.csv(Stack, str_c(gsub("-", "",as.character(Sys.Date())),"_Stock_Information.csv"))

print(str_c(rank," ",stock, "saved in the csv file."))

Sys.sleep(3)

}


