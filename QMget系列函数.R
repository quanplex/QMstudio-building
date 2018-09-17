#=============
# 获取股票行情信息
#=============

QMget_namesANDcodes <- function(exchange="all"){
  url <- "http://quote.eastmoney.com/stock_list.html"
  web <- getURL(url,.encoding = "gb2312")
  web.tree <- htmlTreeParse(web,useInternalNodes = T)
  sh <- xpathSApply(web.tree, "//div[@id='quotesearch']//a[@name='sz']/parent::div/preceding-sibling::ul//li",xmlValue)
  sz <- xpathSApply(web.tree, "//div[@id='quotesearch']//a[@name='sz']/parent::div/following-sibling::ul//li",xmlValue)
  if (exchange=="all"){
    x <- c(sh,sz)
  } else {
    if (exchange=="sh"){
      x <-sh
    } else{
      x <-sz
    }
  }
  names <- str_extract(x, "^.*(?=\\()") 
  codes <- str_extract(x, "(?<=\\().*(?=\\))")
  x<-data.frame(codes=codes,names=names,stringsAsFactors = F)
  return(x)
}

QMget_codesANDconcept <- function(){
  url <- "http://finance.sina.com.cn/data/#stock-schq-hsgs-gnbk"
  rmDv <- remoteDriver(remoteServerAddr = "127.0.0.1", port = 4444, browserName = "chrome")
  rmDv$open(silent=T) #打开浏览器
  rmDv$setTimeout(milliseconds = 60000)
  rmDv$navigate(url)
  blocks <- rmDv$findElements(using = "css selector", ".tableCellAlignLeft span")
  cat("读取了所有的概念板块名称，一共有",length(blocks),"个概念板块\n")
  X <- data.frame(NULL)
  for (i in seq_along(blocks)){
    rmDv$navigate(url)
    cat(round(i/length(blocks)*100,1),"%---> ")
    blocks <- rmDv$findElements(using = "css selector", ".tableCellAlignLeft span")
    block <- blocks[[i]]
    concept <- block$getElementText() %>% unlist
    block$clickElement()
    Sys.sleep(3)
    url.block <- rmDv$getCurrentUrl() %>% unlist
    ndt <- rmDv$findElement(using = "css selector", ".totalPageSpan")
    Npages <- ndt$getElementText() %>% as.numeric()
    if (is.na(Npages)){
      ndt <- rmDv$findElements(using = "css selector", "#block_1 td:nth-child(1) a")
      data <- sapply(ndt,function(s) s$getElementText()) %>% unlist()
      print(data)
      X.p <-data.frame(codes=data,concept=concept,stringsAsFactors = F)
      X <- rbind(X,X.p)
    }else 
      {
      for (page in seq_along(1:Npages)){
        ndt <- rmDv$findElement(using = "css selector", ".pageNoteSpan+ input")
        ndt$clearElement()
        ndt$sendKeysToElement(list(page %>% as.character()))
        ndt <- rmDv$findElement(using = "css selector", "#pageDiv_0 input+ input")
        ndt$clickElement()
        Sys.sleep(3) # 等着浏览器将网页载入
        ndt <- rmDv$findElements(using = "css selector", "#block_1 td:nth-child(1) a")
        data <- sapply(ndt,function(s) s$getElementText()) %>% unlist()
        print(data)
        X.p <-data.frame(codes=data,concept=concept,stringsAsFactors = F)
        X <- rbind(X,X.p)
      } 
    }
  }
  rmDv$close()
  return(X)
}


QMget_codesANDarea <- function(){
  url <- "http://finance.sina.com.cn/data/#stock-schq-hsgs-dybk"
  rmDv <- remoteDriver(remoteServerAddr = "127.0.0.1", port = 4444, browserName = "chrome")
  rmDv$open(silent=T) #打开浏览器
  rmDv$setTimeout(milliseconds = 60000)
  rmDv$navigate(url)
  blocks <- rmDv$findElements(using = "css selector", ".tableCellAlignLeft span")
  cat("读取了所有的地域板块名称，一共有",length(blocks),"个地域板块\n")
  X <- data.frame(NULL)
  for (i in seq_along(blocks)){
    rmDv$navigate(url)
    cat(round(i/length(blocks)*100,1),"%---> ")
    blocks <- rmDv$findElements(using = "css selector", ".tableCellAlignLeft span")
    block <- blocks[[i]]
    area <- block$getElementText() %>% unlist
    block$clickElement()
    Sys.sleep(3)
    url.block <- rmDv$getCurrentUrl() %>% unlist
    ndt <- rmDv$findElement(using = "css selector", ".totalPageSpan")
    Npages <- ndt$getElementText() %>% as.numeric()
    if (is.na(Npages)){
      ndt <- rmDv$findElements(using = "css selector", "#block_1 td:nth-child(1) a")
      data <- sapply(ndt,function(s) s$getElementText()) %>% unlist()
      print(data)
      X.p <-data.frame(codes=data,area=area,stringsAsFactors = F)
      X <- rbind(X,X.p)
    }else 
    {
      for (page in seq_along(1:Npages)){
        ndt <- rmDv$findElement(using = "css selector", ".pageNoteSpan+ input")
        ndt$clearElement()
        ndt$sendKeysToElement(list(page %>% as.character()))
        ndt <- rmDv$findElement(using = "css selector", "#pageDiv_0 input+ input")
        ndt$clickElement()
        Sys.sleep(3) # 等着浏览器将网页载入
        ndt <- rmDv$findElements(using = "css selector", "#block_1 td:nth-child(1) a")
        data <- sapply(ndt,function(s) s$getElementText()) %>% unlist()
        print(data)
        X.p <-data.frame(codes=data,area=area,stringsAsFactors = F)
        X <- rbind(X,X.p)
      } 
    }
  }
  rmDv$close()
  return(X)
}


QMget_codesANDsector <- function(){
  url <- "http://finance.sina.com.cn/data/#stock-schq-hsgs-xlhy"
  rmDv <- remoteDriver(remoteServerAddr = "127.0.0.1", port = 4444, browserName = "chrome")
  rmDv$open(silent=T) #打开浏览器
  rmDv$setTimeout(milliseconds = 60000)
  rmDv$navigate(url)
  blocks <- rmDv$findElements(using = "css selector", ".tableCellAlignLeft span")
  cat("读取了所有的新浪行业板块名称，一共有",length(blocks),"个行业板块\n")
  X <- data.frame(NULL)
  for (i in seq_along(blocks)){
    rmDv$navigate(url)
    cat(round(i/length(blocks)*100,1),"%---> ")
    blocks <- rmDv$findElements(using = "css selector", ".tableCellAlignLeft span")
    block <- blocks[[i]]
    sector <- block$getElementText() %>% unlist
    block$clickElement()
    Sys.sleep(3)
    url.block <- rmDv$getCurrentUrl() %>% unlist
    ndt <- rmDv$findElement(using = "css selector", ".totalPageSpan")
    Npages <- ndt$getElementText() %>% as.numeric()
    if (is.na(Npages)){
      ndt <- rmDv$findElements(using = "css selector", "#block_1 td:nth-child(1) a")
      data <- sapply(ndt,function(s) s$getElementText()) %>% unlist()
      print(data)
      X.p <-data.frame(codes=data,sector=sector,stringsAsFactors = F)
      X <- rbind(X,X.p)
    }else 
    {
      for (page in seq_along(1:Npages)){
        ndt <- rmDv$findElement(using = "css selector", ".pageNoteSpan+ input")
        ndt$clearElement()
        ndt$sendKeysToElement(list(page %>% as.character()))
        ndt <- rmDv$findElement(using = "css selector", "#pageDiv_0 input+ input")
        ndt$clickElement()
        Sys.sleep(3) # 等着浏览器将网页载入
        ndt <- rmDv$findElements(using = "css selector", "#block_1 td:nth-child(1) a")
        data <- sapply(ndt,function(s) s$getElementText()) %>% unlist()
        print(data)
        X.p <-data.frame(codes=data,sector=sector,stringsAsFactors = F)
        X <- rbind(X,X.p)
      } 
    }
  }
  rmDv$close()
  return(X)
}

QMget_MoneyInflow_SnapShoot_RS <- function() {
  url <- "http://money.finance.sina.com.cn/moneyflow/#jlrepm" 
  rmDv <- remoteDriver(remoteServerAddr = "127.0.0.1", port = 4444, browserName = "chrome")
  rmDv$open(silent=T) #打开浏览器
  rmDv$setTimeout(milliseconds = 60000)
  rmDv$navigate(url)
  ndt <- rmDv$findElement(using="css selector","#upd_time")
  source_time <- ndt$getElementText()[[1]] %>% parse_date_time("d HMS",tz="Asia/Shanghai")
  get_time <-Sys.time()
  final_update_time <- str_c(source_time %>% date,"16:15:30",sep=" ") %>% parse_date_time("y-m-d HMS",tz="Asia/Shanghai")
  if (wday(get_time) %in% 2:6 & get_time < final_update_time){
    cat("注意！！数据并不是当天已收盘的更新数据，而是实时变化的临时数据")
  }
  ndt <- rmDv$findElement(using="xpath","//div[contains(@id, 'S_HQ_CONTAINER')]//div[@class='pages']//a[last()-1]")
  Npages <- ndt$getElementText()[[1]] %>% as.numeric()
  X<-data.frame(NULL)
  ndts <- rmDv$findElements(using="xpath","//div[contains(@id, 'S_HQ_CONTAINER')]/table//thead//th")
  names1 <- sapply(ndts,function(s) s$getElementText()) %>% unlist
  ndts <- rmDv$findElements(using="xpath","//div[contains(@id, 'S_HQ_CONTAINER')]/table//thead//td")
  names2 <- sapply(ndts,function(s) s$getElementText()) %>% unlist
  names <-c(names1,names2)
  for (i in 1:Npages){
    cat("现在正在爬取第",i,"个页面的数据，完成率为",round(i/Npages,3)*100,"%.\n")
    ndts <- rmDv$findElements(using="xpath","//div[contains(@id, 'S_HQ_CONTAINER')]/table//tbody//th")
    y <- sapply(ndts,function(s) s$getElementText()) %>% unlist
    y1 <-matrix(y,ncol=2,byrow=T) %>% as.data.frame()
    ndts <- rmDv$findElements(using="xpath","//div[contains(@id, 'S_HQ_CONTAINER')]/table//tbody//td")
    y <- sapply(ndts,function(s) s$getElementText()) %>% unlist
    y2 <-matrix(y,ncol=9,byrow=T) %>% as.data.frame()
    X.p <- cbind(y1,y2)
    X <- rbind(X,X.p)
    ndt <- rmDv$findElement(using="xpath","//div[contains(@id, 'S_HQ_CONTAINER')]//div[@class='pages']//a[last()]")
    ndt$clickElement()
    Sys.sleep(1)
  }
  names(X) <- names
  X[,3:11]<-lapply(X[,3:11],parse_number)
  X[[1]] <- str_replace(X[[1]],"^(sz|sh)","")
  rmDv$close()
  con <- dbConnect(SQLite(),"QM_Data.sqlite")
  if (wday(get_time) %in% 2:6 & get_time < final_update_time){
    dbWriteTable(con,str_c("临时表：资金流入横截面排名|",source_time %>% date),X)
  }else{
    dbWriteTable(con,str_c("数据表：资金流入横截面排名|",source_time %>% date),X)
  }
  dbDisconnect(con)
} # 需要调用RSelenium 比较慢

QMget_MoneyInflow_SnapShoot <- function(){
  url <- "http://money.finance.sina.com.cn/moneyflow/#jlrepm" 
  rmDv <- remoteDriver(remoteServerAddr = "127.0.0.1", port = 4444, browserName = "chrome")
  rmDv$open(silent=T) #打开浏览器
  rmDv$setTimeout(milliseconds = 60000)
  rmDv$navigate(url)
  Sys.sleep(1)
  ndt <- rmDv$findElement(using="css selector","#upd_time")
  source_time <- ndt$getElementText()[[1]] %>% parse_date_time("d HMS",tz="Asia/Shanghai")
  get_time <-Sys.time()
  final_update_time <- str_c(source_time %>% date,"16:15:30",sep=" ") %>% parse_date_time("y-m-d HMS",tz="Asia/Shanghai")
  con <- dbConnect(SQLite(),"QM_Data.sqlite")
  file <- str_c("数据表：资金流入横截面|",source_time %>% date) 
  if (file %in% dbListTables(con)){
    cat("已经有该数据存在，无需重复获取\n")
    dbDisconnect(con)
  }else{
    if (get_time < final_update_time){
      cat("注意！！数据并不是当天已收盘的更新数据，而是实时变化的临时数据\n")
    }
    ndt <- rmDv$findElement(using="xpath","//div[contains(@id, 'S_HQ_CONTAINER')]//div[@class='pages']//a[last()-1]")
    Npages <- ndt$getElementText()[[1]] %>% as.numeric()
    X<-data.frame(NULL)
    url.template <- "http://money.finance.sina.com.cn/quotes_service/api/json_v2.php/MoneyFlow.ssl_bkzj_ssggzj?page=&num=20&sort=netamount&asc=0&bankuai=&shichang="
    for (i in 1:Npages){
      cat("现在正在爬取第",i,"个页面的数据，完成率为",round(i/Npages,3)*100,"%.\n")
      url.i<- str_replace(url.template,"page=",str_c("page=",i))
      X.p <-readLines(url.i)  %>% iconv(from="GB18030","UTF-8") %>% str_replace_all('(,|\\{)(\\w+):','\\1"\\2":') 
      X.p <-fromJSON(X.p)
      X <- rbind(X,X.p)
    }
    names(X) <-c("代码","名称","最新价","涨跌幅","换手率","成交额","流入资金","流出资金","资金净流入额","资金净流入率",
                 "主力流入","主力流出","主力净流入","散户流入","散户流出","散户净流入","主力净流入率","散户净流入率","主力罗盘")
    X$'代码' <- X$'代码' %>% str_replace("^(sz|sh)","")
    X[,3:ncol(X)]<-lapply(X[,3:ncol(X)],parse_number)
    X$'换手率' <- X$'换手率' /100
    if (get_time < final_update_time){
      dbWriteTable(con,str_c("临时表：资金流入横截面|",source_time ),X,overwrite=T)
    }else{
      dbWriteTable(con,str_c("数据表：资金流入横截面|",source_time %>% date),X)
    }
  } 
  dbDisconnect(con)
  rmDv$close()
}


QMget_ZhuliMoneyInflow_SnapShoot_RS <- function() {
  url <- "http://money.finance.sina.com.cn/moneyflow/#zljlrepm" 
  rmDv <- remoteDriver(remoteServerAddr = "127.0.0.1", port = 4444, browserName = "chrome")
  rmDv$open(silent=T) #打开浏览器
  rmDv$setTimeout(milliseconds = 60000)
  rmDv$navigate(url)
  Sys.sleep(1)
  ndt <- rmDv$findElement(using="css selector","#upd_time")
  source_time <- ndt$getElementText()[[1]] %>% parse_date_time("d HMS",tz="Asia/Shanghai")
  get_time <-Sys.time()
  final_update_time <- str_c(source_time %>% date,"16:15:30",sep=" ") %>% parse_date_time("y-m-d HMS",tz="Asia/Shanghai")
  if (wday(get_time) %in% 2:6 & get_time < final_update_time){
    cat("注意！！数据并不是当天已收盘的更新数据，而是实时变化的临时数据")
  }
  ndt <- rmDv$findElement(using="xpath","//div[contains(@id, 'S_HQ_CONTAINER')]//div[@class='pages']//a[last()-1]")
  Npages <- ndt$getElementText()[[1]] %>% as.numeric()
  X<-data.frame(NULL)
  ndts <- rmDv$findElements(using="xpath","//div[contains(@id, 'S_HQ_CONTAINER')]/table//thead//th")
  names1 <- sapply(ndts,function(s) s$getElementText()) %>% unlist
  ndts <- rmDv$findElements(using="xpath","//div[contains(@id, 'S_HQ_CONTAINER')]/table//thead//td")
  names2 <- sapply(ndts,function(s) s$getElementText()) %>% unlist
  names <-c(names1,names2)
  for (i in 1:Npages){
    cat("现在正在爬取第",i,"个页面的数据，完成率为",round(i/Npages,3)*100,"%.\n")
    ndts <- rmDv$findElements(using="xpath","//div[contains(@id, 'S_HQ_CONTAINER')]/table//tbody//th")
    y <- sapply(ndts,function(s) s$getElementText()) %>% unlist
    y1 <-matrix(y,ncol=2,byrow=T) %>% as.data.frame()
    ndts <- rmDv$findElements(using="xpath","//div[contains(@id, 'S_HQ_CONTAINER')]/table//tbody//td")
    y <- sapply(ndts,function(s) s$getElementText()) %>% unlist
    y2 <-matrix(y,ncol=9,byrow=T) %>% as.data.frame()
    X.p <- cbind(y1,y2)
    X <- rbind(X,X.p)
    ndt <- rmDv$findElement(using="xpath","//div[contains(@id, 'S_HQ_CONTAINER')]//div[@class='pages']//a[last()]")
    ndt$clickElement()
    Sys.sleep(1)
  }
  names(X) <- names
  X[,3:11]<-lapply(X[,3:11],parse_number)
  X[[1]] <- str_replace(X[[1]],"^(sz|sh)","")
  rmDv$close()
  con <- dbConnect(SQLite(),"QM_Data.sqlite")
  if (wday(get_time) %in% 2:6 & get_time < final_update_time){
    dbWriteTable(con,str_c("临时表：主力资金流入横截面排名|",source_time %>% date),X)
  }else{
    dbWriteTable(con,str_c("数据表：主力资金流入横截面排名|",source_time %>% date),X)
  }
  dbDisconnect(con)
}

QMget_SanhuMoneyInflow_SnapShoot_RS <- function() {
  url <- "http://money.finance.sina.com.cn/moneyflow/#shjlrepm" 
  rmDv <- remoteDriver(remoteServerAddr = "127.0.0.1", port = 4444, browserName = "chrome")
  rmDv$open(silent=T) #打开浏览器
  rmDv$setTimeout(milliseconds = 60000)
  rmDv$navigate(url)
  Sys.sleep(1)
  ndt <- rmDv$findElement(using="css selector","#upd_time")
  source_time <- ndt$getElementText()[[1]] %>% parse_date_time("d HMS",tz="Asia/Shanghai")
  get_time <-Sys.time()
  final_update_time <- str_c(source_time %>% date,"16:15:30",sep=" ") %>% parse_date_time("y-m-d HMS",tz="Asia/Shanghai")
  if (wday(get_time) %in% 2:6 & get_time < final_update_time){
    cat("注意！！数据并不是当天已收盘的更新数据，而是实时变化的临时数据")
  }
  ndt <- rmDv$findElement(using="xpath","//div[contains(@id, 'S_HQ_CONTAINER')]//div[@class='pages']//a[last()-1]")
  Npages <- ndt$getElementText()[[1]] %>% as.numeric()
  X<-data.frame(NULL)
  ndts <- rmDv$findElements(using="xpath","//div[contains(@id, 'S_HQ_CONTAINER')]/table//thead//th")
  names1 <- sapply(ndts,function(s) s$getElementText()) %>% unlist
  ndts <- rmDv$findElements(using="xpath","//div[contains(@id, 'S_HQ_CONTAINER')]/table//thead//td")
  names2 <- sapply(ndts,function(s) s$getElementText()) %>% unlist
  names <-c(names1,names2)
  for (i in 1:Npages){
    cat("现在正在爬取第",i,"个页面的数据，完成率为",round(i/Npages,3)*100,"%.\n")
    ndts <- rmDv$findElements(using="xpath","//div[contains(@id, 'S_HQ_CONTAINER')]/table//tbody//th")
    y <- sapply(ndts,function(s) s$getElementText()) %>% unlist
    y1 <-matrix(y,ncol=2,byrow=T) %>% as.data.frame()
    ndts <- rmDv$findElements(using="xpath","//div[contains(@id, 'S_HQ_CONTAINER')]/table//tbody//td")
    y <- sapply(ndts,function(s) s$getElementText()) %>% unlist
    y2 <-matrix(y,ncol=9,byrow=T) %>% as.data.frame()
    X.p <- cbind(y1,y2)
    X <- rbind(X,X.p)
    ndt <- rmDv$findElement(using="xpath","//div[contains(@id, 'S_HQ_CONTAINER')]//div[@class='pages']//a[last()]")
    ndt$clickElement()
    Sys.sleep(1)
  }
  names(X) <- names
  X[,3:11]<-lapply(X[,3:11],parse_number)
  X[[1]] <- str_replace(X[[1]],"^(sz|sh)","")
  rmDv$close()
  con <- dbConnect(SQLite(),"QM_Data.sqlite")
  if (wday(get_time) %in% 2:6 & get_time < final_update_time){
    dbWriteTable(con,str_c("临时表：散户资金流入横截面排名|",source_time %>% date),X)
  }else{
    dbWriteTable(con,str_c("数据表：散户资金流入横截面排名|",source_time %>% date),X)
  }
  dbDisconnect(con)
}


QMget_ContZhuliMoneyInflow_SnapShoot_RS <- function() {
  url <- "http://money.finance.sina.com.cn/moneyflow/#lxjlrg" 
  rmDv <- remoteDriver(remoteServerAddr = "127.0.0.1", port = 4444, browserName = "chrome")
  rmDv$open(silent=T) #打开浏览器
  rmDv$setTimeout(milliseconds = 60000)
  rmDv$navigate(url)
  Sys.sleep(1)
  ndt <- rmDv$findElement(using="css selector","#upd_time")
  source_time <- ndt$getElementText()[[1]] %>% parse_date_time("d HMS",tz="Asia/Shanghai")
  get_time <-Sys.time()
  final_update_time <- str_c(source_time %>% date,"16:15:30",sep=" ") %>% parse_date_time("y-m-d HMS",tz="Asia/Shanghai")
  if (wday(get_time) %in% 2:6 & get_time < final_update_time){
    cat("注意！！数据并不是当天已收盘的更新数据，而是实时变化的临时数据")
  }
  ndt <- rmDv$findElement(using="xpath","//div[contains(@id, 'S_HQ_CONTAINER')]//div[@class='pages']//a[last()-1]")
  Npages <- ndt$getElementText()[[1]] %>% as.numeric()
  X<-data.frame(NULL)
  ndts <- rmDv$findElements(using="xpath","//div[contains(@id, 'S_HQ_CONTAINER')]/table//thead//th")
  names1 <- sapply(ndts,function(s) s$getElementText()) %>% unlist
  ndts <- rmDv$findElements(using="xpath","//div[contains(@id, 'S_HQ_CONTAINER')]/table//thead//td")
  names2 <- sapply(ndts,function(s) s$getElementText()) %>% unlist
  names <-c(names1,names2)
  for (i in 1:Npages){
    cat("现在正在爬取第",i,"个页面的数据，完成率为",round(i/Npages,3)*100,"%.\n")
    ndts <- rmDv$findElements(using="xpath","//div[contains(@id, 'S_HQ_CONTAINER')]/table//tbody//th")
    y <- sapply(ndts,function(s) s$getElementText()) %>% unlist
    y1 <-matrix(y,ncol=2,byrow=T) %>% as.data.frame()
    ndts <- rmDv$findElements(using="xpath","//div[contains(@id, 'S_HQ_CONTAINER')]/table//tbody//td")
    y <- sapply(ndts,function(s) s$getElementText()) %>% unlist
    y2 <-matrix(y,ncol=8,byrow=T) %>% as.data.frame()
    X.p <- cbind(y1,y2)
    X <- rbind(X,X.p)
    ndt <- rmDv$findElement(using="xpath","//div[contains(@id, 'S_HQ_CONTAINER')]//div[@class='pages']//a[last()]")
    ndt$clickElement()
    Sys.sleep(1)
  }
  names(X) <- names
  X[,4:10]<-lapply(X[,4:10],parse_number)
  X[[1]] <- str_replace(X[[1]],"^(sz|sh)","")
  rmDv$close()
  con <- dbConnect(SQLite(),"QM_Data.sqlite")
  if (wday(get_time) %in% 2:6 & get_time < final_update_time){
    dbWriteTable(con,str_c("临时表：连续主力资金流入横截面排名|",source_time %>% date),X)
  }else{
    dbWriteTable(con,str_c("数据表：连续主力资金流入横截面排名|",source_time %>% date),X)
  }
  dbDisconnect(con)
}

QMget_ContMoneyInflow_SnapShoot <- function(){
  url <- "http://money.finance.sina.com.cn/moneyflow/#lxjlrg" 
  rmDv <- remoteDriver(remoteServerAddr = "127.0.0.1", port = 4444, browserName = "chrome")
  rmDv$open(silent=T) #打开浏览器
  rmDv$setTimeout(milliseconds = 60000)
  rmDv$navigate(url)
  Sys.sleep(1)
  ndt <- rmDv$findElement(using="css selector","#upd_time")
  source_time <- ndt$getElementText()[[1]] %>% parse_date_time("d HMS",tz="Asia/Shanghai")
  get_time <-Sys.time()
  final_update_time <- str_c(source_time %>% date,"16:15:30",sep=" ") %>% parse_date_time("y-m-d HMS",tz="Asia/Shanghai")
  con <- dbConnect(SQLite(),"QM_Data.sqlite")
  file <- str_c("数据表：主力连续流入横截面|",source_time %>% date)
  if (file %in% dbListTables(con)){
    cat("已经有该数据存在，无需重复获取\n")
    dbDisconnect(con)
  }else{
    if (get_time < final_update_time){
      cat("注意！！数据并不是当天已收盘的更新数据，而是实时变化的临时数据\n")
    }
    ndt <- rmDv$findElement(using="xpath","//div[contains(@id, 'S_HQ_CONTAINER')]//div[@class='pages']//a[last()-1]")
    Npages <- ndt$getElementText()[[1]] %>% as.numeric()
    X<-data.frame(NULL)
    url.template <- "http://money.finance.sina.com.cn/quotes_service/api/json_v2.php/MoneyFlow.ssl_bkzj_lxjlr?page=&num=20&sort=cnt_r0x_ratio&asc=0&bankuai="
    for (i in 1:Npages){
      cat("现在正在爬取第",i,"个页面的数据，完成率为",round(i/Npages,3)*100,"%.\n")
      url.i<- str_replace(url.template,"page=",str_c("page=",i))
      X.p <-readLines(url.i)  %>% iconv(from="GB18030","UTF-8") %>% str_replace_all('(,|\\{)(\\w+):','\\1"\\2":') 
      X.p <-fromJSON(X.p)
      X <- rbind(X,X.p)
    }
    names(X) <-c("代码","名称","流入天数","最新价","阶段涨跌幅","阶段换手率","成交额","资金净流入额","资金净流入率",
                 "主力净流入","主力罗盘")
    X$'代码' <- X$'代码' %>% str_replace("^(sz|sh)","")
    X[,3:ncol(X)]<-lapply(X[,3:ncol(X)],parse_number)
    X$'阶段换手率' <- X$'阶段换手率' /100
    if (get_time < final_update_time){
      dbWriteTable(con,str_c("临时表：主力连续流入横截面|",source_time %>% date),X,overwrite=T)
    }else{
      dbWriteTable(con,str_c("数据表：主力连续流入横截面|",source_time %>% date),X)
    }
  }
  dbDisconnect(con)
  rmDv$close()
}


QMget_MoneyInflow_SnapShootBack <- function(){
  url <- "http://money.finance.sina.com.cn/moneyflow/#ggjdtj" 
  rmDv <- remoteDriver(remoteServerAddr = "127.0.0.1", port = 4444, browserName = "chrome")
  rmDv$open(silent=T) #打开浏览器
  rmDv$setTimeout(milliseconds = 60000)
  rmDv$navigate(url)
  Sys.sleep(1)
  ndt <- rmDv$findElement(using="css selector","#upd_time")
  source_time <- ndt$getElementText()[[1]] %>% parse_date_time("d HMS",tz="Asia/Shanghai")
  get_time <-Sys.time()
  final_update_time <- str_c(source_time %>% date,"16:16:00",sep=" ") %>% parse_date_time("y-m-d HMS",tz="Asia/Shanghai")
  con <- dbConnect(SQLite(),"QM_Data.sqlite")
  ndt <- rmDv$findElement(using="xpath","//div[contains(@id, 'S_HQ_CONTAINER')]//div[@class='pages']//a[last()-1]")
  Npages <- ndt$getElementText()[[1]] %>% as.numeric()
  X<-data.frame(NULL)
  url.template <- "http://money.finance.sina.com.cn/quotes_service/api/json_v2.php/MoneyFlow.ssl_bkzj_jdtj_jd?page=&num=20&sort=symbol&asc=1&bankuai=&tian="
  back <-c(3,5,10,20)
  for (j in seq_along(back)){
    for (i in 1:Npages){
      cat("现在正在爬取近",back[j],"天的第",i,"个页面的数据，完成率为",round((i/Npages)/4+(j-1)*1/4,3)*100,"%.\n")
      url.i<- str_replace(url.template,"page=",str_c("page=",i))
      url.ij <- str_replace(url.i,"tian=",str_c("tian=",back[j]))
      X.p <-readLines(url.ij)  %>% iconv(from="GB18030","UTF-8") %>% str_replace_all('(,|\\{)(\\w+):','\\1"\\2":') 
      X.p <-fromJSON(X.p)
      X.p[[11]]<-back[j]
      X <- rbind(X,X.p)
    }
  }
  names(X) <-c("代码","名称","阶段结束价","阶段涨跌幅","阶段换手率","成交额","资金净流入额","资金净流入率",
               "主力净流入","主力罗盘","回溯期")
  X$'代码' <- X$'代码' %>% str_replace("^(sz|sh)","")
  X[,3:ncol(X)]<-lapply(X[,3:ncol(X)],parse_number)
  X$'阶段换手率' <- X$'阶段换手率' /100
  X.rsp <- X %>% gather(k=var,value=value,-代码,-名称,-回溯期)
  X.rsp <- X.rsp %>% unite(var.new,回溯期,var, sep="日")
  X.rsp <- X.rsp %>% mutate(var.new=str_c("近",var.new))
  Y <- spread(X.rsp, key=var.new, value=value)
  if (get_time < final_update_time){
    dbWriteTable(con,str_c("临时表：资金流入回顾横截面|",source_time %>% date),Y,overwrite=T)
  }else{
    dbWriteTable(con,str_c("数据表：资金流入回顾横截面|",source_time %>% date),Y)
  }
  dbDisconnect(con)
  rmDv$close()
}


QMget_MoneyInflowFORConcepts_3SnapShoot <- function() {
  url <- "http://money.finance.sina.com.cn/moneyflow/#bkgainian" 
  rmDv <- remoteDriver(remoteServerAddr = "127.0.0.1", port = 4444, browserName = "chrome")
  rmDv$open(silent=T) #打开浏览器
  rmDv$setTimeout(milliseconds = 60000)
  rmDv$navigate(url)
  Sys.sleep(1)
  ndt <- rmDv$findElement(using="css selector","#upd_time")
  source_time <- ndt$getElementText()[[1]] %>% parse_date_time("d HMS",tz="Asia/Shanghai")
  get_time <-Sys.time()
  final_update_time <- str_c(source_time %>% date,"16:15:30",sep=" ") %>% parse_date_time("y-m-d HMS",tz="Asia/Shanghai")
  if (wday(get_time) %in% 2:6 & get_time < final_update_time){
    cat("注意！！数据并不是当天已收盘的更新数据，而是实时变化的临时数据")
  }
  ndt <- rmDv$findElement(using="xpath","//div[contains(@id, 'S_HQ_CONTAINER')]//div[@class='pages']//a[last()-1]")
  Npages <- ndt$getElementText()[[1]] %>% as.numeric()
  X<-data.frame(NULL)
  ndts <- rmDv$findElements(using="xpath","//div[contains(@id, 'S_HQ_CONTAINER')]/table//thead//tr[last()]//td")
  names1 <- sapply(ndts,function(s) s$getElementText()) %>% unlist
  names1 <- str_c(names1,rep(c("3日","5日","10日"),c(3,3,3)),sep=":")
  names <-c("概念板块",names1)
  for (i in 1:Npages){
    cat("现在正在爬取第",i,"个页面的数据，完成率为",round(i/Npages,3)*100,"%.\n")
    ndts <- rmDv$findElements(using="xpath","//div[contains(@id, 'S_HQ_CONTAINER')]/table//tbody//th[last()]")
    y <- sapply(ndts,function(s) s$getElementText()) %>% unlist
    y1 <-matrix(y,ncol=1,byrow=T) %>% as.data.frame()
    ndts <- rmDv$findElements(using="xpath","//div[contains(@id, 'S_HQ_CONTAINER')]/table//tbody//td")
    y <- sapply(ndts,function(s) s$getElementText()) %>% unlist
    y2 <-matrix(y,ncol=9,byrow=T) %>% as.data.frame()
    X.p <- cbind(y1,y2)
    X <- rbind(X,X.p)
    ndt <- rmDv$findElement(using="xpath","//div[contains(@id, 'S_HQ_CONTAINER')]//div[@class='pages']//a[last()]")
    ndt$clickElement()
    Sys.sleep(1)
  }
  names(X) <- names
  X[,2:10]<-lapply(X[,2:10],parse_number)
  rmDv$close()
  con <- dbConnect(SQLite(),"QM_Data.sqlite")
  if (wday(get_time) %in% 2:6 & get_time < final_update_time){
    dbWriteTable(con,str_c("临时表：概念板块资金流入横截面排名|",source_time %>% date),X)
  }else{
    dbWriteTable(con,str_c("数据表：概念板块资金流入横截面排名|",source_time %>% date),X)
  }
  dbDisconnect(con)
    
}



QMget_OHLCVA_History <-function(stockcode,database="QM_Data.sqlite"){
  cat("正在下载股票",stockcode,"的历史行情数据\n")
  con <- dbConnect(SQLite(),database)
  file <-str_c("数据表：股票行情历史(",stockcode,")")
  if (file %in% dbListTables(con)){
    cat("数据库中已经有该股票的历史数据，转换用 QMupdate_OHLCVA_History函数处理\n")
    QMupdate_OHLCVA_History(stockcode,database=database)
    dbDisconnect(con)
  } else{
    url=paste("http://vip.stock.finance.sina.com.cn/corp/go.php/vMS_FuQuanMarketHistory/stockid/",
              stockcode,".phtml",sep="")
    web<-getURL(url,.encoding = "gb2312")
    web=htmlTreeParse(web,useInternalNodes = T)
    year.node<-getNodeSet(web, "//select[@name='year']//option")
    year=sapply(year.node,xmlValue) %>% as.numeric()
    year<-year[year>=1990]
    jidu=1:4
    urls=c()
    for (i in 1:length(year)){
      for(j in 1:length(jidu)){
        urls=c(urls,paste(url,"?year=",year[i],"&jidu=",jidu[j],sep=""))
      }
    }
    X=data.frame(NULL,stringsAsFactors = F)
    for(i in 1:length(urls)){
      cat("从finance.sina.com 读取第 ",i,"个页面,","完成率为",round(i/length(urls),3)*100,"%\n")
      all.tables=readHTMLTable(urls[i],header=F,verbose=T)
      pick<-sapply(1:length(all.tables),function(i)ncol(all.tables[[i]]))
      fg.table<-which(pick==8)
      if(length(fg.table)==0) {
        next
      } else {
        X.p <- all.tables[[fg.table]]
        X.p <- X.p[-c(1,2),]
        X=rbind(X,X.p)
      }
    }
    X[,2:8]<-lapply(X[,2:8],parse_number)
    names(X)<-c("日期","开盘价","最高价","收盘价","最低价","交易量","交易金额","复权因子")
    X<-X[order(X$"日期"),]
    dbWriteTable(con,file,X)
    dbDisconnect(con)
    return(X)
  }
}

QMget_OHLCVA_History_Group <- function (codelist,database="QM_Data.sqlite") {
  log<-data.frame(NULL)
  for (i in 1:length(codelist)){
    d=0
    repeat{
      stockdata<-tryCatch(QMget_OHLCVA_History(codelist[i]),error=function(e) NULL)
      d=d+1
      cat("下载股票",codelist[i],"的数据，尝试第",d,"次 \n")
      if(!is.null(stockdata)|d>=10) break
    } 
    log<-rbind(log,data.frame(codelist[i],d,ifelse(d<10,"成功","失败"),stringsAsFactors = F))
  }
  names(log)<-c("股票代码","尝试下载次数","是否成功")
  return(log)
}


QMget_OHLCVA_Index_History <-function(indexcode,database="QM_Data.sqlite"){
  cat("正在下载指数",indexcode,"的历史行情数据\n")
  con <- dbConnect(SQLite(),database)
  file <-str_c("数据表：指数行情历史(",indexcode,")")
  if (file %in% dbListTables(con)){
    cat("数据库中已经有该指数的历史数据，转换用 QMupdate_OHLCVA_Index_History函数处理\n")
    QMupdate_OHLCVA_Index_History(indexcode,database=database)
    dbDisconnect(con)
  } else{
    url=paste("http://vip.stock.finance.sina.com.cn/corp/go.php/vMS_MarketHistory/stockid/",
              indexcode,"/type/S.phtml",sep="")
    web<-getURL(url,.encoding = "gb2312")
    web=htmlTreeParse(web,useInternalNodes = T)
    year.node<-getNodeSet(web, "//select[@name='year']//option")
    year=sapply(year.node,xmlValue) %>% as.numeric()
    year<-year[year>=1990]
    jidu=1:4
    urls=c()
    for (i in 1:length(year)){
      for(j in 1:length(jidu)){
        urls=c(urls,paste(url,"?year=",year[i],"&jidu=",jidu[j],sep=""))
      }
    }
    X=data.frame(NULL,stringsAsFactors = F)
    for(i in 1:length(urls)){
      cat("从finance.sina.com 读取第 ",i,"个页面,","完成率为",round(i/length(urls),3)*100,"%\n")
      all.tables=readHTMLTable(urls[i],header=F,verbose=T)
      pick<-sapply(1:length(all.tables),function(i)ncol(all.tables[[i]]))
      fg.table<-which(pick==7)
      if(length(fg.table)==0) {
        next
      } else {
        X.p <- all.tables[[fg.table]]
        X.p <- X.p[-c(1,2),]
        X=rbind(X,X.p)
      }
    }
    X[,2:7]<-lapply(X[,2:7],parse_number)
    names(X)<-c("日期","开盘价","最高价","收盘价","最低价","交易量","交易金额")
    X<-X[order(X$"日期"),]
    dbWriteTable(con,file,X)
    dbDisconnect(con)
    return(X)
  }
}
