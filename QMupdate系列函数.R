QMupdate_OHLCVA_History <-function(stockcode,database="QM_Data.sqlite"){
  cat("正在更新股票",stockcode,"的历史行情数据\n")
  con <- dbConnect(SQLite(),database)
  file <-str_c("数据表：股票行情历史(",stockcode,")")
  X.old <-dbReadTable(con, file)
  year.old <- max(X.old$"日期")  %>% year()
  jidu.old <- max(X.old$"日期")  %>% quarter()
  if (Sys.time() %>% hour <= 19) { 
    update.day <- today()-1
    } else {
    update.day <-today() 
    }
  old.day <- X.old$"日期" %>% max() %>% parse_date()
  if (!(file %in% dbListTables(con))){
    cat("数据库中还没有该股票的历史数据，自动转换用 QMget_OHLCVA_History 函数进行处理处理\n")
    QMget_OHLCVA_History(stockcode,database=database)
    dbDisconnect(con)
  } else{
    if ((old.day %>% wday %in% 2:5 & difftime(update.day,old.day) %>% as.numeric==0) | 
        (old.day %>% wday ==6 & difftime(update.day,old.day) %>% as.numeric <=2)){
      cat("已经是最新数据，不需要更新。\n")
    } else{
      url=paste("http://vip.stock.finance.sina.com.cn/corp/go.php/vMS_FuQuanMarketHistory/stockid/",
                stockcode,".phtml",sep="")
      web<-try(getURL(url,.encoding = "gb2312"))
      web=htmlTreeParse(web,useInternalNodes = T)
      year.node<-getNodeSet(web, "//select[@name='year']//option")
      year=sapply(year.node,xmlValue) %>% as.numeric()
      year<-year[year>=1990]
      jidu=1:4
      update.index<-data.frame(NULL)
      for (yr in year[year>=year.old]){
        if (yr==year.old){
          for (qt in jidu[jidu>=jidu.old]){
            url1<-str_c(url,"?year=",yr,"&jidu=",qt)
            update.index<-rbind(update.index,data.frame(yr,qt,url1,stringsAsFactors = F))
          }
        }else{
          for(qt in jidu){
            url1<-str_c(url,"?year=",yr,"&jidu=",qt)
            update.index<-rbind(update.index,data.frame(yr,qt,url1,stringsAsFactors = F))
          } 
        }
      }
      names(update.index)<-c("year","qtr","urls")
      update.index <-update.index[order(update.index$urls),]
      X=data.frame(NULL)
      for(i in 1:nrow(update.index)){
        cat("从 finance.sina.com 上更新 ",
            update.index$year[i],"年",update.index$qtr[i],"季度的数据，完成率为",round(i/nrow(update.index),3)*100,"%\n")
        all.tables=tryCatch(readHTMLTable(update.index$urls[i],header=F,verbose=T),
                            error=function(e) list(table=data.table(NULL)))
        pick<-sapply(1:length(all.tables),function(i)ncol(all.tables[[i]]))
        fg.table<-which(pick==8)
        fg.table <-ifelse(length(fg.table)==0,0,max(which(pick==8)))
        if(fg.table==0) {
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
      X.new <- rbind(X.old,X)
      X.new<-X.new[!duplicated(X.new$"日期"),]
      dbWriteTable(con,file,X.new,overwrite=T)
      dbDisconnect(con)
      return(X.new)
    }
    
  }
}

QMupdate_OHLCVA_History_Group <- function (codelist,database="QM_Data.sqlite") {
  log<-data.frame(NULL)
  for (i in 1:length(codelist)){
    d=0
    repeat{
      stockdata<-tryCatch(QMupdate_OHLCVA_History(codelist[i]),error=function(e) NULL)
      d=d+1
      cat("更新股票",codelist[i],"的数据，尝试第",d,"次 \n")
      if(!is.null(stockdata)|d>=10) break
    } 
    log<-rbind(log,data.frame(codelist[i],d,ifelse(d<10,"成功","失败"),stringsAsFactors = F))
  }
  Sys.sleep(1)
  names(log)<-c("股票代码","尝试更新次数","是否成功")
  return(log)
}


