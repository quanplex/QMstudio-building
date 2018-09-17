tablename_HQ_stock <- na.omit(str_extract(tablelist,"数据表：股票行情历史.*")) 
X <- data.frame(NULL)
for (i in tablename_HQ_stock){
  X.1 <- dbReadTable(con, i)
  if (nrow(X.1)==0) next
  code <- str_extract(i,"\\d+")
  X.1 <- cbind(str_c("CN",code),X.1); names(X.1)[1]<- "stockcode"
  cat("正在读取股票",code,"的数据\n")
  X <- rbind(X,X.1)
}
X <- X[,.SD[order(日期),],by=.(stockcode)]
dbWriteTable(con,"LL数据表：股票行情数据全表|201804",X)