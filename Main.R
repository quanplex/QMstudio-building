# 加载包 ====
library(plyr)
library(tidyverse)
library(stringr)
library(magrittr)
library(RCurl)
library(XML)
library(data.table)
library(jsonlite)
library(dtplyr)
library(rvest)
library(RSelenium)
library(knitr)
library(lubridate)
library(DBI)
library(RSQLite)
library(caret)
library(quantmod)
library(TTR)

# 加载程序 ====
source("QMget系列函数.R")
source("QMupdate系列函数.R")
source("QMtaste系列函数.R")
source("QMbake系列函数.R")
source("QMgen系列函数.R")

# 加载数据=====
con <- dbConnect(SQLite(),"QM_Data.sqlite")
tablelist <- dbListTables(con)
## 获取表类型 ====
type_table <- unique(str_extract(tablelist,"[\\x{4e00}-\\x{9fa5}]+"))
type_table <- unique(str_extract(tablelist,"[\\x{4e00}-\\x{9fa5}]+：[[\\x{4e00}-\\x{9fa5}]-]+"))
# 合并所有数据 ====
#source("task_合并全部股票.R")
#======================


X <- dbReadTable(con,"LL数据表：股票行情数据全表|201804")
X <- data.table(X)
# 进行抽样
A1 <- QMsample(X,seed="19810804")

#测试：30天内最高价与最低价只差的排序值作为信号
VX <- A1[,{
  H <-最高价/复权因子
  L <-最低价/复权因子
  sig1 <- max(H[1:30],na.rm=T)-min(L[1:30],na.rm=T)
  list(股票表现=股票表现,sig1=sig1)
},by=.(预测日期,stockcode)]
VX <-VX[!duplicated(VX),]
VX <-VX[,{
  list(stockcode=stockcode,股票表现=股票表现,sig1=rank(sig1)>5)
},by=.(预测日期)]

QMtaste_SignalPerf(VX,signal="sig1")

QMgen_test_Qstick(A1,4,0.6) 
QMgen_test_Rsquare(A1,75,0.3,0.3) 

lin<- function(a=5,b=6){
  write(str_c(deparse(a),"\n",deparse(b),"\n"),"see.txt")
  write(deparse(body()),"see.txt",append = T)
}
lin()

QMgen_test_MACD(A1,par=c(5,14,5),N.min=5)

