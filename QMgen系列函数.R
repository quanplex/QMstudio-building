QMgen_test_Qstick <- function(X,n=4,cv){
  # X : A1 型的抽样矩阵
  # n : 计算信号时利用的滞后天数
  # cv: 制作0-1 信号时的控制截取的critical value 
  X <- fill(X,开盘价:复权因子,.direction="up")
  Y <- X[,{
    G <- copy(.SD)
    close <- SMA(rev(收盘价/复权因子),n=n)
    open <-  SMA(rev(开盘价/复权因子),n=n)
    G <- G[,sg:=rev(close-open)]
    G <- G[1:n,]
  },by=.(预测日期,stockcode)]
  Z <-Y[,{
    fg <- QMbake_cross(sg,cv)
    fg.up <- fg$up
    fg.dn <- fg$down
    if (!is.na(fg.up) & !is.na(fg.dn)){
      SGratio <- ifelse(fg.up < fg.dn, sg[fg.up],0)
      SG01 <- ifelse(fg.up < fg.dn, 1,0)
    }else{
      if (!is.na(fg.up) & is.na(fg.dn)){
        SGratio <- sg[fg.up]
        SG01 <- 1
      }else{
        SGratio <-0
        SG01 <-0
      }
    }
    list(股票表现=股票表现, Qstick_ratio=SGratio,Qstick_deter=SG01)
  },by=.(预测日期,stockcode)]
  path <- str_replace_all(Sys.time(),"( |\\:)","_")
  dir.create(path)
  QMtaste_SignalPerf(Z,"Qstick_ratio",type="ratio",file=path)
  QMtaste_SignalPerf(Z,"Qstick_deter",type="0-1",file=path)
  explore <- str_c("n=",deparse(n),"\n","cv=",deparse(cv),"\n")
  write(explore,file=str_c(str_c("./",path,"/"),"产生信号的函数.txt"))
  write(deparse(body()),file=str_c(str_c("./",path,"/"),"产生信号的函数.txt"),append = T)
  return(Z)
}

QMgen_test_Rsquare <- function(X, n=75, cv.rsq=0.3, cv.coe=0.5){
  # X : A1 型的抽样矩阵
  # n : 计算信号时利用的滞后天数
  # cv.rsq: 制作0-1 信号时的针对 R2 截取的 critical value 
  # cv.coe: 制作0-1 信号时的针对 回归斜率 截取的 critical value
  X <- X[,{
    G <- copy(.SD)
    G <- G[1:n,]
  },by=.(预测日期,stockcode)]
  
  Y <- X[,{
    G <- copy(.SD)
    close <- rev(收盘价/复权因子)
    ols <- lm(close~c(1:n))
    rsq <- summary(ols)$r.squared 
    coe <- ols$coefficient[2]
    G <- G[,c("rsq","coe"):=list(rsq,coe)]
    G <-G[1,]
  },by=.(预测日期,stockcode)]
  
  Z <- Y[,Rsquare_deter:=ifelse(rsq> cv.rsq & coe> cv.coe,1,0)]
  path <- str_replace_all(Sys.time(),"( |\\:)","_")
  dir.create(path)
  QMtaste_SignalPerf(Z,"Rsquare_deter",type="0-1",file=path)
  explore <- str_c("n=",deparse(n),"\n","cv.rsq=",deparse(cv.rsq),"\n","cv.coe=",deparse(cv.coe))
  write(explore,file=str_c(str_c("./",path,"/"),"产生信号的函数.txt"))
  write(deparse(body()),file=str_c(str_c("./",path,"/"),"产生信号的函数.txt"),append = T)
  return(Z)
}

QMgen_test_MACD <- function(X,par=c(12,26,9),N.min=5){
  # X : A1 型的抽样矩阵
  # par : 计算MACD(a,b,c)时的 a,b,c 三元组参数向量
  # N.min: 计算支撑线的最小窗口长度
  #====================
  X <- fill(X,开盘价:复权因子,.direction="up")
  
  X <- X[,{
    G <- copy(.SD)
    close <- rev(收盘价/复权因子)
    DIFF <- MACD(close, nFast=par[1],nSlow=par[2],nSig=par[3],maType="SMA")[,1]
    EDA  <- MACD(close, nFast=par[1],nSlow=par[2],nSig=par[3],maType="SMA")[,2]
    macd.prime=DIFF-EDA
    G <- G[,c("DIFF","EDA","macd.prime"):=list(rev(DIFF),rev(EDA),rev(macd.prime))]
  },by=.(预测日期,stockcode)]
  
  Z <-X[,{
    y<-rev(macd.prime[1:10])
    fg <- QMbake_cross(macd.prime,0,winsize = 1)
    fg.up <- fg$up
    fg.dn <- fg$down
    if (!is.na(fg.up)){
      # close<-收盘价/复权因子
      # close<-close[1:7]
      # close<-rev(close)
      # sr <-lm(close~ c(1:7))$coefficient[2]
      MACDrt <-lm(y~ c(1:10))$coefficient[2]
      # MACD01 <- ifelse(!any(rev(EDA[1:7])[(fg.up+1):7]-close[(fg.up+1):7]>0) 
      #                  & DIFF[fg.up]>0
      #                  & MACDrt>0.4
      #                  & sr<0.3,1,0)
      MACD01 <- ifelse(macd.prime[1]>0,1,0)
    }else{
      MACDrt <-0
      MACD01<-0
    }
    list(股票表现=股票表现[1], MACD_ratio=MACDrt,MACD_deter=MACD01)
  },by=.(预测日期,stockcode)]
  path <- str_replace_all(Sys.time(),"( |\\:)","_")
  dir.create(path)
  QMtaste_SignalPerf(Z,"MACD_ratio",type="ratio",file=path)
  QMtaste_SignalPerf(Z,"MACD_deter",type="0-1",file=path)
  explore <- str_c("par=",deparse(par),"\n","N.min=",deparse(N.min),"\n")
  write(explore,file=str_c(str_c("./",path,"/"),"产生信号的函数.txt"))
  write(deparse(body()),file=str_c(str_c("./",path,"/"),"产生信号的函数.txt"),append = T)
  return(Z)
}

QMgen_test_WR <- function(X,par=c(a=13,b=34,c=89),cv=1,thd=-80){
  # X : A1 型的抽样矩阵
  # par : 计算WR短中长期参数(a,b,c)，一般建议参数为(13,34,89)
  #====================
  a<-par[1]
  b<-par[2]
  c<-par[3]
  X <- fill(x.1,开盘价:复权因子,.direction="up")
  X <- X[,c("close","high","low"):=list(收盘价/复权因子,
                                           最高价/复权因子,
                                           最低价/复权因子)]
  
  Y <- X[,{
    G <- copy(.SD)
    WR13<-(-100)*(max(high[1:a])-close[1])/(max(high[1:a])-min(low[1:a]))
    #print(WR13)
    WR34<-(-100)*(max(high[1:b])-close[1])/(max(high[1:b])-min(low[1:b]))
    #print(WR34)
    WR89<-(-100)*(max(high[1:c])-close[1])/(max(high[1:c])-min(low[1:c]))
    G[,c("WR13","WR34","WR89"):=list(WR13,WR34,WR89)]
    G<-G[1,]
  },by=.(预测日期,stockcode)]
  
  #===================
  
  Z <-Y[,{
    ch.cond <- abs(WR13-WR34)<cv & abs(WR34-WR89)<cv & abs(WR13-WR89) <cv 
    WR01 <- ifelse( !any(c(WR13,WR34,WR89)>thd) & ch.cond, 1,0)
    WRrt <- -WR01*WR13
    list(股票表现=股票表现[1], WilliamR_ratio=WRrt,WilliamR_deter=WR01)
  },by=.(预测日期,stockcode)]
  
  
  path <- str_replace_all(Sys.time(),"( |\\:)","_")
  dir.create(path)
  QMtaste_SignalPerf(Z,"WilliamR_deter",type="0-1",file=path)
  QMtaste_SignalPerf(Z,"WilliamR_ratio",type="ratio",file=path)
  explore <- str_c("par=",deparse(par),"\n","cv=",deparse(cv),"\n",
                   "thd=",deparse(thd),"\n")
  write(explore,file=str_c(str_c("./",path,"/"),"产生信号的函数.txt"))
  write(deparse(body()),file=str_c(str_c("./",path,"/"),"产生信号的函数.txt"),append = T)
}

QMgen_test_早晨十字星 <- function(X,wdw){
  # X: A2型抽样矩阵
  # wdw: 判断下跌的时间窗口长度
  X <- X[,{
    G <-copy(.SD)
    y <- cl[3:(wdw+3)]
    β <- lm(rev(y)~ c(3:(wdw+3)))$coefficient[2]
    zcszx01<-ifelse(cl[3]<op[3] & op[2]<cl[2] & abs(op[2]-cl[2])<0.1 & cl[1]>op[1] & cl[1]>cl[3]
                    & β<0,
                    1,
                    0)
    G <-G[1,]
    G <-G[,早晨十字星_deter:=zcszx01]
  }
         ,by=.(预测日期,stockcode)]
  path <- str_replace_all(Sys.time(),"( |\\:)","_")
  dir.create(path)
  QMtaste_SignalPerf(X,"早晨十字星_deter",type="0-1",file=path)
  explore <- str_c("wdw=",deparse(wdw))
  write(explore,file=str_c(str_c("./",path,"/"),"产生信号的函数.txt"))
  write(deparse(body()),file=str_c(str_c("./",path,"/"),"产生信号的函数.txt"),append = T)
  return(X)
}

QMgen_test_希望之星 <- function(X,wdw=5,k=1/3){
  # X: A2型抽样矩阵
  # wdw: 判断下跌的时间窗口长度
  # k: 启明星K线长度占头一天阴线的比率
  X <- X[,{
    G <-copy(.SD)
    y <- cl[3:(wdw+3)]
    β <- lm(rev(y)~ c(3:(wdw+3)))$coefficient[2]
    xwzx01<-ifelse(cl[3]<op[3] & abs(cl[3]-op[3])/(hi[3]-lo[3])>0.8 & op[2]<cl[2] 
                   & abs(op[2]-cl[2])/abs(op[3]-cl[3])<k & cl[1]>op[1] & cl[1]>cl[3]
                   & β<0,
                    1,
                    0)
    G <-G[1,]
    G <-G[,希望之星_deter:=xwzx01]
  }
  ,by=.(预测日期,stockcode)]
  path <- str_replace_all(Sys.time(),"( |\\:)","_")
  dir.create(path)
  QMtaste_SignalPerf(X,"希望之星_deter",type="0-1",file=path)
  explore <- str_c("wdw=",deparse(wdw))
  write(explore,file=str_c(str_c("./",path,"/"),"产生信号的函数.txt"))
  write(deparse(body()),file=str_c(str_c("./",path,"/"),"产生信号的函数.txt"),append = T)
  return(X)
}

QMgen_test_好友反攻 <- function(X,wdw=5,rd=0.05,ru=0.03,cv=0.1){
  # X: A2型抽样矩阵
  # wdw: 判断下跌的时间窗口长度
  # rd: 前一天大阴线的判定标准
  # ru: 当天中阳线的判定标准
  # cv: K线相近的标准
  X <- X[,{
    G <-copy(.SD)
    y <- cl[2:(wdw+2)]
    β <- lm(rev(y)~ c(2:(wdw+2)))$coefficient[2]
    hyfg01<-ifelse(cl[2]<op[2] & (op[2]-cl[2])/op[2]>rd
                   & op[1]<cl[2]
                   & cl[1]>op[1]
                   & (cl[1]-op[1])/op[1]>ru
                   & abs(cl[2]-cl[1])/cl[2]< cv
                   & β<0,
                   1,
                   0)
    G <-G[1,]
    G <-G[,好友反攻_deter:=hyfg01]
  }
  ,by=.(预测日期,stockcode)]
  path <- str_replace_all(Sys.time(),"( |\\:)","_")
  dir.create(path)
  QMtaste_SignalPerf(X,"好友反攻_deter",type="0-1",file=path)
  explore <- str_c("wdw=",deparse(wdw),"\n rd=",deparse(rd),"\n ru=",deparse(ru),"\n cv=",deparse(cv))
  write(explore,file=str_c(str_c("./",path,"/"),"产生信号的函数.txt"))
  write(deparse(body()),file=str_c(str_c("./",path,"/"),"产生信号的函数.txt"),append = T)
  return(X)
}

QMgen_test_曙光初现 <- function(X,wdw=5,rd=0.02,ru=0.01){
  # X: A2型抽样矩阵
  # wdw: 判断下跌的时间窗口长度
  # rd: 前一天大阴线的判定标准
  # ru: 当天中阳线的判定标准
  X <- X[,{
    G <-copy(.SD)
    y <- cl[3:(wdw+3)]
    β <- lm(rev(y)~ c(3:(wdw+3)))$coefficient[2]
    sgcx01<-ifelse(cl[2]<op[2] & (op[2]-cl[2])/op[2]>rd
                   & op[1]<cl[2]
                   & cl[1]>op[1]
                   & (cl[1]-op[1])/op[1]>ru
                   & cl[1]>op[2]*0.1+cl[2]*(1-0.1)
                   & !is.na(β),
                   1,
                   0)
    G <-G[1,]
    G <-G[,曙光初现_deter:=sgcx01]
  }
  ,by=.(预测日期,stockcode)]
  path <- str_replace_all(Sys.time(),"( |\\:)","_")
  dir.create(path)
  QMtaste_SignalPerf(X,"曙光初现_deter",type="0-1",file=path)
  explore <- str_c("wdw=",deparse(wdw),"\n rd=",deparse(rd),"\n ru=",deparse(ru))
  write(explore,file=str_c(str_c("./",path,"/"),"产生信号的函数.txt"))
  write(deparse(body()),file=str_c(str_c("./",path,"/"),"产生信号的函数.txt"),append = T)
  return(X)
}

QMgen_test_旭日东升 <- function(X,wdw=5,rd=0.02,ru=0.02){
  # X: A2型抽样矩阵
  # wdw: 判断下跌的时间窗口长度
  # rd: 前一天大阴线的判定标准
  # ru: 当天中阳线的判定标准
  X <- X[,{
    G <-copy(.SD)
    y <- cl[3:(wdw+3)]
    β <- lm(rev(y)~ c(3:(wdw+3)))$coefficient[2]
    xrds01<-ifelse(cl[2]<op[2] & (op[2]-cl[2])/op[2]>rd
                   & cl[1]>op[1] & (cl[1]-op[1])/op[1]>ru
                   & cl[1]>op[2]
                   & β<0,
                   1,
                   0)
    G <-G[1,]
    G <-G[,旭日东升_deter:=xrds01]
  }
  ,by=.(预测日期,stockcode)]
  path <- str_replace_all(Sys.time(),"( |\\:)","_")
  dir.create(path)
  QMtaste_SignalPerf(X,"旭日东升_deter",type="0-1",file=path)
  explore <- str_c("wdw=",deparse(wdw),"\n rd=",deparse(rd),"\n ru=",deparse(ru))
  write(explore,file=str_c(str_c("./",path,"/"),"产生信号的函数.txt"))
  write(deparse(body()),file=str_c(str_c("./",path,"/"),"产生信号的函数.txt"),append = T)
  return(X)
}

QMgen_test_倒锤头线 <- function(X,wdw=5,δ=0.1){
  # X: A2型抽样矩阵
  # wdw: 判断下跌的时间窗口长度
  # δ: K线下部引线消失的标准
  X <- X[,{
    G <-copy(.SD)
    y <- cl[1:(wdw+1)]
    β <- lm(rev(y)~ c(1:(wdw+1)))$coefficient[2]
    head <- hi[1]-max(op[1],cl[1])
    body <- abs(op[1]-cl[1])
    rear <- min(op[1],cl[1])-lo[1]
    dctx01<-ifelse(head>2*body
                   & rear<δ*body
                   & β<0,
                   1,
                   0)
    G <-G[1,]
    G <-G[,倒锤头线_deter:=dctx01]
  }
  ,by=.(预测日期,stockcode)]
  path <- str_replace_all(Sys.time(),"( |\\:)","_")
  dir.create(path)
  QMtaste_SignalPerf(X,"倒锤头线_deter",type="0-1",file=path)
  explore <- str_c("wdw=",deparse(wdw),"\n δ=",deparse(δ))
  write(explore,file=str_c(str_c("./",path,"/"),"产生信号的函数.txt"))
  write(deparse(body()),file=str_c(str_c("./",path,"/"),"产生信号的函数.txt"),append = T)
  return(X)
}

QMgen_test_锤头线 <- function(X,wdw=5,δ=0.1){
  # X: A2型抽样矩阵
  # wdw: 判断下跌的时间窗口长度
  # δ: K线上部引线消失的标准
  X <- X[,{
    G <-copy(.SD)
    y <- cl[1:(wdw+1)]
    β <- lm(rev(y)~ c(1:(wdw+1)))$coefficient[2]
    head <- hi[1]-max(op[1],cl[1])
    body <- abs(op[1]-cl[1])
    rear <- min(op[1],cl[1])-lo[1]
    ctx01<-ifelse(rear>2*body
                   & head<δ*body
                   & β<0,
                   1,
                   0)
    G <-G[1,]
    G <-G[,锤头线_deter:=ctx01]
  }
  ,by=.(预测日期,stockcode)]
  path <- str_replace_all(Sys.time(),"( |\\:)","_")
  dir.create(path)
  QMtaste_SignalPerf(X,"锤头线_deter",type="0-1",file=path)
  explore <- str_c("wdw=",deparse(wdw),"\n δ=",deparse(δ))
  write(explore,file=str_c(str_c("./",path,"/"),"产生信号的函数.txt"))
  write(deparse(body()),file=str_c(str_c("./",path,"/"),"产生信号的函数.txt"),append = T)
  return(X)
}

QMgen_test_钳子底 <- function(X,wdw=5,δ=0.01,η=0.02,k=1/3){
  # X: A2型抽样矩阵
  # wdw: 判断下跌的时间窗口长度
  # δ: 两根K线尾部的相等的标准
  # η: K线body实体大小标准
  # k: 下引线伸出的长度标准
  X <- X[,{
    G <-copy(.SD)
    y <- cl[1:(wdw+1)]
    β <- lm(rev(y)~ c(1:(wdw+1)))$coefficient[2]
    head1 <- hi[1]-max(op[1],cl[1])
    body1 <- abs(op[1]-cl[1])
    rear1 <- min(op[1],cl[1])-lo[1]
    head2 <- hi[2]-max(op[2],cl[2])
    body2 <- abs(op[2]-cl[2])
    rear2 <- min(op[2],cl[2])-lo[2]
    qzd01<-ifelse(abs(lo[1]-lo[2])<δ*lo[2]
                  & abs(op[2]-cl[2])>η*min(op[2],cl[2])
                  & abs(op[1]-cl[1])>η*min(op[1],cl[1])
                  & rear2>k*body2
                  & rear1>k*body1
                  & β<0,
                  1,
                  0)
    G <-G[1,]
    G <-G[,钳子底_deter:=qzd01]
  }
  ,by=.(预测日期,stockcode)]
  path <- str_replace_all(Sys.time(),"( |\\:)","_")
  dir.create(path)
  QMtaste_SignalPerf(X,"钳子底_deter",type="0-1",file=path)
  explore <- str_c("wdw=",deparse(wdw),"\n δ=",deparse(δ),"\n η=",deparse(η),"\n k=",deparse(k))
  write(explore,file=str_c(str_c("./",path,"/"),"产生信号的函数.txt"))
  write(deparse(body()),file=str_c(str_c("./",path,"/"),"产生信号的函数.txt"),append = T)
  return(X)
}