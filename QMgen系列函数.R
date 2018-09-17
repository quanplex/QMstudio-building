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
  QMtaste_SignalPerf(Z,"Qstick_ratio",type="ratio")
  QMtaste_SignalPerf(Z,"Qstick_deter",type="0-1")
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
  QMtaste_SignalPerf(Z,"Rsquare_deter",type="0-1")
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
    y <-rev(macd.prime[1:3])
    fg <- QMbake_cross(macd.prime,0,winsize = 4)
    fg.up <- fg$up
    fg.dn <- fg$down
    if (!is.na(fg.up)){
      ols <- lm(rev(macd.prime[1:10])~c(1:10))
      MACDrt <- ols$coefficient[2]
      #close <- 收盘价/复权因子;close <- close[1:30];close<-rev(close)
      #resist.slope <- QMbake_valleytrend(close,N.min=N.min)$confirm.trend
      #MACD01 <- ifelse(!is.na(resist.slope) & resist.slope<=0 & EDA[fg.up]>0,1,0)
      MACD01 <- ifelse(DIFF[fg.up]>0 & MACDrt>0.3,1,0)
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

