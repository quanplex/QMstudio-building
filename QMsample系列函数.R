QMsample <- function(X,Nstock=300,Ndate=100,Obs=100,st.S="2008-01-01",st.D="2009-01-01",seed=NULL){
  # X: data.table 格式的股市行情数据全表，日期还是字符形式
  # Nstock: 筛选出的股票数量
  # Ndate: 取样的预测日期数量
  # Obs: 用于预测的数据时间长度
  # st.S: 该日之前就有数据的股票才会被抽样，但是不能保证不会退市
  # st.D: 从该日起的日期才会被抽样成为预测日期（预测时间窗口的终点）
  # seed: 抽样的随机种子

  Z <- X[,{
    y <-ifelse(sort(日期)[1]<=st.S,1,0)
    list(够长=y)
  },by=.(stockcode)]
  if(!is.null(seed)) {set.seed(seed)}
  stock_sample <- sample(Z[够长==1,]$stockcode,300)
  Y <- X[stockcode %in% stock_sample,]
  # 转换日期为时间格式 ====
  Y <- Y[,日期:=as.Date(日期,tz="Asia/Shanghai")]
  # 裁出st.S之后的数据 ====
  Y <- Y[日期>=st.S,]
  # 随机筛选Ndeta天作为预测的5天收益的结算日
  date <-unique(Y$日期); date <- date[date>=st.D]
  date_sample <- sample(date,Ndate)
  Y.1 <- Y[日期 %in% date_sample,list(stockcode,日期)]
  setnames(Y.1,"日期","预测日期")
  # 构建以随机筛选的日期为收益结算日的可操作数据矩阵A1
  Y <- merge(Y.1,Y,allow.cartesian = T)
  i <- 0
  A1 <- Y[,{
    G<- copy(.SD)
    i <- i+1
    cat("正在处理第",i,"只股票\n")
    G[,{
      G2 <- copy(.SD)
      fg <- which(日期==预测日期)
      if(fg<=Obs+10){
        G2 <- cbind("N",G2[1,])
        names(G2)[1] <- "股票表现"
        G2
      }else{
        Adjclose <- 收盘价/复权因子
        ret <- log(Adjclose[fg])-log(Adjclose[fg-5])
        losschk <- (log(Adjclose[(fg-4):fg])-log(Adjclose[fg-5]))< 0.05*(-1)
        losschk <-ifelse(is.na(any(losschk)),1,any(losschk))
        if (losschk) ret <-"D"
        ret <- ifelse(ret>=0.15,"R","D")
        G2 <- G2[(fg-5):(fg-(Obs+5)),]
        G2 <- cbind(ret, G2)
        names(G2)[1] <- "股票表现"
        G2
      }
      
    },by=.(预测日期)]
  },by=.(stockcode)]
  A1 <- A1[股票表现!="N",]
}