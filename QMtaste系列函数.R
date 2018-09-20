QMtaste_SignalPerf <-function(X,signal,type="0-1",file=NA){
  #X: 一个包含预测日期，股票代码，股票表现，若干信号的A1格式的表格
  #signal：进行探索性分析的signal名称，即X中的某列列名，字符串形式
  path <- str_replace_all(Sys.time(),"( |\\:)","_")
  data=X[,list(predictdate=预测日期,stockcode=stockcode,performance=股票表现,signal=X[[signal]])]
  if(is.na(file)){
    filename=str_c("./",path,"/",signal)
  } else{
    filename=str_c("./",file,"/",signal)
  }
 
  if(type=="0-1"){
    data <- data[,signal:=ifelse(signal==1,"R","D")]
    tb <- table(data$signal, data$performance)
    sen <- confusionMatrix(tb)[["byClass"]][["Sensitivity"]]
    spc <- confusionMatrix(tb)[["byClass"]][["Specificity"]]
    acc <- confusionMatrix(tb)[["byClass"]][["Balanced Accuracy"]]
    ppv <- confusionMatrix(tb)[["byClass"]][["Pos Pred Value"]]
    npv <- confusionMatrix(tb)[["byClass"]][["Neg Pred Value"]]
    cat("总体精确度为 ",acc,"\n")
    cat("真阳性率为 ",spc,"\n")
    cat("阳性预测率 ",npv,"\n")
    cat("阴性预测率 ",ppv,"\n")
    print(filename)
    write(str_c("总体精确度为 ",acc,"\n真阳性率为 ",spc,"\n阳性预测率 ",npv,"\n阴性预测率 ",ppv),
          file=str_c(filename,"_总体效果评估报告.txt"))
    data.1 <- data[,{
      signal[signal==1]<-"R"
      signal[signal==0]<-"D"
      tb <-table(factor(signal,levels = c("D","R")),factor(performance,levels=c("D","R")))
      sen <- confusionMatrix(tb)[["byClass"]][["Sensitivity"]]
      spc <- confusionMatrix(tb)[["byClass"]][["Specificity"]]
      acc <- confusionMatrix(tb)[["byClass"]][["Balanced Accuracy"]]
      ppv <- confusionMatrix(tb)[["byClass"]][["Pos Pred Value"]]
      npv <- confusionMatrix(tb)[["byClass"]][["Neg Pred Value"]]
      list(sen=sen,spc=spc,acc=acc,ppv=ppv,npv=npv)
    },by=.(predictdate)]
    p<-ggplot(data.1,aes(x=as.Date(predictdate),y=acc))+geom_line()+ylab("准确率")+labs(x="Date",y="rate")
    ggsave(filename=str_c(filename,"_准确率_bydate.pdf"),plot=p,width=5,height=4,limitsize = F) 
    p<-ggplot(data.1,aes(x=as.Date(predictdate),y=spc))+geom_line()+ylab("真阳性率")+labs(x="Date",y="rate")
    ggsave(filename=str_c(filename,"_真阳性率_bydate.pdf"),plot=p,width=5,height=4,limitsize = F) 
    p<-ggplot(data.1,aes(x=as.Date(predictdate),y=sen))+geom_line()+ylab("真阴性率")+labs(x="Date",y="rate")
    ggsave(filename=str_c(filename,"_真阴性率_bydate.pdf"),plot=p,width=5,height=4,limitsize = F) 
    p<-ggplot(data.1,aes(x=as.Date(predictdate),y=npv))+geom_line()+ylab("阳性预测率")+labs(x="Date",y="rate")
    ggsave(filename=str_c(filename,"_阳性预测率_bydate.pdf"),plot=p,width=5,height=4,limitsize = F) 
    p<-ggplot(data.1,aes(x=as.Date(predictdate),y=ppv))+geom_line()+ylab("阴性预测率")+labs(x="Date",y="rate")
    ggsave(filename=str_c(filename,"_阴性预测率_bydate.pdf"),plot=p,width=5,height=4,limitsize = F) 
    
  }else{
    p<-ggplot(data,aes(x=signal,fill=performance))+geom_density(alpha=0.5)
    ggsave(filename=str_c(filename,"_density_total.pdf"),plot=p,width=5,height=4)
    p<-ggplot(data,aes(x=performance,y=signal))+geom_boxplot(width=0.5)+xlab("stock performance")
    ggsave(filename=str_c(filename,"_boxplot_total.pdf"),plot=p,width=5,height=4)
    p<-ggplot(data,aes(x=signal,fill=performance))+geom_density(alpha=0.5)+facet_wrap(~predictdate,ncol=10,scales="free")
    ggsave(filename=str_c(filename,"_density_bydate.pdf"),plot=p,width=20,height=16,limitsize = F) 
  }
}






