QMbake_cross <- function(y,ref,winsize=3){
  
  #y: 一列准备构建信号的向量
  #ref: 作为参考的基准信号向量
  #winsize：观察穿越的时间窗口大小
  
  trend <- y-ref
  chg <- sign(trend[1:winsize])-sign(trend[2:(winsize+1)])
  up <- which(chg==2)[1]
  down <- which(chg==-2)[1]
  return(list(up=up,down=down))
  # up: 上穿的位置
  # down: 下穿的位置
}

QMbake_speed_resistence <- function(y){
  #y: 一列准备构建信号价格向量
  N <- length(y)
  fg.max <- which.max(y)
  fg.min <- which.min(y)
  if (fg.min < fg.max){
    p.min <- y[fg.min]
    p.max.23 <- 2/3*y[fg.max]
    p.max.13 <- 1/3*y[fg.max]
    k.23 <- (p.max.23-p.min)/(fg.max-fg.min)
    k.13 <- (p.max.13-p.min)/(fg.max-fg.min)
    z.23 <- p.min+ seq(0,N-fg.min)*k.23
    z.13 <- p.min+ seq(0,N-fg.min)*k.13
    return(z.13=z.13,z.23=z.23)
  }else{
    return(z.13=rep(NA,(N-fg.min+1)),z.23=rep(NA,(N-fg.min+1)))
    # z.23 表示计算出的2/3 支撑线
    # z.13 表示计算出的1/3 支撑线
  }
}

QMbake_peaktrend <- function(y,N.min=10,offset=4){
  #y: 一列准备构建信号的向量
  #offset: 当最近的一天效果不理想时，允许替代的天数
  #N.min: 允许趋势的最小天数
  back <-0
  N=length(y)
  while(back <= offset){
    iscross <- sapply(1:(N-N.min-back),function(s) {
      trend=y[s]+seq(0,N-s-back)*(y[N-back]-y[s])/(N-back-s)
      x <- any(y[s:(N-back)]>trend)
      return(x)
      })
     fg <- which(!iscross)[1]
    if (!is.na(fg)) {
      confirm.trend=(y[N-back]-y[fg])/(N-back-fg)
      start=fg
      end=N-back
      break
    }else{
      confirm.trend=NA
      start=NA
      end=N-NA
    }
    back <- back+1
  }
  return(list(confirm.trend=confirm.trend,
              start=start,
              end=end
              ))
  # confirm.trend: 连接顶部的一条趋势线的斜率
  # start: 趋势计算的开始坐标
  # end: 趋势计算的终点坐
}


QMbake_valleytrend <- function(y,N.min=10,offset=4){
  #y: 一列准备构建信号向量
  #offset: 当最近的一天效果不理想时，允许替代的天数
  #N.min: 允许趋势的最小天数
  back <-0
  N=length(y)
  while(back <= offset){
    iscross <- sapply(1:(N-N.min-back),function(s) {
      trend=y[s]+seq(0,N-s-back)*(y[N-back]-y[s])/(N-back-s)
      x <- any(y[s:(N-back)]<trend)
      return(x)
    })
    fg <- which(!iscross)[1]
    if (!is.na(fg)) {
      confirm.trend=(y[N-back]-y[fg])/(N-back-fg)
      start=fg
      end=N-back
      break
    }else{
      confirm.trend=NA
      start=NA
      end=N-NA
    }
    back <- back+1
  }
  return(list(confirm.trend=confirm.trend,
              start=start,
              end=end
  ))
  # confirm.trend: 连接底部的一条趋势线的斜率
  # start: 趋势计算的开始坐标
  # end: 趋势计算的终点坐
}

