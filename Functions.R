#######Confim data read in correctly

input <- function(df){
  d<-dim(df)
  c<-class(df)
  c2<-sapply(df, class)
  ins<-list(d, c, c2)
  return(ins)
}

#########Multiplot on one page

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


####Split list that includes test and train into 

###Train
train<- function(x){
  ###Loop through all versions of x
  for(i in length(x)){  
    ###Take first item of list
    train <- list(x[[1]])
    ###Return all first items in a list
    ret <- list (train)
    return(ret)
  }
}

###Test
test<- function(x){
  ###Loop through all versions of x
  for(i in length(x)){  
    ###Take second item of list
    test <- list(x[[2]])
    ###Return all second items in a list
    ret <- list (test)
    return(ret)
  }
}

###Function to bind and create source
AppendMe <- function(dfNames) {
  do.call(rbind, lapply(dfNames, function(x) {
    cbind(get(x), source = x)
  }))
}


qfun<- function(list){
  q <- lapply(list, unlist, function(x){cbind(get(x), source = x)
  })
}

# mean.k function
mean.k=function(x) {
  if (is.numeric(x)) round(mean(x, na.rm=TRUE), digits = 2)
  else "N*N"
}

# median.k function
median.k=function(x) {
  if (is.numeric(x)) round(median(x, na.rm=TRUE), digits = 2)
  else "N*N"
}

# sd.k function
sd.k=function(x) {
  if (is.numeric(x)) round(sd(x, na.rm=TRUE), digits = 2)
  else "N*N"
}

# min.k function
min.k=function(x) {
  if (is.numeric(x)) round(min(x, na.rm=TRUE), digits = 2)
  else "N*N"
}

# max.k function
max.k=function(x) {
  if (is.numeric(x)) round(max(x, na.rm=TRUE), digits = 2)
  else "N*N"
}

###########################################################

# sumstats function #

sumstats=function(x) {  # start function sumstats
  sumtable = cbind(as.matrix(colSums(!is.na(x))),
                   sapply(x,mean.k),
                   sapply(x,median.k),
                   sapply(x,sd.k),
                   sapply(x,min.k),
                   sapply(x,max.k))
  sumtable=as.data.frame(sumtable)
  names(sumtable)=c("Obs","Mean","Median","Std.Dev","min","MAX")
  sumtable
}                       # end function sumstats

correlate <- function(df){
  c <- cor(df[sapply(df, is.numeric)])
  corrplot(c)
}


# f1 maps (0,infinity) to (0,1)
f1 <- function(x,a,b)
{
  eax <- exp(a*x)
  if (eax == Inf)
    f1eax <- 1
  else
    f1eax <- (eax-1)/(eax+b)
  return(f1eax)
}

# f2 maps (0,1) onto (0,1)
f2 <- function(x,a,b)
{
  eax <- exp(a*x)
  ea <- exp(a)
  return((eax-1)/(eax+b)*(ea+b)/(ea-1))
}

# decomposition data - detrend & deseasonal
decomp <- function(x,transform=TRUE)
{
  require(forecast)
  # Transform series
  if(transform & min(x,na.rm=TRUE) >= 0)
  {
    lambda <- BoxCox.lambda(na.contiguous(x))
    x <- BoxCox(x,lambda)
  }
  else
  {
    lambda <- NULL
    transform <- FALSE
  }
  # Seasonal data
  if(frequency(x)>1)
  {
    x.stl <- stl(x,s.window="periodic",na.action=na.contiguous)
    trend <- x.stl$time.series[,2]
    season <- x.stl$time.series[,1]
    remainder <- x - trend - season
  }
  else #Nonseasonal data
  {
    require(mgcv)
    tt <- 1:length(x)
    trend <- rep(NA,length(x))
    trend[!is.na(x)] <- fitted(gam(x ~ s(tt)))
    season <- NULL
    remainder <- x - trend
  }
  return(list(x=x,trend=trend,season=season,remainder=remainder,transform=transform,lambda=lambda))
}

# === function to find frequency from time series data ====
find.freq <- function(x)
{
  n <- length(x)
  spec <- spec.ar(c(na.contiguous(x)),plot=FALSE)
  if(max(spec$spec)>10) # Arbitrary threshold chosen by trial and error.
  {
    period <- round(1/spec$freq[which.max(spec$spec)])
    if(period==Inf) # Find next local maximum
    {
      j <- which(diff(spec$spec)>0)
      if(length(j)>0)
      {
        nextmax <- j[1] + which.max(spec$spec[j[1]:500])
        if(nextmax <= length(spec$freq))
          period <- round(1/spec$freq[nextmax])
        else
          period <- 1
      }
      else
        period <- 1
    }
  }
  else
    period <- 1
  
  return(period)
}

# === Following function computes all measures

measures <- function(x)
{
  require(forecast)
  
  N <- length(x)
  freq <- find.freq(x)
  fx <- c(frequency=(exp((freq-1)/50)-1)/(1+exp((freq-1)/50)))
  x <- ts(x,f=freq)
  
  # Decomposition
  decomp.x <- decomp(x)
  
  # Adjust data
  if(freq > 1)
    fits <- decomp.x$trend + decomp.x$season
  else # Nonseasonal data
    fits <- decomp.x$trend
  adj.x <- decomp.x$x - fits + mean(decomp.x$trend, na.rm=TRUE)
  
  # Backtransformation of adjusted data
  if(decomp.x$transform)
    tadj.x <- InvBoxCox(adj.x,decomp.x$lambda)
  else
    tadj.x <- adj.x
  
  # Trend and seasonal measures
  # avoids the divide by zero problem by testing if the variances are close to zero first
  v.adj <- var(adj.x, na.rm=TRUE)
  if(freq > 1)
  {
    detrend <- decomp.x$x - decomp.x$trend
    deseason <- decomp.x$x - decomp.x$season
    trend <- ifelse(var(deseason,na.rm=TRUE) < 1e-10, 0, 
                    max(0,min(1,1-v.adj/var(deseason,na.rm=TRUE))))
    season <- ifelse(var(detrend,na.rm=TRUE) < 1e-10, 0,
                     max(0,min(1,1-v.adj/var(detrend,na.rm=TRUE))))
  }
  else #Nonseasonal data
  {
    trend <- ifelse(var(decomp.x$x,na.rm=TRUE) < 1e-10, 0,
                    max(0,min(1,1-v.adj/var(decomp.x$x,na.rm=TRUE))))
    season <- 0
  }
  
  m <- c(fx,trend,season)
  
  # Measures on original data
  xbar <- mean(x,na.rm=TRUE)
  s <- sd(x,na.rm=TRUE)
  
  # Serial correlation
  Q <- Box.test(x,lag=10)$statistic/(N*10)
  fQ <- f2(Q,7.53,0.103)
  
  # Nonlinearity
  p <- terasvirta.test(na.contiguous(x))$statistic
  fp <- f1(p,0.069,2.304)
  
  # Skewness
  sk <- abs(mean((x-xbar)^3,na.rm=TRUE)/s^3)
  fs <- f1(sk,1.510,5.993)
  
  # Kurtosis
  k <- mean((x-xbar)^4,na.rm=TRUE)/s^4
  fk <- f1(k,2.273,11567)
  
  # Hurst=d+0.5 where d is fractional difference.
  H <- fracdiff(na.contiguous(x),0,0)$d + 0.5
  
  # Lyapunov Exponent
  if(freq > N-10)
    stop("Insufficient data")
  Ly <- numeric(N-freq)
  for(i in 1:(N-freq))
  {
    idx <- order(abs(x[i] - x))
    idx <- idx[idx < (N-freq)]
    j <- idx[2]
    Ly[i] <- log(abs((x[i+freq] - x[j+freq])/(x[i]-x[j])))/freq
    if(is.na(Ly[i]) | Ly[i]==Inf | Ly[i]==-Inf)
      Ly[i] <- NA
  }
  Lyap <- mean(Ly,na.rm=TRUE)
  fLyap <- exp(Lyap)/(1+exp(Lyap))
  
  m <- c(m,fQ,fp,fs,fk,H,fLyap)
  
  # Measures on adjusted data
  xbar <- mean(tadj.x, na.rm=TRUE)
  s <- sd(tadj.x, na.rm=TRUE)
  
  # Serial
  Q <- Box.test(adj.x,lag=10)$statistic/(N*10)
  fQ <- f2(Q,7.53,0.103)
  
  # Nonlinearity
  p <- terasvirta.test(na.contiguous(adj.x))$statistic
  fp <- f1(p,0.069,2.304)
  
  # Skewness
  sk <- abs(mean((tadj.x-xbar)^3,na.rm=TRUE)/s^3)
  fs <- f1(sk,1.510,5.993)
  
  # Kurtosis
  k <- mean((tadj.x-xbar)^4,na.rm=TRUE)/s^4
  fk <- f1(k,2.273,11567)
  
  m <- c(m,fQ,fp,fs,fk)
  names(m) <- c("frequency", "trend","seasonal",
                "autocorrelation","non-linear","skewness","kurtosis","Hurst","Lyapunov",
                "dc autocorrelation","dc non-linear","dc skewness","dc kurtosis")
  
  return(m)
}


# cs is the vector of complex points to convert
convert.fft <- function(cs, sample.rate=1) {
  cs <- cs / length(cs) # normalize
  
  distance.center <- function(c)signif( Mod(c),        4)
  angle           <- function(c)signif( 180*Arg(c)/pi, 3)
  
  df <- data.frame(cycle    = 0:(length(cs)-1),
                   freq     = 0:(length(cs)-1) * sample.rate / length(cs),
                   strength = sapply(cs, distance.center),
                   delay    = sapply(cs, angle))
  df
}

plot.frequency.spectrum <- function(X.k, xlimits=c(0,length(X.k))) {
  plot.data  <- cbind(0:(length(X.k)-1), Mod(X.k))
  
  # TODO: why this scaling is necessary?
  plot.data[2:length(X.k),2] <- 2*plot.data[2:length(X.k),2] 
  
  plot(plot.data, t="h", lwd=2, main="", 
       xlab="Frequency (Hz)", ylab="Strength", 
       xlim=xlimits, ylim=c(0,max(Mod(plot.data[,2]))))
}

# Plot the i-th harmonic
# Xk: the frequencies computed by the FFt
#  i: which harmonic
# ts: the sampling time points
# acq.freq: the acquisition rate
plot.harmonic <- function(Xk, i, ts, acq.freq, color="red") {
  Xk.h <- rep(0,length(Xk))
  Xk.h[i+1] <- Xk[i+1] # i-th harmonic
  harmonic.trajectory <- get.trajectory(Xk.h, ts, acq.freq=acq.freq)
  points(ts, harmonic.trajectory, type="l", col=color)
}

###Create rule to identify rain with simple threshold
rule <- function(x){
  prediction <- rep(NA, length(x))
  prediction[x > 0] <- 1
  prediction[x <= 0] <- 0
  return(prediction)
}

###Apply rule
s<-table(rule(training1$log), training1$rain_lenny)

tn <- s[1]
fn <- s[3]
fp <- s[2]
tp <- s[4]

sensitivity <- tp/(tp+fn)
specificity <- tn/(fp+tn)
positive_predictive_value <- tp/(tp+fp)
negative_predictive_value <- tn/(fn+tn)
accuracy<- (tp+tn)/(tp+fp+fn+tn)

sensitivity
specificity
positive_predictive_value
negative_predictive_value
accuracy

stats2 <- cbind(sensitivity, specificity, positive_predictive_value, negative_predictive_value, accuracy)
stats2

###Caluclate accuracy and total correct
rbind("Rule 1" = c(Accuracy = mean(rule(training1$log)==training1$rain_lenny),
                   "Total Correct" = sum(rule(training1$log)==training1$rain_lenny)))
