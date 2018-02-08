library(chebpol)
library(ggplot2)
library(plyr)
library(dplyr)
library(reshape2)
library(scales)

parseLifeTab <- function(fname)
{
  lines <- readLines(fname)
  lines <- lines[lines!=""]
  
  hdrs<-grep("\fUnited States", lines)
  years <- as.integer(gsub(".*calendar year (\\d\\d\\d\\d)","\\1",lines[hdrs]))
  sexs <- factor(gsub(".*percent interest for (\\S+)s .*","\\1",lines[hdrs]))
  
  df<-NULL
  
  for(i in hdrs) {
    hdridx <- which(hdrs==i)
    sex <- sexs[hdridx]
    year <- years[hdridx]
    
    x   <- gsub("^\\s*(\\d{1,3})\\s+(\\S+)\\s+\\S+\\s+\\S+\\s+\\S+\\s+\\S+\\s+(\\S+).*", "\\1,\\2,\\3",lines[(i+6):(i+65)])
    xx  <-strsplit(x, ",")
    age <- sapply(xx, function(y) as.double(y[1]))
    q   <- sapply(xx, function(y) as.double(y[2]))
    lex <- sapply(xx, function(y) as.double(y[3]))
    
    d <- data.frame(age=age,lex=lex,q=q,sex=sex,year=year)
    if(is.null(df)) {df<-d}
    else {df<-rbind(df,d)}
  }
  
  return(df)  
}

# Generates a function that generates ERA data
#   histFile - historic life expectancy
#   projFile - projected life expectancy
make.getERA <- function(histFile, projFile)
{
  d <- parseLifeTab(histFile)
  d <- rbind(d, parseLifeTab(projFile))
  o <- order(d$sex, d$year, d$age )
  d <- d[o,]
  lex_a <- array(d$lex, dim=c(120,201,2))
  
  # Life expectancy function
  lex_f <- chebappxg(lex_a, grid=list(0:119, 1900:2100, 1:2))
  
  age_f <- function(lex, year, sex)
  {
    r <- uniroot(function(age,lex,year,sex) {lex_f(c(age,year,sex))-lex}, interval=c(0,119), lex=lex, year=year, sex=sex)
    return(r$root)
  }
  
  relage_f <- function(c, year, sex)
  {
    r <- uniroot(function(age,lex,year,sex) {lex_f(c(age,year,sex))-c*(age-21)}, interval=c(22,118), lex=lex, year=year, sex=sex)
    return(r$root)
  }
  
  # getERA function that we return
  function(refage=65, refyear=2017, sex="male",from=1900,to=2100)
  {
    # 1 for f, 2 for m
    sex <- tolower(sex)
    nsex <- (sex=="male")+1
    
    r <- lex_f(c(refage,refyear,nsex))
    c <- r/(refage-21)
    aera <- sapply(from:to, function(x) age_f(r,x,nsex))
    rera <- sapply(from:to, function(x) relage_f(c,x,nsex))
    return(data.frame(year=from:to, aera=aera, rera=rera))
  }

}

make.getLFP <- function(lfpFile)
{
  lfpdf <- read.csv(lfpFile, row.names = NULL)
  
  function(years, ages, sex="Male")
  {
    
    if(length(years) != length(ages)) {
      stop("getLFP: years and ages have to have same length")
    }
    tmp <- merge(data.frame(year=years, age=ages), 
                 lfpdf[lfpdf$sex==sex,], 
                 by=c("year","age"), 
                 all=F
    )
    return(tmp$lfp)
  }
}

plotERA <- function(refage, refyear, sex, from, to)
{
  eradf <- getERA(refage, refyear, sex)
  eradf <- filter(eradf, year %in% seq(from,to))
  eradf <- melt(eradf, id.vars="year", value.name = "ERA", variable.name="type")
  eradf$type <- revalue(eradf$type, c("aera"="Absolute", "rera"="Relative"))
  qplot(year, ERA, data=eradf, colour=type, geom=c("path"))
}


plotERALFP <- function(from, to, refage, refyear, sex, text=F)
{  
  eradf <- getERA(from=from, to=to, sex=sex, refage=refage,refyear=refyear)
  
  eradf$alfp <- getLFP(from:to, round(eradf$aera), sex=sex)
  eradf$rlfp <- getLFP(from:to, round(eradf$rera), sex=sex)
  eradf$lfp  <- getLFP(from:to, rep(refage,length(from:to)), sex=sex)
  
  p <- ggplot(data=eradf) +
    geom_path(aes(x=year,y=lfp,colour="Fixed")) +
    xlab("Year") +
    ylab("Labor Force Participation") +
    scale_y_continuous(labels=scales::percent, limits=c(0,1))
  
  if( text ) {
    p <- p + 
      geom_text(data=eradf,
                mapping=aes(x=year,y=alfp,label=format(aera,digits=3,nsmall=1),colour="Absolute"),
                size=3,
                show.legend = F) +
      geom_text(data=eradf,
                mapping=aes(x=year,y=rlfp,label=format(rera,digits=3,nsmall=1),colour="Relative"),
                size=3,
                show.legend = F)
  }
  else {
    p <- p +
      geom_path(aes(x=year,y=alfp,colour="Absolute")) +
      geom_path(aes(x=year,y=rlfp,colour="Relative"))
  }
  p
}

