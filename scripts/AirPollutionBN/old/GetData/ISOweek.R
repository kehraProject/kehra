# Source: http://www.r-bloggers.com/iso-week/
# test:
# ISOweek("2012-09-14")

ISOweek<-function(date,format="%Y-%m-%d",return.val="weekofyear"){
  ##converts dates into "dayofyear" or "weekofyear", the latter providing the ISO-8601 week
  ##date should be a vector of class Date or a vector of formatted character strings
  ##format refers to the date form used if a vector of
  ##  character strings  is supplied
  
  ##convert date to POSIXt format 
  if(class(date)[1]%in%c("Date","character")){
    date=as.POSIXlt(date,format=format)
  }
  if(class(date)[1]!="POSIXlt"){
    print("Date is of wrong format.")
    break
  }else if(class(date)[2]=="POSIXct"){
    date=as.POSIXlt(date)
  }
  
  
  if(return.val=="dayofyear"){
    ##add 1 because POSIXt is base zero
    return(date$yday+1)
  }else if(return.val=="weekofyear"){
    ##Based on the ISO8601 weekdate system,
    ## Monday is the first day of the week
    ## W01 is the week with 4 Jan in it.
    year=1900+date$year
    jan4=strptime(paste(year,1,4,sep="-"),format="%Y-%m-%d")
    wday=jan4$wday
    
    wday[wday==0]=7  ##convert to base 1, where Monday == 1, Sunday==7
    
    ##calculate the date of the first week of the year
    weekstart=jan4-(wday-1)*86400  
    weeknum=ceiling(as.numeric((difftime(date,weekstart,units="days")+0.1)/7))
    
    #########################################################################
    ##calculate week for days of the year occuring in the next year's week 1.
    #########################################################################
    mday=date$mday
    wday=date$wday
    wday[wday==0]=7
    year=ifelse(weeknum==53 & mday-wday>=28,year+1,year)
    weeknum=ifelse(weeknum==53 & mday-wday>=28,1,weeknum)
    
    ################################################################
    ##calculate week for days of the year occuring prior to week 1.
    ################################################################
    
    ##first calculate the numbe of weeks in the previous year
    year.shift=year-1
    jan4.shift=strptime(paste(year.shift,1,4,sep="-"),format="%Y-%m-%d")
    wday=jan4.shift$wday
    wday[wday==0]=7  ##convert to base 1, where Monday == 1, Sunday==7
    weekstart=jan4.shift-(wday-1)*86400
    weeknum.shift=ceiling(as.numeric((difftime(date,weekstart)+0.1)/7))
    
    ##update year and week
    year=ifelse(weeknum==0,year.shift,year)
    weeknum=ifelse(weeknum==0,weeknum.shift,weeknum)
    
    return(list("year"=year,"weeknum"=weeknum))
  }else{
    print("Unknown return.val")
    break
  }
}
