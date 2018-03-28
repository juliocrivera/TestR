CheckRSI<-function(dataStock)
{
#recorrer arreglo y ver si el angulo es positivo si es asi seguir el tracing hasta que haya un cambio considerable si es que lo hay  
arr<-rev(dataStock)
arr[1]
arr[2]
arr2<-Angle(arr[1],arr[2],1)
arr2
}

ModifyVolume<-function(volData)
{
  #Function to modify volume according to splits,distributions and dividends
  #TBD
  #download info from tradingview
}


CheckforSpikes<-function(dataStock,SymbolN)
{
  #RSI=1 VALUE, Candle = 1 value(close), 
}

CheckForCrossing<-function(dataStock)
{
  #For RSI and SMA when stocks cross and actually tell you if its going up or down 
}

Angle<-function(close,closeM1,TimeLapse)
{
  #close = close for today, closeM1= close from Yesterday or older one, time lapse= how many days are between both
  i_close<-close-closeM1
  deg<-(atan2(i_close,TimeLapse)*(180/pi))
  return(deg)
}
CandleAnalysis<- function(openV,lowV,highV,closeV,Threshold,Minimum)
{
  mid<- (openV+closeV)/2
  
  if(closeV>openV)#1 First we check if we have a positive value
  {
    if((highV-closeV)<Minimum)#2 With this we know this is potentially a hammer because high and close are really close
    {
      if((openV-lowV) >Threshold)#3 With this we know that there was some movement between low and Open which is healthy... unnecesary?
      {
        if((closeV-openV)<Threshold)
        {
          #3.1 This is a Hammer with positive movement
          if((closeV-lowV)<(Threshold*2))
          {
            #3.1.1 not much movement
            return(X*2)
          }
          else if((closeV-lowV)>(Treshold*2))
          {
            #3.1.2 a lot of movement
            return(X*3)
          }
          
        }
        else if((closeV-openV)>Threshold)
        {
          #3.2 Hammer but with a lot of movement 
          if((closeV-lowV)<(Threshold*2))
          {
            #3.2.1 not much movement
            return(X)
          }
          else if((closeV-lowV)>(Treshold*2))
          {
            #3.2.2 a lot of movement
            return(X*5)
          }
        }        
        
      }
      else if(((openV-lowV)<Threshold))#4 low and open are near meaning it didnt really decrease==Bullish
      {
        if((closeV-openV)<Threshold)
        {
          #4.1 This is a Hammer with positive movement
          if((closeV-lowV)<(Threshold*2))
          {
            #4.1.1 not much movement
            return(X)
          }
          else if((closeV-lowV)>(Treshold*2))
          {
            #4.1.2 a lot of movement might never get here though...
            return(X)
          }
        }
        else if((closeV-openV)>Threshold)
        {
          #4.2 Hammer but with a lot of movement 
          if((closeV-lowV)<(Threshold*2))
          {
            #4.2.1 not much movement
            return(X*3)
          }
          else if((closeV-lowV)>(Treshold*2))
          {
            #4.2.2 a lot of movement
            return(X*5)
          }
        }
      }
      
    }
    #Above this was to check if it was a hammer,Below is for Doji and others
    if((highV-closeV)<Threshold)#5 If close is still close to High then we know we're good i guess?
    {
      if((openV-lowV) >Threshold)#5.1
      {
        if((closeV-openV)<Threshold)
        {
          #5.1.1 This is a Hammer with positive movement
          if((closeV-lowV)<(Threshold*2))
          {
            #5.1.1.1 not much movement
            return(X)
          }
          else if((closeV-lowV)>(Treshold*2))
          {
            #5.1.1.2 a lot of movement
            return(X*2)
          }
        }
        else if((closeV-openV)>Threshold)
        {
          #5.1.2 Hammer but with a lot of movement 
          if((closeV-lowV)<(Threshold*2))
          {
            #5.1.2.1 not much movement
            return(X)
          }
          else if((closeV-lowV)>(Treshold*2))
          {
            #5.1.2.2 a lot of movement
            return(X*2)
          }
        }        
        
      }
      else if(((openV-lowV)<Threshold))#5.2 low and open are near meaning it didnt really decrease==Bullish
      {
        if((closeV-openV)<Threshold)
        {
          #5.2.1 This is a Hammer with positive movement
          if((closeV-lowV)<(Threshold*2))
          {
            #5.2.1.1 not much movement
            return(X)
          }
          else if((closeV-lowV)>(Treshold*2))
          {
            #5.2.1.2 a lot of movement
            return(X)
          }
        }
        else if((closeV-openV)>Threshold)
        {
          #5.2.2 Hammer but with a lot of movement 
          if((closeV-lowV)<(Threshold*2))
          {
            #5.2.2.1 not much movement
            return(X*3)
          }
          else if((closeV-lowV)>(Treshold*2))
          {
            #5.2.2.2 a lot of movement
            return(X*4)
          }
        }
        
      }
    }
    if(closeV>mid)      #6 We know with this we finished at least higher than mid
    {
      if(openV>mid)
      {
        #6.1 We actually did really well in this we started higher than mid
        return(X*2)
      }
      else
      {
        #6.2  still did good i guess 
        return(X)
      }
    }
    if(closeV<mid)      #7 Bad we are below mid and somehow above open
    {
      if(closeV>(mid-Threshold))#7.1 We're probably not THAT down
      {
        return(-X)
      }
      else#7.2 Really down
      {
        return(-X*2)
      }
    }
  }
  else if(openV==closeV)#8 we finished where we began...
  {
    return(-X)
  }
  else
  {
    #9 Close was below open so this is a dark indicator...
    if(closeV>mid)#9.1
    {
      #At least it is above  mid, this is bad but NOT THAT bad
      return(-X*2)
    }
    else#9.2
    {
      #Almost the Worst
      if(closeV<= lowV+Threshold)#9.2.1
      {
        #Worst
        return(-X*5)
      }
      else#9.2.2
      {
        return(-X*4)
      }
    }
    
  }
  
  return(1)
}