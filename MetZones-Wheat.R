#The installs for the code see below
#install.packages("agricolae",destdir = "d:/download/RDownload")
#install.packages("Rtools",destdir = "d:/download/RDownload")
#install.packages("asreml",destdir = "d:/download/RDownload")
#install.packages("readxl",destdir = "d:/download/RDownload")
#install.packages("tidyverse",destdir = "d:/download/RDownload")
#install.packages("data.table",destdir = "d:/download/RDownload")
#install.packages("ggplot2",destdir = "d:/download/RDownload")
#install.packages("jsonlite",destdir = "d:/download/RDownload")
#install.packages("stringr",destdir = "d:/download/RDownload")
#install.packages("D:/RStuff/ASReml/asreml_4.1.0.106.zip", repos = NULL, type = "win.binary")
#install.packages("rlist")
#asreml.license.activate()#Please enter your activation code (RET or 0 to exit):XXXX-XXXX-XXXX-XXXX
########################################################################################################

library(stringr)
library(asreml)
library("agricolae") ##not actually necessary for LSD 5% calcs but can make things easier if the data is balanced. 
library("readxl") ## just to read the data
library("tidyverse") ## just to read the data
library(stringr)
library(rlist)


options(max.print = 999999)
options(tibble.print_max=50)

#this opens up a file upload prompt
DataFilename<-file.choose(new = FALSE)

#get the OS we are on windows ???
OS<-Sys.info()["sysname"]

#if windows get the default dir otherwise its UN*X/MAC
if (.Platform$OS.type == "windows"){
  dirend<-sapply(gregexpr("\\\\", DataFilename), tail, 1)
}else {
  dirend<-sapply(gregexpr("\\/", DataFilename), tail, 1)
}
diris<-substr(DataFilename, 1, dirend)

#CSV DIR path
csvdir<-paste(diris,"csv", sep = "")
workdir<-paste(diris,"work", sep = "")

#if the CSV subdir s\doesnt exist create it
if (file.exists(csvdir)== FALSE)
{
  dir.create(file.path(csvdir))
}
#if the WORK subdir s\doesnt exist create it
if (file.exists(workdir)== FALSE)
{
  dir.create(file.path(workdir))
}

#set working directory
setwd(diris)

#Read in the data from the selected file
data<-read_excel(DataFilename)

#injecting a environment when it doesn't exist
data$environment <- paste(gsub(" ","_",data$location) , "_" , data$year, sep="")#create an environment attribute in the dataframe

#get the colomn names
header<-names( data )#names(data)

HasBlock<-grepl('bloc', toString(header))

############################################################################################
#injecting a block when it doesn't exist
#if (!HasBlock) 
#{
#  data$bloc <- 1#create an bloc attribute in the dataframe
#}
#header<-names( data )#names(data)
#HasBlock<-grepl('bloc', toString(header))
#col_idx <- grep("bloc", names(data))
#data <- data[, c(col_idx, (1:ncol(data))[-col_idx])]
############################################################################################

col_idx <- grep("environment", names(data))
data <- data[, c(col_idx, (1:ncol(data))[-col_idx])]
names(data)


header<-names( data )#names(data)


start <- 7 #column where attributes start
end<-length(header) #last attribute column
attrib<-header[start:end]



######################################################################################
TheYear<-unique(data[c("year")])

TheYearNum<-nrow(unique(data[c("year")]))

TheYear[lengths(TheYear) != 0]
uloc<-unique(data[c("location")])
uzone<-unique(data[c("zone")])
numlocal<-nrow(unique(data[c("location")]))

#make arrays out of the Locations,Zones and years
locarray<-uloc$location
zonearray<-uzone$zone
yeararray<-TheYear$year
######################################################################################

##ASREML all code below this bit is patched in after the fact a bit will clean it up


if (HasBlock) 
{
  data$bloc<-as.factor(data$bloc)
}


data$location<-as.factor(data$location)
data$name<-as.factor(data$name)
#data$zone<-as.factor(data$zone)
data$year<-as.factor(data$year)
data$environment<-as.factor(data$environment)


TheYear<-unique(data[c("year")])

TheYearNum<-nrow(unique(data[c("year")]))

TheYear[lengths(TheYear) != 0]
LastYear <- yeararray[TheYearNum]






###################################################################
for(atr in attrib) #atr<-"yield"_kgha"
{
  yeararray<-sort(yeararray,decreasing = TRUE)
  TheYear<-yeararray[1]
  at<-atr
  
  print (paste("0-TheYear:" ,TheYear," attrib:",at))
  data <- data %>% arrange(environment) #sort data first
  #temp <- data %>% filter(!is.na(get(at))) %>% filter(get(at)!=0) %>% filter(get(at)!="NA")  %>% filter(get(at)!="")
  temp <- data %>% filter(!is.na(get(at))) %>% filter(get(at)!=0) %>% filter(get(at)!="NA")  %>% filter(get(at)!="")  %>% filter(year>2006)
  temp$year<-as.factor(temp$year)
  
  filename<-sprintf("./csv/%s.csv",at)
  
  write.csv(temp, filename)
  numatr<-nrow(temp)
  
  
  
  if(numatr>0)
  {

  #below for accross all zones
  #use a try below incase the model doesnt work with the data
  if (HasBlock) 
  {
   modelis="model<-asreml(fixed=get(at)~name , random=~at(environment):bloc + environment + environment:name,residual=~dsum(~units|environment),data=temp)"
   e1 <- try(model<-asreml(fixed=get(at)~name , random=~at(environment):bloc + environment + environment:name,residual=~dsum(~units|environment),data=temp))
  }
  else
  {
   #e1 <- try(model<-asreml(fixed=get(at)~name , random=~at(environment):environment + environment:name,residual=~dsum(~units|environment),data=temp))
   #e1 <- try(model<-asreml(fixed=get(at)~name , random=~environment + environment:name,residual=~dsum(~units|environment),data=temp))
   modelis="model<-asreml(fixed=get(at)~name , random=~year+location + location:name, residual=~dsum(~units|location), data=temp)"
   e1 <- try(model<-asreml(fixed=get(at)~name , random=~year+location + location:name, residual=~dsum(~units|location), data=temp))
  
  }
  
    
  #e1test<-grepl("Error",e1, fixed = TRUE)
  
  e1test<-str_detect(toString(e1), "Error", negate = FALSE)
  
  #if initial model does not work fall back to this model below
  if (e1test) 
  {
    if (HasBlock) 
    {
     modelis="asreml(fixed=get(at)~name, random=~environment:bloc + environment + environment:name,data=temp)"
     model<-asreml(fixed=get(at)~name, random=~environment:bloc + environment + environment:name,data=temp)
    }
    else
    {
     #model<-asreml(fixed=get(at)~name, random=~environment:environment + environment:name,data=temp)
    modelis="asreml(fixed=get(at)~name, random=~environment + environment:name,data=temp)"
     model<-asreml(fixed=get(at)~name, random=~environment + environment:name,data=temp)
     
    }
  }



#use a try below incase the model doesnt work with the data
  e2 <- try(pred<-predict(model,classify='environment:name',data=temp,workspace = "900mb"))
  e2test<-grepl("Error",e2, fixed = TRUE)
#if initial model does not work fall back to this model below
  #if (e2test ) {pred<-predict(model,classify='year:location:name',data=temp,workspace = "300mb")}
  LastYearString<-toString(LastYear)
  
  if (e2test ) {pred<-predict(model,classify='year:location:name',levels=list(year=LastYearString),data=temp,workspace = "900mb")}
            
  
  filename1<-sprintf("\\ZoneAll%s-%s.csv", TheYear,at)
  filename<-paste(csvdir,filename1, sep = "")
  
  if (file.exists(filename)) 
    #Delete file if it exists
    file.remove(filename)
  
  write.table(pred$pvals, filename, append = TRUE, quote = TRUE, sep = ",",
              eol = "\n", na = "NA", dec = ".", row.names = TRUE,
              col.names = NA, qmethod = c("escape", "double"),
              fileEncoding = "")
  
  if (HasBlock) 
  {
   model<-aov(get(at)~name+location+bloc:location+name:location,data=temp)
  }
  else
  {
   model<-aov(get(at)~name+location+location+name:location,data=temp)
  }
  
  
  summary(model)
  order<-temp %>% group_by(name) %>% summarize(n=n())
  maxord<-max(order$n)
  dfx<-tail(summary(model)[[1]]$`Df`, n=1)
  msx<-tail(summary(model)[[1]]$`Mean Sq`, n=1)
  print(abs(qt(0.05/2,dfx*1))*sqrt(msx*2/(maxord)))
  lsdis<-abs(qt(0.05/2,dfx*1))*sqrt(msx*2/(maxord))
  meanis=mean(temp[[at]], na.rm = TRUE)
  stddevis=sd(temp[[at]], na.rm = TRUE)
  cvpctis=(stddevis/meanis)*100

  Zonelist<-nrow(unique(temp[c("zone")]))
  ZonelistName<-unique(temp[c("zone")])
  
  #this is where the LSD for the zone will be
  
  
  lsdisZone = list() 

  meanisZones=  list()
  stddevisZone= list()
  cvpctisZone= list()
  

  ######################################################################################### 
  #create the means for zones x attribute 
  p<-pred$pvals
  
  
  
  header<-names( p )#names(data)
  HasEnvironment<-grepl('environment', toString(header))
  #make a merge column environment
  if (!HasEnvironment)
  {
   p$environment <- paste(gsub(" ","_",p$location) , "_" , p$year, sep="")#create an environment attribute in the dataframe
  }

  t<-temp
  subt <- subset(t,environment != "", select = c(environment, zone))#sub array of environment attribute x zone
  usubt<-unique(subt)#make is a unique array
  mg<-merge(x=p,y=usubt,by="environment",all.x=TRUE) #merge predictions with the zones
  aggregatePred<-aggregate(predicted.value~zone+name,FUN=mean,data=mg) #getm the means of the predicted means of that zone
  #########################################################################################   
  
  write.table(paste("lsd at 5%",  lsdis), filename, sep = ",", col.names = !file.exists(filename), append = T)
  write.table(paste("CV%:",  cvpctis), filename, sep = ",", col.names = !file.exists(filename), append = T)
  
  
  for (p in ZonelistName$zone) {
    print(p)
    tempzone <- temp %>% filter(zone==p)
    if (HasBlock) 
    {
      model<-aov(get(at)~name+location+bloc:location+name:location,data=tempzone)
    }
    else
    {
      model<-aov(get(at)~name+location+location+name:location,data=tempzone)
    }
    
    
    summary(model)
    order<-tempzone %>% group_by(name) %>% summarize(n=n())
    maxord<-max(order$n)
    dfx<-tail(summary(model)[[1]]$`Df`, n=1)
    msx<-tail(summary(model)[[1]]$`Mean Sq`, n=1)
    print(abs(qt(0.05/2,dfx*1))*sqrt(msx*2/(maxord)))
    lsdisZone[[p]]<-abs(qt(0.05/2,dfx*1))*sqrt(msx*2/(maxord))
    meanisZones[[p]]=mean(temp[[at]], na.rm = TRUE)
    stddevisZone[[p]]=sd(temp[[at]], na.rm = TRUE)
    cvpctisZone[[p]]=(stddevis/meanis)*100
    print (paste("Zone:",p," LDIS:",lsdisZone[[p]]))
    
    write.table(paste("Zone:",p," lsd at 5%",  lsdisZone[[p]]), filename, sep = ",", col.names = !file.exists(filename), append = T)
    write.table(paste("Zone:",p," CV%:",  cvpctisZone[[p]]), filename, sep = ",", col.names = !file.exists(filename), append = T)
    
  }
  
  
  
  write.table("-----Zone Aggregate below---------------------", filename, sep = ",", col.names = !file.exists(filename), append = T)
  write.table(aggregatePred, filename, sep = ",", col.names = !file.exists(filename), append = T)
  

  
  
  
  
  #add in a caveate if the model selected was fall back
  if (e1test ) 
  {
    write.table(paste("ERROR MODEL UPDATE: ",e1), filename, sep = ",", col.names = !file.exists(filename), append = T)
  }
  if (e2test ) 
  {
    write.table(paste("ERROR PREDICT: ",e2), filename, sep = ",", col.names = !file.exists(filename), append = T)
  } 
  write.table(paste("ASREML MODEL: ",modelis), filename, sep = ",", col.names = !file.exists(filename), append = T)
}
}
###################################################################



