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
#asreml.license.activate()#Please enter your activation code (RET or 0 to exit):XXXX-XXXX-XXXX-XXXX
########################################################################################################

library(stringr)
library(asreml)
library("agricolae") ##not actually necessary for LSD 5% calcs but can make things easier if the data is balanced. 
library("readxl") ## just to read the data
library("tidyverse") ## just to read the data
library(stringr)


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

data$environment <- paste(gsub(" ","_",data$location) , "_" , data$year, sep="")#create an envirinment attribute in the dataframe
header<-names(data)
start <- 6 #column where attributes start
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



data$bloc<-as.factor(data$bloc)
data$location<-as.factor(data$location)
data$name<-as.factor(data$name)
data$zone<-as.factor(data$zone)
data$year<-as.factor(data$year)
data$environment<-as.factor(data$environment)


TheYear<-unique(data[c("year")])

TheYearNum<-nrow(unique(data[c("year")]))

TheYear[lengths(TheYear) != 0]




###################################################################

#loop through locations and years and call the function to build files for each zone and attribute
i=1 #use most recent year

for(atr in attrib) #atr<-"earlyvig"
{
  for (j in 1:nrow(uzone))#j<-1
  {
    #str_replace(string, pattern, replacement)
    filestr<-paste("Zone ",zonearray[j]," Predicted Means ", yeararray[i],"-",atr,".csv", sep = "")
    print(filestr)
    #zoneplavs(zonearray[j],yeararray[i],atr)
    
    
    thiszone<-zonearray[j]
    TheYear<-yeararray[i]
    at<-atr
    
    temp<-data  %>% filter(!is.na(get(at))) %>% filter(get(at)!=0) %>% filter(zone==zonearray[j])
    numatr<-nrow(temp)
    numloc=nrow(unique(temp[c("location")]))
    numloc
    unique(temp$zone)
    unique(temp$location)
    
    #if(numatr>0)get(at)
    #{
    #general<-asreml(fixed=get(at)~name,data=temp)
    #general<-asreml(fixed=get(at),data=temp)
    

    if (numloc == 1) 
    {
      model<-asreml(fixed=get(at)~name, random=~environment:bloc,data=temp)
      pred<-predict(model,classify='name',data=temp)
    }else
    {
      model<-asreml(fixed=get(at)~name , random=~at(environment):bloc + environment + environment:name,residual=~dsum(~units|environment),data=temp)
      pred<-predict(model,classify='environment:name',data=temp)
    }
    
    
    
    #pred<-predict(model,classify='environment:name',data=data)$pvals
    filename1<-sprintf("\\Zone%s-%s.csv", thiszone,at)
    filename<-paste(csvdir,filename1, sep = "")
    print(paste("this is the file ", filename))
    
    write.table(pred$pvals, filename, append = TRUE, quote = TRUE, sep = ",",
                eol = "\n", na = "NA", dec = ".", row.names = TRUE,
                col.names = NA, qmethod = c("escape", "double"),
                fileEncoding = "")
    
    if (numloc == 1) 
    {
      model<-aov(get(at)~name+bloc,data=temp)
    }else
    {
      model<-aov(get(at)~name+location+bloc:location+name:location,data=temp)
    }
    
    
    summary(model)
    order<-temp %>% group_by(name) %>% summarize(n=n())
    max(order$n)
    dfx<-tail(summary(model)[[1]]$`Df`, n=1)
    msx<-tail(summary(model)[[1]]$`Mean Sq`, n=1)
    print(abs(qt(0.05/2,dfx*1))*sqrt(msx*2/(33)))
    lsdis<-abs(qt(0.05/2,dfx*1))*sqrt(msx*2/(33))
    meanis=mean(temp[[at]], na.rm = TRUE)
    stddevis=sd(temp[[at]], na.rm = TRUE)
    cvpctis=(stddevis/meanis)*100
    
    
    write.table(paste("lsd at 5%",  lsdis), filename, sep = ",", col.names = !file.exists(filename), append = T)
    write.table(paste("CV%:",  cvpctis), filename, sep = ",", col.names = !file.exists(filename), append = T)
    
  }
}
###################################################################


#as above but do all zones as 1 for each attribute
###################################################################
for(atr in attrib) #atr<-"yield_kgha"
{
  yeararray<-sort(yeararray,decreasing = TRUE)
  TheYear<-yeararray[1]
  at<-atr
  
  print (paste("0-TheYear:" ,TheYear," attrib:",at))
  temp <- data %>% filter(!is.na(get(at))) %>% filter(get(at)!=0) %>% filter(get(at)!="NA")  %>% filter(get(at)!="")

  filename<-sprintf("./csv/%s.csv",at)
  
  write.csv(temp, filename)
  numatr<-nrow(temp)


  if(numatr>0)
  {

  #below for accross all zones
  #use a try below incase the model doesnt work with the data
  e1 <- try(model<-asreml(fixed=get(at)~name , random=~at(environment):bloc + environment + environment:name,residual=~dsum(~units|environment),data=temp))
  e1test<-grepl("Error in asreml",e1, fixed = TRUE)
  #if initial model does not work fall back to this model below
  if (e1test) {model<-asreml(fixed=get(at)~name, random=~environment:bloc + environment + environment:name,data=temp)}



#use a try below incase the model doesnt work with the data
  e2 <- try(pred<-predict(model,classify='environment:name',data=temp))
  e2test<-grepl("Error in asreml",e2, fixed = TRUE)
#if initial model does not work fall back to this model below
  if (e2test ) {pred<-predict(model,classify='environment:name',data=temp)}


  
  
  filename1<-sprintf("\\ZoneAll%s-%s.csv", TheYear,at)
  filename<-paste(csvdir,filename1, sep = "")
  write.table(pred$pvals, filename, append = TRUE, quote = TRUE, sep = ",",
              eol = "\n", na = "NA", dec = ".", row.names = TRUE,
              col.names = NA, qmethod = c("escape", "double"),
              fileEncoding = "")
  
  
  model<-aov(get(at)~name+location+bloc:location+name:location,data=data)
  
  
  summary(model)
  order<-temp %>% group_by(name) %>% summarize(n=n())
  max(order$n)
  dfx<-tail(summary(model)[[1]]$`Df`, n=1)
  msx<-tail(summary(model)[[1]]$`Mean Sq`, n=1)
  print(abs(qt(0.05/2,dfx*1))*sqrt(msx*2/(33)))
  lsdis<-abs(qt(0.05/2,dfx*1))*sqrt(msx*2/(33))
  meanis=mean(temp[[at]], na.rm = TRUE)
  stddevis=sd(temp[[at]], na.rm = TRUE)
  cvpctis=(stddevis/meanis)*100

  
  
  ######################################################################################### 
  #create the means for zones x attribute 
  p<-pred$pvals
  t<-temp
  subt <- subset(t,environment != "", select = c(environment, zone))#sub array of environment attribute x zone
  usubt<-unique(subt)#make is a unique array
  mg<-merge(x=p,y=usubt,by="environment",all.x=TRUE) #merge oredictions with the zones
  aggregatePred<-aggregate(predicted.value~zone+name,FUN=mean,data=mg) #getm the means of the predicted means of that zone
  #########################################################################################   
  
  write.table(paste("lsd at 5%",  lsdis), filename, sep = ",", col.names = !file.exists(filename), append = T)
  write.table(paste("CV%:",  cvpctis), filename, sep = ",", col.names = !file.exists(filename), append = T)
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
  
}
}
###################################################################

p<-pred$pvals
t<-temp
subt <- subset(t,environment != "", select = c(environment, zone))
usubt<-unique(subt)
mg<-merge(x=p,y=usubt,by="environment",all.x=TRUE)
aggregate(predicted.value~zone+name,FUN=mean,data=mg)

