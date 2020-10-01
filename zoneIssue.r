tempzone <- read.csv(file = 'D:\\download\\megan\\CDCFlaxMetPipline-master\\csv\\ZonelistName2.csv')
head(tempzone)
model<-aov(get(at)~name+location+bloc:location+name:location,data=tempzone)