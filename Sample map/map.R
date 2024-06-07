library(maps)
library(ggplot2)
library(dplyr)
data <- read.delim("map-full.txt",header = T)
data$habitat <- factor(data$habitat,levels = c(
  'Animal husbandry',
  'Food',
  'Human',
  'Soil',
  'Surface water',
  'Aquatic organism',
  'Sediment',
  'Wild animal',
  'Plant',
  'Insect',
  'Seawater'
))
world<-map_data("world")
p <- ggplot() +
  geom_polygon(data=world,aes(x=long,y=lat,group=group),
               fill="#dedede")+
  geom_point(data=data,aes(x=longitude,y=latitude,fill=habitat,size=number),shape=21,alpha=0.6)+
  scale_fill_manual(values = c('#BDB9DA',"#80B0D2",'#8DD2C6','#FA8071',"#B3DE69",
                               "#D9D9D9","#CCEBC5","#FBCDE4","#FCB461","#BB80BC","#FFEC6D"))+
  scale_y_continuous(expand = expansion(mult=c(0,0)))+
  scale_x_continuous(expand = expansion(add=c(0,0)))+
  theme_void()

p

ggsave('map-full.pdf',p,width = 11,height = 5)



