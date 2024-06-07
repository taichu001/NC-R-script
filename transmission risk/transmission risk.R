library(ggradar)
library(ggplot2)
library(gridExtra)
data.Aquatic.organism <- read.delim('Aquatic organism pARG transmission risk.txt',header = T)
data.food <- read.delim('food pARG transmission risk.txt',header = T)
data.huamn <-read.delim('human pARG transmission risk.txt',header = T)
data.Insects <- read.delim('insects pARG transmission risk.txt',header = T)
data.animal.husbandry <- read.delim('livertock pARG transmission risk.txt',header = T)
data.Plant <- read.delim('plant pARG transmission risk.txt',header = T)
data.Seawater <- read.delim('seawater pARG transmission risk.txt',header = T)
data.Sediment <- read.delim('sediments pARG transmission risk.txt',header = T)
data.soil <- read.delim('Soil pARG transmission risk.txt',header = T)
data.Wild.animal <- read.delim('wildlife pARG transmission risk.txt',header = T)
data.Surface.water <- read.delim('Fresh water pARG transmission risk.txt',header = T)

#"#C57962","#4B9347","#D37A0F"

range(data.food[,2:12])
p1 <- ggradar2(plot.data = data.food,
               all.radar = c(0,1,1.25,2.5),
               grid.label.size = 10,
               all.label = c("0","100%","125%","250%"),
               background.circle.colour = "#C7E6F0",
               fill = T,
               fill.alpha = 0.2,  
               group.line.width = 1, 
               group.point.size = 3,
               group.colours = c("#78A8C6")
)
p1

ggsave("food prophageARGs传播频率(统一阈值).pdf",p1,width = 12,height =8)



range(data.animal.husbandry[,2:12])
p2 <- ggradar2(plot.data =data.animal.husbandry,
               all.radar = c(0,1,1.25,2.5),
               grid.label.size=10,
               all.label = c("0","100%","125%","250%"),
               background.circle.colour="#C7E6F0",
               fill=T,
               fill.alpha = 0.2,  
               group.line.width = 1, 
               group.point.size = 3,
               group.colours = c("#BEBADA")
)
p2

ggsave("animal.husbandry prophageARGs传播频率(统一阈值).pdf",p2,width = 12,height =8)



range(data.huamn[,2:12])
p3 <- ggradar2(plot.data =data.huamn,
               all.radar = c(0,1,1.25,2.5),
               grid.label.size=10,
               all.label = c("0","100%","125%","250%"),
               background.circle.colour="#C7E6F0",
               fill=T,
               fill.alpha = 0.2,  
               group.line.width = 1, 
               group.point.size = 3,
               group.colours = c("#8DD3C7")
)
p3

ggsave("human prophageARGs传播频率(统一阈值).pdf",p3,width = 12,height =8)



range(data.soil[,2:12])
p4 <- ggradar2(plot.data =data.soil,
               all.radar = c(0,1,1.25,2.5),
               grid.label.size=10,
               all.label = c("0","100%","125%","250%"),
               background.circle.colour="#C7E6F0",
               fill=T,
               fill.alpha = 0.2,  
               group.line.width = 1, 
               group.point.size = 3,
               group.colours = c("#F37D74")
)
p4

ggsave("soil prophageARGs传播频率(统一阈值).pdf",p4,width = 12,height =8)



range(data.Sediment[,2:12])
p5 <- ggradar2(plot.data =data.Sediment,
               all.radar = c(0,1,1.25,2.5),
               grid.label.size=10,
               all.label = c("0","100%","125%","250%"),
               background.circle.colour="#C7E6F0",
               fill=T,
               fill.alpha = 0.2,  
               group.line.width = 1, 
               group.point.size = 3,
               group.colours = c("#BEE0BE")
)
p5

ggsave("Sediment prophageARGs传播频率(统一阈值).pdf",p5,width = 12,height =8)




range(data.Wild.animal[,2:12])
p6 <- ggradar2(plot.data =data.Wild.animal,
               all.radar = c(0,1,1.25,2.5),
               grid.label.size=10,
               all.label = c("0","100%","125%","250%"),
               background.circle.colour="#C7E6F0",
               fill=T,
               fill.alpha = 0.2,  
               group.line.width = 1, 
               group.point.size = 3,
               group.colours = c("#F5C2D9")
)
p6

ggsave("Wild.animal prophageARGs传播频率(统一阈值).pdf",p6,width = 12,height =8)




range(data.Surface.water[,2:12])
p7 <- ggradar2(plot.data =data.Surface.water,
               all.radar = c(0,1,1.25,2.5),
               grid.label.size=10,
               all.label = c("0","100%","125%","250%"),
               background.circle.colour="#C7E6F0",
               fill=T,
               fill.alpha = 0.2,  
               group.line.width = 1, 
               group.point.size = 3,
               group.colours = c("#A0CE3A")
)
p7

ggsave("Surface.water prophageARGs传播频率(统一阈值).pdf",p7,width = 12,height =8)




range(data.Aquatic.organism[,2:12])
p8 <- ggradar2(plot.data =data.Aquatic.organism,
               all.radar = c(0,1,1.25,2.5),
               grid.label.size=10,
               all.label = c("0","100%","125%","250%"),
               background.circle.colour="#C7E6F0",
               fill=T,
               fill.alpha = 0.2,  
               group.line.width = 1, 
               group.point.size = 3,
               group.colours = c("#9D9E98")
)
p8

ggsave("Aquatic.organism prophageARGs传播频率(统一阈值).pdf",p8,width = 12,height =8)




range(data.Insects[,2:12])
p9 <- ggradar2(plot.data =data.Insects,
               all.radar = c(0,1,1.25,2.5),
               grid.label.size=10,
               all.label = c("0","100%","125%","250%"),
               background.circle.colour="#C7E6F0",
               fill=T,
               fill.alpha = 0.2,  
               group.line.width = 1, 
               group.point.size = 3,
               group.colours = c("#AA7CB6")
)
p9

ggsave("Insects prophageARGs传播频率(统一阈值).pdf",p9,width = 12,height =8)




range(data.Seawater[,2:12])
p10 <- ggradar2(plot.data =data.Seawater,
                all.radar = c(0,1,1.25,2.5),
                grid.label.size=10,
                all.label = c("0","100%","125%","250%"),
                background.circle.colour="#C7E6F0",
                fill=T,
                fill.alpha = 0.2,  
                group.line.width = 1, 
                group.point.size = 3,
                group.colours = c("#F2E27B")
)
p10

ggsave("Seawater prophageARGs传播频率(统一阈值).pdf",p10,width = 12,height =8)




range(data.Plant[,2:12])
p11 <- ggradar2(plot.data =data.Plant,
                all.radar = c(0,1,1.25,2.5),
                grid.label.size=10,
                all.label = c("0","100%","125%","250%"),
                background.circle.colour="#C7E6F0",
                fill=T,
                fill.alpha = 0.2,  
                group.line.width = 1, 
                group.point.size = 3,
                group.colours = c("#D37A0F","#FCB461")
)
p11

ggsave("Plant prophageARGs传播频率(统一阈值).pdf",p11,width = 12,height =8)


p <- grid.arrange(p3, p2, p1, p6, p7, p4, p8, p5, p11, p9, p10 ,ncol = 3, nrow =4)
p
ggsave("传播频率组合(统一阈值).pdf",p,width =24,height =18)
p2
p3
p6
p1
p7
p4
p8
p5
p11
p9
p10