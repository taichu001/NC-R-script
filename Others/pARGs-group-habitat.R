library("ggplot2")
library("vegan")
require('Cairo')
library('ggrepel')
library('ape')


design =read.delim("pARGs-group-habitat.txt", header=T, row.names= 1,sep="\t")
otu=read.delim("pARG_abundance_habitat.txt",sep="\t",row.names= 1,header=T, check.names=F)
data <- vegdist(otu,na.rm = T)
pcoa<- pcoa(data, correction = "none", rn = NULL)
PCA1 = pcoa$vectors[,1]
PCA2 = pcoa$vectors[,2]

#nmds
NMDS <- metaMDS(otu, distance = "bray",k=2)
MDS1 = NMDS$points[,1]
MDS2 = NMDS$points[,2]

index<- data.frame(rownames(NMDS$points),MDS1,MDS2,design$habitat,design$group1)
colnames(index) <-c("sample","MDS1","MDS2","site","group")
pca1 <-floor(pcoa$values$Relative_eig[1]*100)
pca2 <-floor(pcoa$values$Relative_eig[2]*100)


cbbPalette <- c('Human'='#B3D0E4',
                'Food'='#D7D5E9',
                'Soil'='#FCB3AA',
                'Plant'="#FCB461",
                'Fram animals'='#BBE4DD',
                'Sediment'="#CCEBC5",
                'Wild animal'="#FBCDE4",
                'Fresh water'="#B3DE69",
                'Aquatic organism'="#D9D9D9",
                'Insects'="#BB80BC",
                'Seawater'="#FFEC6D"
)


p2<-ggplot(index,aes(MDS1,MDS2))+ 
  geom_point(aes(fill=index$site,shape=index$group),size=3,alpha=0.8)+
  scale_shape_manual(values = c(21,24))+
  annotate('text', label = 'habitat:R2:index, p=0.001,treat:R2:0.04293, p=0.001 ', x =0.01, y =-0.070, size =3.5)+
  #stat_ellipse(aes(fill=index$site),geom = "polygon",size=1.2,level = 0.95,alpha = 0.2)+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(axis.text = element_text(color = "black",size =11))
p2

adonis.res<-adonis2(data~design$habitat+design$group2,permutations = 999,p.adjust.m = "BH",method = "bray",contr.ordered = "contr.poly")
adonis.res

ggsave(p2,filename ="pARGs habitat-type.pdf",width =5.5,height =4.0) 
dev.off

