library(dplyr)
library(ggplot2)
library(scales)
library(reshape2)
library(tibble)
library(plotly)

HQAI<-read.csv("HQAIdataOct.csv", sep=";")

#Calculate average 
HQAI[length(HQAI[,1])+1,3:11]<-colMeans(HQAI[,3:11])
names<-as.character(HQAI[,1])
names[length(HQAI[,1])]<-"Average"
HQAI[,1]<-as.factor(names)

#Data manipulation 
df<-HQAI[,-c(2)]
ids<-colnames(df[-1])
df<-melt(df,id.vars=c("Organization"),measure.vars=ids)%>%arrange(Organization)
colnames(df)<-c("Organization","Commitment","Score")
df$Score<-round(df$Score, 2)

dfNOAV<-df[df$Organization!='Average',]#dataframe without average values

#Boxplots
 
boxHQAI1<-ggplot(df, aes(Commitment, Score))+
  geom_boxplot(aes(alpha=.5), show.legend = FALSE) +geom_jitter(height=0,aes(color=Organization), alpha=.5)+
  theme_bw()+theme( axis.text.x  = element_text(angle=45, vjust=0.5))

boxHQAI2<-ggplot(df, aes(Organization, Score))+
  geom_boxplot(aes(alpha=.5), show.legend = FALSE) +geom_jitter(height=0,aes(color=Commitment),alpha=.8)+theme_bw()+
  theme( axis.text.x  = element_text(angle=90, vjust=0.5, hjust=1))+scale_color_brewer(palette="Set1")

p2<-ggplotly(boxHQAI1)
p3<-ggplotly(boxHQAI2) 


#Radar plot

dfAV<-df[df$Organization=='Average',]
dfAV[1:(9*(length(HQAI[,1])-1)),]<-dfAV

radar<-ggplot(dfNOAV, aes(x=Commitment, y=Score, group=Organization, color=Organization)) +
  geom_polygon (aes(fill=Organization),show.legend = FALSE, alpha=0.5) + theme_bw() +
  facet_wrap(~ Organization) + coord_polar() + geom_polygon(aes(y = dfAV$Score, x=Commitment), color="black", show.legend=FALSE, fill=NA)+
theme(text = element_text(size=20))

###########
#Initial vs midterm audit
#########

HQAI2<-read.csv("HQAIdataUpdates.csv",sep=";")

comm<-as.character(HQAI2[,3])
HQAI2[,3]<-as.factor(comm)

colnames(HQAI2)<-c("Organization","Score","Commitment","Audit")

HQAI2Init<-HQAI2[HQAI2$Audit=='Initial',]
HQAI2Mid<-HQAI2[HQAI2$Audit=='Mid',]

radarchange<-ggplot(HQAI2Mid, aes(x=Commitment, y=Score, group=Organization, color=Organization)) +
  geom_polygon (aes(fill=Organization),show.legend = FALSE, alpha=0.5) + theme_bw() +
  facet_wrap(~ Organization) + coord_polar() +ylim (0,5) + geom_polygon(aes(y = HQAI2Init$Score, x=Commitment), color="black", show.legend=FALSE, fill=NA)
