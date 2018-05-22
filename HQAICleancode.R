library(dplyr)
library(ggplot2)
library(scales)
library(reshape2)
library(tibble)
library(plotly)

HQAI<-read.csv("HQAIdata.csv", sep=";")

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
  theme( axis.text.x  = element_text(angle=45, vjust=0.5))+scale_color_brewer(palette="Set1")

p2<-ggplotly(boxHQAI1)
p3<-ggplotly(boxHQAI2) 


#Radar plot

dfAV<-df[df$Organization=='Average',]
dfAV[1:(9*(length(HQAI[,1])-1)),]<-dfAV

radar<-ggplot(dfNOAV, aes(x=Commitment, y=Score, group=Organization, color=Organization)) +
  geom_polygon (aes(fill=Organization),show.legend = FALSE, alpha=0.5) + theme_bw() +
  facet_wrap(~ Organization) + coord_polar() + geom_polygon(aes(y = dfAV$Score, x=Commitment), color="black", show.legend=FALSE, fill=NA)+
theme(text = element_text(size=20))


# short dataframe with averages for comparison with single organizations
'%!in%' <- function(x,y)!('%in%'(x,y))
  dfAVold<-df[df$Organization %!in% c('Women Support Association', 'Plan Int', 'Caritas DK', 'Coast Trust','Average'),]
dfAVold<-aggregate(dfAVold[, 3], list(dfAVold$Commitment), mean)
dfAVold$Organization<-"OldAverage"
colnames(dfAVold)<-c("Commitment","Score","Organization")
dfAVshort<-dfAV[1:9,]


#Radar plot Coast Trust
dfCT<-df[df$Organization=='Coast Trust',]
dfCT <-rbind(dfCT, dfAVshort, dfAVold)

radarCT<-ggplot(dfCT, aes(x=Commitment, y=Score, group=Organization, color=Organization, fill=Organization, alpha=Organization)) +
  geom_polygon (aes(),show.legend = TRUE) + ylim (0,5) + theme_bw() +
  coord_polar() + ggtitle ("Coast Trust") + 
  scale_color_manual(values = c("red","blue","black")) +scale_fill_manual(values = c("red","red","black"))+ scale_alpha_manual(values = c(0,0.5,0))

#Radar plot Women Support Association
dfWSA<-df[df$Organization=='Women Support Association',]
dfWSA <-rbind(dfWSA, dfAVshort, dfAVold)

radarWSA<-ggplot(dfWSA, aes(x=Commitment, y=Score, group=Organization, color=Organization, fill=Organization, alpha=Organization)) +
  geom_polygon (aes(),show.legend = TRUE) + ylim (0,5) + theme_bw() +
  coord_polar() + ggtitle ("Women Support Association") + 
  scale_color_manual(values = c("red","green","black")) +scale_fill_manual(values = c("red","yellow","black"))+ scale_alpha_manual(values = c(0,0.5,0))

#Radar plot Plan International
dfPlan<-df[df$Organization=='Plan Int',]
dfPlan <-rbind(dfPlan, dfAVshort, dfAVold)

radarPlan<-ggplot(dfPlan, aes(x=Commitment, y=Score, group=Organization, color=Organization, fill=Organization, alpha=Organization)) +
  geom_polygon (aes(),show.legend = TRUE) + ylim (0,5) + theme_bw() +
  coord_polar() + ggtitle ("Plan International") + 
  scale_color_manual(values = c("red","blue","black")) +scale_fill_manual(values = c("red","blue","black"))+ scale_alpha_manual(values = c(0,0.3,0))

#plan and plan de
dfPlanPlan<-rbind(df[df$Organization=="Plan Int",],df[df$Organization=="Plan Int DE",])
  
radarPlanPlan<-ggplot(dfPlanPlan, aes(x=Commitment, y=Score, group=Organization, color=Organization)) +
  geom_polygon (aes(fill=Organization), alpha=0.3) +  ylim (0,5) + theme_bw() +
  coord_polar() + ggtitle ("Plan International and Plan International DE")

#Radar plot Caritas DK
dfCarDK<-df[df$Organization=='Caritas DK',]
dfCarDK <- rbind(dfCarDK, dfAVshort, dfAVold) 

radarCaritasDK<-ggplot(dfCarDK, aes(x=Commitment, y=Score, group=Organization, color=Organization, fill=Organization, alpha=Organization)) +
  geom_polygon (aes(),show.legend = TRUE) + ylim (0,5) + theme_bw() +
  coord_polar() + ggtitle ("Caritas DK") + 
  scale_color_manual(values = c("red","pink","black")) +scale_fill_manual(values = c("red","red","black"))+ scale_alpha_manual(values = c(0,0.5,0))
