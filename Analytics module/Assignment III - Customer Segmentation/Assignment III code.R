library(reshape2)
library(ggplot2)
library(tidyverse)
library(cluster)
library(klaR)
library(gridExtra)
library(NbClust)
library(factoextra)

#read in the data
dat <- read.csv("bakery.csv", header = T)

# EXPLORATORY DATA ANALYSIS

overall <- as.data.frame(colSums(dat))
overall <- cbind( rownames(overall),overall)
colnames(overall) <-c("Variable", "Total")

overall <- overall[-51,]
eda1 <- ggplot(aes(x=Variable, y=Total), data=overall) + 
  geom_col(fill="#CC3366") + 
  theme( axis.text.x  = element_text(angle=90, vjust=0.5), 
         panel.background = element_rect(fill = "white", colour='black'))

# PART 2
week <- dat[which(dat$Weekend==0),-51]
week <- as.data.frame(colSums(week))
names(week) <- "Week"
#week <- cbind(Variable=rownames(week),Total=week, Weekday="Week")

weekend <- dat[which(dat$Weekend==1),-51]
weekend <- as.data.frame(colSums(weekend))
names(weekend) <- "Weekend"
#weekend <- cbind(Variable=rownames(weekend),weekend, Weekday="Weekend")

total_week <- cbind(Variable=rownames(week),week,weekend)

total_week1 <- as.data.frame(melt(total_week))
names(total_week1) <- c("Variable","Weekday","Total")


eda2<- ggplot(total_week1,aes(x=Variable,y=Total, fill=Weekday))+geom_col()+ 
  theme( axis.text.x  = element_text(angle=90, vjust=0.5), 
         panel.background = element_rect(fill = "white", colour='black'))+
  scale_fill_brewer(palette = "Dark2")


eda3 <- ggplot(aes(x=Weekday, y=Total), data=total_week1) + 
  geom_col(width = .3, fill="#669933") + 
  theme( axis.text.x  = element_text(angle=90, vjust=0.5), 
         panel.background = element_rect(fill = "white", colour='black'))

grid.arrange(
  grobs = list(eda1,eda2,eda3),
  widths = c(2,1),
  layout_matrix = rbind(c(1, 3),
                        c(2, 2))
)

## BY CUSTOMER EDA

week1 <- dat[which(dat$Weekend==0),-51]
week1 <- as.data.frame(cbind(rowSums(week1),"Week"))
names(week1) <- c("Purchases","Weekday")
#week <- cbind(Variable=rownames(week),Total=week, Weekday="Week")

weekend1 <- dat[which(dat$Weekend==1),-51]
weekend1 <- as.data.frame(cbind(rowSums(weekend1),"Weekend"))
names(weekend1) <- c("Purchases","Weekday")

#weekend <- cbind(Variable=rownames(weekend),weekend, Weekday="Weekend")

total_week_cus <- rbind(week1,weekend1)


eda5 <- ggplot(total_week_cus,aes(x=Purchases, fill=Weekday))  + 
  geom_bar() + labs(y="Frequency", x="Number of purchases")+
  theme(panel.background = element_rect(fill = "white", colour='black'))+
  scale_fill_brewer(palette = "Dark2",direction=-1)+coord_flip()


eda4 <- ggplot(total_week_cus,aes(x=Purchases))  + 
  geom_bar(fill="#660099") + labs(y="Frequency", x="Number of purchases")+
  theme(panel.background = element_rect(fill = "white", colour='black'))

grid.arrange(
  grobs = list(eda4,eda5),
  widths = c(1,2))

bakery<- read.csv("bakery.csv", sep = ",", header = T)
bakery.data <- bakery[,-51]

# Using gower distance as dissimilarity matrix
dis.matrix <- daisy(bakery.data, metric = "gower", type = list(asymm = c(1:50))) #dissimilarity matrix
k.meds <- pam(dis.matrix, k=8, metric = "euclidean")
# Using correlation as dissimilarity matrix
corr <- as.dist(1-cor(t(bakery.data))) # Get correlation as dissimilirity
clusB8 <- pam(corr,8, metric="euclidean") # Clustering with K=8

s1 <- fviz_nbclust(bakery.data, pam, diss = dis.matrix, k.max = 8,method = "silhouette") + labs(subtitle = "Silhouette method: Gower distance.")
s11 <- fviz_silhouette(k.meds, print.summary = F)

s2<- fviz_nbclust(bakery.data, pam, diss = corr, k.max = 8, method = "silhouette", nboot = 5) +
  labs(subtitle = "Silhouette method: Correlation distance.") # K=8
s22 <- fviz_silhouette(clusB8, print.summary = F)

grid.arrange(grobs=list(s1,s11,s2,s22),nrow=2,ncol=2)

# Method B
dat_clus_B <- data.frame(cbind(bakery,clusB8$clustering))
names(dat_clus_B) <- c(colnames(bakery),"K8")

## VISUALIZE FOR K=8

## Get TOP 5 sums of purchases/total purchases for each product in each cluster number

dat_clus_B1 <- data.frame(Total=colSums(dat_clus_B[which(dat_clus_B$K8=="1"),1:50]))
dat_clus_B1 <- cbind(Variable=rownames(dat_clus_B1),dat_clus_B1,Cluster=1)
dat_clus_B1 <- dat_clus_B1[order(dat_clus_B1$Total,decreasing=T)[1:5],]

dat_clus_B2 <- data.frame(Total=colSums(dat_clus_B[which(dat_clus_B$K8=="2"),1:50]))
dat_clus_B2 <- cbind(Variable=rownames(dat_clus_B2),dat_clus_B2,Cluster=2)
dat_clus_B2 <- dat_clus_B2[order(dat_clus_B2$Total,decreasing=T)[1:5],]

dat_clus_B3 <- data.frame(Total=colSums(dat_clus_B[which(dat_clus_B$K8=="3"),1:50]))
dat_clus_B3 <- cbind(Variable=rownames(dat_clus_B3),dat_clus_B3,Cluster=3)
dat_clus_B3 <- dat_clus_B3[order(dat_clus_B3$Total,decreasing=T)[1:5],]

dat_clus_B4 <- data.frame(Total=colSums(dat_clus_B[which(dat_clus_B$K8=="4"),1:50]))
dat_clus_B4 <- cbind(Variable=rownames(dat_clus_B4),dat_clus_B4,Cluster=4)
dat_clus_B4 <- dat_clus_B4[order(dat_clus_B4$Total,decreasing=T)[1:5],]

dat_clus_B5 <- data.frame(Total=colSums(dat_clus_B[which(dat_clus_B$K8=="5"),1:50]))
dat_clus_B5 <- cbind(Variable=rownames(dat_clus_B5),dat_clus_B5,Cluster=5)
dat_clus_B5 <- dat_clus_B5[order(dat_clus_B5$Total,decreasing=T)[1:5],]

dat_clus_B6 <- data.frame(Total=colSums(dat_clus_B[which(dat_clus_B$K8=="6"),1:50]))
dat_clus_B6 <- cbind(Variable=rownames(dat_clus_B6),dat_clus_B6,Cluster=6)
dat_clus_B6 <- dat_clus_B6[order(dat_clus_B6$Total,decreasing=T)[1:5],]

dat_clus_B7 <- data.frame(Total=colSums(dat_clus_B[which(dat_clus_B$K8=="7"),1:50]))
dat_clus_B7 <- cbind(Variable=rownames(dat_clus_B7),dat_clus_B7,Cluster=7)
dat_clus_B7 <- dat_clus_B7[order(dat_clus_B7$Total,decreasing=T)[1:5],]

dat_clus_B8 <- data.frame(Total=colSums(dat_clus_B[which(dat_clus_B$K8=="8"),1:50]))
dat_clus_B8 <- cbind(Variable=rownames(dat_clus_B8),dat_clus_B8,Cluster=8)
dat_clus_B8 <- dat_clus_B8[order(dat_clus_B8$Total,decreasing=T)[1:5],]


## GATHER ALL CLUSTERS AND SUMS

dat_clus_Bk8 <- rbind(dat_clus_B5,dat_clus_B2,dat_clus_B3,dat_clus_B4,
                      dat_clus_B1,dat_clus_B6,dat_clus_B7,dat_clus_B8)
dat_clus_Bk8$Cluster <- as.factor(dat_clus_Bk8$Cluster)

ggplot(dat_clus_Bk8, aes(x=Cluster,y=Total,group=Variable, fill=Cluster))+
  geom_bar(position="dodge", stat="identity",  colour="white")+ 
  theme(panel.background = element_rect(fill = "white", colour='black'),
        legend.position = "none")+
  geom_text(aes(label=Variable),angle=90,position = position_dodge(width = 1),size=2)+
  scale_fill_brewer(palette = "Dark2")

dat_clus_B$Weekend <- as.factor(dat_clus_B$Weekend )

w1 <- ggplot(dat_clus_B, aes(x=as.factor(K8), fill=Weekend, group=Weekend))+geom_bar()+
  labs(x="Cluster K=8", y="Number of purchases")+
  scale_fill_brewer(palette = "Dark2",direction=-1)+
  theme(panel.background = element_rect(fill = "white", colour='black'))+coord_flip()

dat_clusB_weekend <- dat_clus_B[which(dat_clus_B$Weekend==1),]
w2 <- ggplot(dat_clusB_weekend, aes(x=as.factor(K8)))+geom_bar(fill="#669933")+
  labs(x="Cluster K=8", y="Number of purchases")+
  theme(panel.background = element_rect(fill = "white", colour='black'))
grid.arrange(w1,w2, widths=c(1,1))

## ANALYSE WEEKEND SALE

clusterB.weekend <-melt(dat_clus_B[which(dat_clus_B$Weekend==1),-51], id.vars = c("K8"))
clusterB.weekend <- cbind(clusterB.weekend, Weekday="Weekend")



clusterB.weekday <-melt(dat_clus_B[which(dat_clus_B$Weekend==0),-51], id.vars = c("K8"))
clusterB.weekday<- cbind(clusterB.weekday, Weekday="Weekday")


clusterB <- rbind(clusterB.weekend,clusterB.weekday)
clusterB <- clusterB[which(clusterB$value==1),]


clus1<- ggplot(clusterB[which(clusterB$K8==1),], aes(x=variable, fill=Weekday))+ geom_bar()+ 
  theme( axis.text.x  = element_text(angle=90, vjust=0.5,size=5), 
         panel.background = element_rect(fill = "white", colour='black'))+
  labs(title="Cluster 1")+
  scale_fill_brewer(palette = "Dark2",direction=-1)


clus2<- ggplot(clusterB[which(clusterB$K8==2),], aes(x=variable, fill=Weekday))+ geom_bar()+ 
  theme( axis.text.x  = element_text(angle=90, vjust=0.5,size=5), 
         panel.background = element_rect(fill = "white", colour='black'))+
  labs(title="Cluster 2")+
  scale_fill_brewer(palette = "Dark2",direction=-1)


clus3<- ggplot(clusterB[which(clusterB$K8==3),], aes(x=variable, fill=Weekday))+ geom_bar()+ 
  theme( axis.text.x  = element_text(angle=90, vjust=0.5,size=5), 
         panel.background = element_rect(fill = "white", colour='black'))+
  labs(title="Cluster 3")+
  scale_fill_brewer(palette = "Dark2",direction=-1)

clus4<- ggplot(clusterB[which(clusterB$K8==4),], aes(x=variable, fill=Weekday))+ geom_bar()+ 
  theme( axis.text.x  = element_text(angle=90, vjust=0.5,size=5), 
         panel.background = element_rect(fill = "white", colour='black'))+
  labs(title="Cluster 4")+
  scale_fill_brewer(palette = "Dark2",direction=-1)

clus5<- ggplot(clusterB[which(clusterB$K8==5),], aes(x=variable, fill=Weekday))+ geom_bar()+ 
  theme( axis.text.x  = element_text(angle=90, vjust=0.5,size=5), 
         panel.background = element_rect(fill = "white", colour='black'))+
  labs(title="Cluster 5")+
  scale_fill_brewer(palette = "Dark2",direction=-1)

clus6<- ggplot(clusterB[which(clusterB$K8==6),], aes(x=variable, fill=Weekday))+ geom_bar()+ 
  theme( axis.text.x  = element_text(angle=90, vjust=0.5,size=5), 
         panel.background = element_rect(fill = "white", colour='black'))+
  labs(title="Cluster 6")+
  scale_fill_brewer(palette = "Dark2",direction=-1)

clus7<- ggplot(clusterB[which(clusterB$K8==7),], aes(x=variable, fill=Weekday))+ geom_bar()+ 
  theme( axis.text.x  = element_text(angle=90, vjust=0.5,size=5), 
         panel.background = element_rect(fill = "white", colour='black'))+
  labs(title="Cluster 7")+
  scale_fill_brewer(palette = "Dark2",direction=-1)

clus8<- ggplot(clusterB[which(clusterB$K8==8),], aes(x=variable, fill=Weekday))+ geom_bar()+ 
  theme( axis.text.x  = element_text(angle=90, vjust=0.5,size=5), 
         panel.background = element_rect(fill = "white", colour='black'))+
  labs(title="Cluster 8")+
  scale_fill_brewer(palette = "Dark2",direction=-1)


grid.arrange(grobs=list(clus1,clus2,clus3,clus4),nrow=4)

grid.arrange(grobs=list(clus5,clus6,clus7,clus8),nrow=4)

