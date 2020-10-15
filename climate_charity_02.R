###############
# Set up data #
###############

library(factoextra)
library(ggplot2)
library(RColorBrewer)
library(gridExtra)
library(grid)
library(egg)
library(sjPlot)

#Import data
library(readxl)
data.backup <- read_excel("data.xlsx", sheet = "cleaned")
data <- na.omit(data.backup)
nrow(data)

#####################
# Calculate indices #
#####################

library(multilevel) #Used to calculate Cronbach's alpha and item-total correlations
library(dplyr) #Used to manipulate data frames
names(data)

#Political interest
pol.int <- data[,7]
pol.int <- pol.int %>% mutate_all(as.numeric)
colMeans(na.omit(pol.int))
apply(na.omit(pol.int),2,mean)
apply(na.omit(pol.int),2,sd)

#Political spectrum
pol.spec <- data[,8]
pol.spec <- pol.spec %>% mutate_all(as.numeric)
apply(na.omit(pol.spec),2,mean)
apply(na.omit(pol.spec),2,sd)

#Parties stated
parties <- data[,9:12]
parties <- parties %>% mutate_all(as.numeric)
apply(na.omit(parties),2,mean)
apply(na.omit(parties),2,sd)

#Postmaterialist index
postmaterialism <- data[,13:14]
postmaterialism <- postmaterialism %>% mutate_all(as.numeric)
postmaterialism$index <- ifelse(postmaterialism[,1]+postmaterialism[,2]==4,
                                    "materialist",
                                    ifelse(
                                      postmaterialism[,1]+postmaterialism[,2]==6,
                                      "postmaterialist",
                                      "mixed"
                                    ))
table(postmaterialism$index)
prop.table(table(postmaterialism$index))

#Objective finance
finance.o <- data[,45:47]
finance.o <- finance.o %>% mutate_all(as.numeric)
finance.o[,2] <- 6-finance.o[,2]
finance.o[,3] <- 6-finance.o[,3]
cronbach(finance.o)
item.total(finance.o)
apply(finance.o,2,mean)
apply(finance.o,2,sd)
mean(rowMeans(finance.o))
sd(rowMeans(finance.o))

#Subjective finance
finance.s <- data[,48]
finance.s <- finance.s %>% mutate_all(as.numeric)
mean(rowMeans(finance.s))
sd(rowMeans(finance.s))

#Climate change concern scale
climate <- cbind(data[,61:67])
climate <- climate %>% mutate_all(as.numeric)
climate[,5] <- 6-climate[,5]
cronbach(climate)
item.total(climate)
apply(climate,2,mean)
apply(climate,2,sd)
mean(rowMeans(climate))
sd(rowMeans(climate))

#Beliefs and values scale
beliefs <- data[,68:87]
beliefs <- beliefs %>% mutate_all(as.numeric)
cronbach(na.omit(beliefs))
item.total(na.omit(beliefs))
apply(na.omit(beliefs),2,mean)
apply(na.omit(beliefs),2,sd)
mean(rowMeans(na.omit(beliefs)))
sd(rowMeans(na.omit(beliefs)))

#Self-reported altruism scale
altruism <- data[,88:107]
altruism <- altruism %>% mutate_all(as.numeric)
cronbach(na.omit(altruism))
item.total(na.omit(altruism))
apply(na.omit(altruism),2,mean)
apply(na.omit(altruism),2,sd)
mean(rowMeans(na.omit(altruism)))
sd(rowMeans(na.omit(altruism)))

#Individual-communitarian cultural worldview
individualist <- data[,49:54]
individualist <- individualist %>% mutate_all(as.numeric)
individualist[,1] <- 6-individualist[,1]
individualist[,5] <- 6-individualist[,5]
individualist[,6] <- 6-individualist[,6]
cronbach(na.omit(individualist))
item.total(na.omit(individualist))
apply(na.omit(individualist),2,mean)
apply(na.omit(individualist),2,sd)
mean(rowMeans(na.omit(individualist)))
sd(rowMeans(na.omit(individualist)))

#Hierarchy-egalitarian cultural worldview
hierarchical <- data[,55:60]
hierarchical <- hierarchical %>% mutate_all(as.numeric)
hierarchical[,2] <- 6-hierarchical[,2]
hierarchical[,3] <- 6-hierarchical[,3]
hierarchical[,5] <- 6-hierarchical[,5]
cronbach(na.omit(hierarchical))
item.total(na.omit(hierarchical))
apply(na.omit(hierarchical),2,mean)
apply(na.omit(hierarchical),2,sd)
mean(rowMeans(na.omit(hierarchical)))
sd(rowMeans(na.omit(hierarchical)))

#Political parties
compass <- data[,15:44]
compass <- compass %>% mutate_all(as.numeric)

r.alp <- c(3,4,5,4,5,4,3,4,5,4,4,4,5,3,1,2,4,5,1,5,4,5,5,5,5,5,5,4,5,3)
r.lib <- c(3,2,3,2,4,3,3,4,4,2,4,3,4,2,1,5,1,5,5,4,2,2,1,2,3,2,2,5,2,3)
r.grn <- c(5,5,5,5,5,5,1,3,5,5,5,5,5,3,5,1,5,1,1,5,5,5,5,4,5,5,5,1,5,5)
r.on <- c(3,5,3,2,3,3,2,4,1,1,1,3,1,1,1,5,1,5,5,1,1,5,4,1,3,1,1,5,1,5)

compass.dist.max <- abs(compass-3)+2
compass.dist.alp <- abs(sweep(compass,2,r.alp,"-"))
compass.dist.lib <- abs(sweep(compass,2,r.lib,"-"))
compass.dist.grn <- abs(sweep(compass,2,r.grn,"-"))
compass.dist.on <- abs(sweep(compass,2,r.on,"-"))

compass.dist <- data.frame(
  "alp" = 1-(rowSums(compass.dist.alp,na.rm=TRUE)/rowSums(compass.dist.max,na.rm=TRUE)),
  "lib" = 1-(rowSums(compass.dist.lib,na.rm=TRUE)/rowSums(compass.dist.max,na.rm=TRUE)),
  "grn" = 1-(rowSums(compass.dist.grn,na.rm=TRUE)/rowSums(compass.dist.max,na.rm=TRUE)),
  "on" = 1-(rowSums(compass.dist.on,na.rm=TRUE)/rowSums(compass.dist.max,na.rm=TRUE))
  )


compass.dist.untransformed <- compass.dist
compass.dist <- compass.dist.untransformed*4+1

apply(na.omit(compass.dist*5),2,mean)
apply(na.omit(compass.dist*5),2,sd)

cor.test(na.omit(cbind(parties[,1],compass.dist[,2]))[,1],na.omit(cbind(parties[,1],compass.dist[,2]))[,2])
0.7444677/9.1949 #stdev

cor.test(na.omit(cbind(parties[,2],compass.dist[,1]))[,1],na.omit(cbind(parties[,2],compass.dist[,1]))[,2])
0.1686095/1.4106 #stdev

cor.test(na.omit(cbind(parties[,3],compass.dist[,3]))[,1],na.omit(cbind(parties[,3],compass.dist[,3]))[,2])
0.4507522/4.164 #stdev

cor.test(na.omit(cbind(parties[,4],compass.dist[,4]))[,1],na.omit(cbind(parties[,4],compass.dist[,4]))[,2])
0.4858497/4.5838 #stdev

#Put all these scales to a new data frame
data2 <- data.frame("group" = data$group,
                    "day" = data$day,
                    "donation" = as.numeric(data$donation),
                    "gender" = data$gender,
                    "Age" = as.numeric(data$age),
                    "country" = data$country,
                    "Political Interest" = pol.int$Q1,
                    "Political Right" = pol.spec$Q2,
                    "Liberal S" = parties$Q3,
                    "Labor S" = parties$Q4,
                    "Greens S" = parties$Q5,
                    "One Nation S" = parties$Q6,
                    "Liberal C" = compass.dist[,2],
                    "Labor C" = compass.dist[,1],
                    "Greens C" = compass.dist[,3],
                    "One Nation C" = compass.dist[,4],
                    "Objective Finance" = rowMeans(finance.o),
                    "Subjective Finance" = finance.s$Q42,
                    "Climate Concern" = rowMeans(climate),
                    "Religious" = rowMeans(beliefs),
                    "Altruism" = rowMeans(altruism),
                    "Individualist" = rowMeans(individualist),
                    "Hierarchical" = rowMeans(hierarchical)
                    )

aggregate(donation~group,data2,mean); aggregate(donation~group,data2,sd)

############
# Analysis #
############

#PCA and colour by group
#5,7-22 are explanatory
#3 is response
#1 is explanatory group
#2 is covariate

data2.pca <- prcomp(na.omit(data2[,c(5,7:23)]), center = TRUE,scale. = TRUE)
groups <- na.omit(data2[,c(1,2,5,7:23)])$group
day <- na.omit(data2[,c(1,2,5,7:23)])$day

fviz_pca_ind(data2.pca,
                col.ind=groups,
                repel = FALSE)
fviz_eig(data2.pca)

loadings <- data2.pca$rotation
axes <- data.frame(predict(data2.pca, newdata = data2))
data3 <- cbind(data2,axes)

head(data3)
data3$PC1scaled <- 1-(data3$PC1-min(na.omit(data3$PC1)))/(max(na.omit(data3$PC1))-min(na.omit(data3$PC1)))
data3$PC2scaled <- (data3$PC2-min(na.omit(data3$PC2)))/(max(na.omit(data3$PC2))-min(na.omit(data3$PC2)))
data3$groupn <- 4-as.numeric(data3$group)
data3$groupf <- factor(data3$groupn,levels=c(1,2,3))
levels(data3$groupf) <- c("Low-impact Message","Med-impact Message",
                          "High-impact Message")


xmi <- min(data3$PC1)
xma <- max(data3$PC1)
ymi <- min(data3$PC2)
yma <- max(data3$PC2)

xbreaks <- c(xmi,((xma+xmi)/2+xmi)/2,(xma+xmi)/2,(xma+(xma+xmi)/2)/2,xma)
ybreaks <- c(ymi,((yma+ymi)/2+ymi)/2,(yma+ymi)/2,(yma+(yma+ymi)/2)/2,yma)

#PC Plot
data2.pca$rotation[,1] <- -1*data2.pca$rotation[,1]
data2.pca$x[,1] <- -1*data2.pca$x[,1]

gpc <- fviz_pca_biplot(data2.pca,
                       axes=c(1,2),
                       repel = TRUE,
                       col.var = "black",
                       col.ind = "gray50",
                       label="var",
                       labelsize=4) + theme_classic() + xlab("PC1 (40%)") + ylab("PC2 (9.1%)") +
  scale_x_continuous(breaks=xbreaks,
                     labels=c(0.00,0.25,0.50,0.75,1.00)) +
  scale_y_continuous(breaks=ybreaks,
                     labels=c(0.00,0.25,0.50,0.75,1.00)) +
  theme(plot.title = element_blank())
gpc
#ggsave("figure1.png", plot = gpc, width = 140, height = 140*233/324, units = "mm", dpi=300)

#Linear regression
data3.lm <- lm(donation~PC1scaled+PC2scaled+groupn+PC1scaled:groupn+PC2scaled:groupn,data=data3)
summary(data3.lm)

tab_model(data3.lm,
          show.ci=0.95,
          show.se=TRUE,
          emph.p=FALSE,
          dv.labels=c("Donation ($)"),
          pred.labels=c("Intercept","PC1","PC2","Impact",
                                 "PC1*Impact","PC2*Impact"),
          file="regressionn1.doc")

par(mfrow=c(2,2))
plot(data3.lm, which=1:4)
par(mfrow=c(1,1))


####################################
#### Testing alternative models ####
####################################

data3.lm.day <- lm(donation~PC1scaled+PC2scaled+groupn+PC1scaled:groupn+PC2scaled:groupn+day,data=data3)
summary(data3.lm.day)

tab_model(data3.lm.day,
          show.ci=0.95,
          show.se=TRUE,
          emph.p=FALSE,
          dv.labels=c("Donation ($)"),
          pred.labels=c("Intercept","PC1","PC2","Impact",
                        "PC1*Impact","PC2*Impact","Hypothetical Rewards"),
          file="regressionn1_day.doc")

data3.lm.gender <- lm(donation~PC1scaled+PC2scaled+groupn+PC1scaled:groupn+PC2scaled:groupn+gender,data=data3)
summary(data3.lm.gender)

tab_model(data3.lm.gender,
          show.ci=0.95,
          show.se=TRUE,
          emph.p=FALSE,
          dv.labels=c("Donation ($)"),
          pred.labels=c("Intercept","PC1","PC2","Impact",
                        "PC1*Impact","PC2*Impact","Gender"),
          file="regressionn1_gender.doc")



#####################
#### Effect size ####
#####################

predict(data3.lm,newdata=data.frame("PC1scaled"=rep(mean(data3$PC1scaled),3),
                          "PC2scaled"=rep(mean(data3$PC1scaled),3),
                          "groupn" = c(1,2,3)),
        se.fit=TRUE)


########################
#### Raw data graph ####
########################

m3 <- ggplot(aes(x=PC2scaled,y=donation,colour=groupf),data=data3)
m3 <- m3 + geom_smooth(method="lm",formula=y~x,se=TRUE,alpha=0.2)
m3 <- m3 + geom_point()
m3 <- m3 + xlim(NA,0.75) + ylim(-1,11)
m3 <- m3 + theme_classic()
m3 <- m3 + theme(legend.position="none")
m3 <- m3 + facet_wrap(~groupf,ncol=3)
m3 <- m3 + scale_color_manual(values=
                                c("gray50", "steelblue3", "steelblue2"))
m3 <- m3 + scale_color_manual(values=
                                c("goldenrod4", "goldenrod3", "goldenrod2"))
m3 <- m3 + ylab("Donation ($)") + xlab("PC2")
m3

#ggsave("figure2_scatter.png", plot = m3, width = 180, height = 140*233/324, units = "mm", dpi=300)
