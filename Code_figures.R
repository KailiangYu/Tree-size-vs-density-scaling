#SoDA is no longer available online and requires manual installation offline. The file name is SoDA_1.0-6.tar.gz.

library(spatialreg)
library(ggplot2)
library(randomForest)
library(pdp)
library('MASS')
library(SoDA)
library(spdep)
library("dplyr") 
library("lmodel2") 
library("plotrix") 
library("viridis") 

setwd("~/Documents/Collab/submission")
total_RMA<-read.csv('data_submission_final.csv')
head(total_RMA)

data_tro<-total_RMA[which(total_RMA$Biome=='Tropical'),]
data_te<-total_RMA[which(total_RMA$Biome=='Temperate'),]
data_bo<-total_RMA[which(total_RMA$Biome=='Boreal'),]

# note that the scaling slope is consistent, but the specific locations of each point could be different while ploting

pdf(file = paste("Figure_2a.pdf",sep=""), width = 4.4, height = 3.6)

ggplot(data = total_RMA, aes(
  x = factor(Biome),
  y = Slope,
  alpha = 0.2)) + labs(x="",y="Scaling slope") +  
  geom_hline(yintercept = -1.5,size = 1,color = "dark red") +
  geom_hline(yintercept = -1.33,size = 1,linetype="dashed",color = "dark blue") +
  geom_jitter(alpha = 0.3, size = 0.5, color = "dark green") +
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1) +
  theme_minimal() +
  scale_alpha(guide = 'none') +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(),
        legend.position = "none",
        axis.text.x = element_text(angle = 0, hjust = 1,size=14),
        axis.text.y = element_text(size=14),axis.title=element_text(size=14)) +
  coord_flip(ylim = c(-4,0)) +
  scale_y_continuous(breaks = c(0,-2,4,-4)) 

dev.off()

# figure 2 use bootstrap to get probility of mean 
# use anova and compare means to show significant difference
summary(aov(Slope ~ Biome, data = total_RMA))

index_Tro<-which(total_RMA$Biome=='Tropical')
data_Tro<-total_RMA[index_Tro,]

min(data_Tro$Slope)
max(data_Tro$Slope)

index_Te<-which(total_RMA$Biome=='Temperate')
data_Te<-total_RMA[index_Te,]

min(data_Te$Slope)
max(data_Te$Slope)

index_Bo<-which(total_RMA$Biome=='Boreal')
data_Bo<-total_RMA[index_Bo,]

min(data_Bo$Slope)
max(data_Bo$Slope)

Tro_boot.sample <- list()
for(i in 1:1000){
  Tro_boot.sample[[i]] <- sample(data_Tro$Slope,size = length(data_Tro$Slope)*0.95, replace = TRUE)
}

Te_boot.sample <- list()
for(i in 1:1000){
  Te_boot.sample[[i]] <- sample(data_Te$Slope,size = length(data_Te$Slope)*0.95, replace = TRUE)
}

Bo_boot.sample <- list()
for(i in 1:1000){
  Bo_boot.sample[[i]] <- sample(data_Bo$Slope,size = length(data_Bo$Slope)*0.95, replace = TRUE)
}

Tro_boot.mean <- data.frame(unlist(lapply(Tro_boot.sample, mean)))
colnames(Tro_boot.mean)<-c('Slope')
Te_boot.mean <- data.frame(unlist(lapply(Te_boot.sample, mean)))
colnames(Te_boot.mean)<-c('Slope')
Bo_boot.mean <- data.frame(unlist(lapply(Bo_boot.sample, mean)))
colnames(Bo_boot.mean)<-c('Slope')

Tro_boot.mean$Biome<-'Tropical'
Te_boot.mean$Biome<-'Temperate'
Bo_boot.mean$Biome<-'Boreal'

data_all<-rbind(Tro_boot.mean,Te_boot.mean,Bo_boot.mean)

# use anova and compare means to show significant difference
summary(aov(Slope ~ Biome, data = data_all))
t.test(Tro_boot.mean[,1], Te_boot.mean[,1])
t.test(Tro_boot.mean[,1], Bo_boot.mean[,1])
t.test(Te_boot.mean[,1], Bo_boot.mean[,1])

Bomperatest<-factor(data_all$Biome)
data_all$Biome<-factor(Bomperatest,levels(Bomperatest)[(c(3,2,1))])

pdf(file = paste("Figure_2b.pdf",sep=""), width = 4.4, height = 3.6)  

ggplot(data = data_all, aes(Slope, fill = Biome))+ labs(x="Scaling slope",y="Density") + geom_density(alpha = 0.1) +
  theme_minimal() +
  scale_alpha(guide = 'none') +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(),
        legend.position = "none",
        axis.text.x = element_text(angle = 0, hjust = 1,size=14),
        axis.text.y = element_text(size=14),axis.title=element_text(size=14)) + theme(legend.position=c(0.2,0.85)) + theme(legend.text=element_text(size=12)) + theme(legend.title = element_blank()) + xlim(c(-2.5,-1))

dev.off()

# for Figure 3 using data in North America
index_North<-which(total_RMA$Source%in%c("FIA","USA","Can")) # choose data of North Ameirca
total_North<-total_RMA[index_North,]

imp.data_inv<-na.omit(total_North[,c(4,5,6,9,10,11,12,13,14,15)])
# scale environmental data
imp.data_inv[,c(4:10)]<-scale(imp.data_inv[,c(4:10)])

start.model <- lm(Slope~., data=imp.data_inv[,c(3:10)])  	# make a linear model

# account for spatial autocorrletaion
spat.data<-imp.data_inv

# add noise to latitude and longitude
spat.data$Lon<-spat.data$Lon+rnorm(length(spat.data$Lon),0,0.0001)
spat.data$Lat<-spat.data$Lat+rnorm(length(spat.data$Lon),0,0.0001)

plot.xy_unique<-geoXY(spat.data$Lon, spat.data$Lat, unit=1000)# unit is 1km
k1<-knn2nb(knearneigh(plot.xy_unique,k=1))
dsts <- unlist(nbdists(k1, plot.xy_unique))
summary(dsts)
hist(dsts,breaks=200)
quant95 <- as.numeric(quantile(dsts,0.95))#follwoing Portier 2018 LandScapeEcol
#https://link.springer.com/article/10.1007/s10980-017-0578-8
#Portier 2018: "the lag is defined as the distance between two neighTers when all observations are equally spaced out."
#"the distance at which 95% of the LDs had at least one neighTer"
#Use jittered one for analysis (because doesn't run with unique coordinates - distance cannot be zero)
#but using the distance (quan95) estimated by the unique corrdinate
plot.xy<-geoXY(spat.data$Lat, spat.data$Lon, unit=1000) # unit is 1km
xy.dist<-dist(plot.xy)
plot(plot.xy) #visual aid to see the clustering of plots 
nbsr1<-dnearneigh(as.matrix(plot.xy),0,quant95) # calculate the neighTeurs distance (based on lags)
listw1<-nb2listw(nbsr1, glist=NULL, style="W", zero.policy=TRUE)#Bivand 2018

# evaluate spatial autocorrleation for stepAIC model
North_AIC_resid<-resid(start.model)
North_AIC_resid_moran<-moran.test(North_AIC_resid,listw1,zero.policy=TRUE)

# use SAR model to accont for spatial autocorrelation
#SEM Spatial Error Model  
NA_SEM_reg_age=errorsarlm(spat.data$Slope ~.,data=spat.data[,c(3:10)], listw1,zero.policy=TRUE)
summary(NA_SEM_reg_age)

# quantify the R2 of model fittings
summary(NA_SEM_reg_age, correlation=TRUE, Nagelkerke=TRUE, Hausman=TRUE)

NA_SEM_resid_age<-resid(NA_SEM_reg_age)
NA_SEM_resid_moran_age<-moran.test(NA_SEM_resid_age,listw1,zero.policy=TRUE)
#Spatial Hausman Test
Hausman.test(NA_SEM_reg_age)

# synthesize results for plot
Estimate<-coef(summary(NA_SEM_reg_age))[,"Estimate"]
Std_Error<-coef(summary(NA_SEM_reg_age))[,"Std. Error"]
Vari<-c('Intercept','Rainfall seasonality','PWQ','pH','SOC',
        'Basal area','Size variation','Age')

Predictor_scaling<-as.data.frame(cbind(Vari,Estimate,Std_Error))

# delete intercept
Predictor_scaling<-Predictor_scaling[-c(1),]
Bomperatest<-factor(Predictor_scaling$Vari)
Predictor_scaling$Vari<-factor(Bomperatest,levels(Bomperatest)[rev(c(1,2,6,4,5,7,3))])

Predictor_scaling$Estimate<-as.numeric(Predictor_scaling$Estimate)
Predictor_scaling$Std_Error<-as.numeric(Predictor_scaling$Std_Error)

pdf(file = paste("Figure_3a.pdf",sep=""), width = 4.4, height = 3.6)

p <- ggplot(Predictor_scaling, aes(x=Predictor_scaling$Vari, y=(Predictor_scaling$Estimate))) + 
  geom_point(size=3, shape=21,position=position_dodge(.6),color=c("#E7298A","#E7298A","#D95F02","#D95F02",'#66A61E',"#66A61E",'#66A61E')) +
  geom_errorbar(aes(ymin=(Predictor_scaling$Estimate)-1.96*(Predictor_scaling$Std_Error), ymax=(Predictor_scaling$Estimate)+1.96*(Predictor_scaling$Std_Error)), width=.2,
                position=position_dodge(.6),color=c("#E7298A","#E7298A","#D95F02","#D95F02",'#66A61E',"#66A61E",'#66A61E')) 

p + scale_x_discrete(labels=)+ labs(x=c(),y="Estimate coefficient") + geom_hline(yintercept = 0,size = 1,color="grey") + coord_flip() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        panel.background = element_blank(),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        axis.title=element_text(size=13)) + theme(legend.title=element_blank(),legend.key=element_rect(fill='white')) + theme(legend.position=c(0.13,0.88)) + ylim(-0.6,0.6)    

dev.off()

# for Figure 3b
total_North_ran<-imp.data_inv[,c(3:10)]

impToPlot<-list()
for (i in 1:100) {
  set.seed(i)
  mean_ran_for<-randomForest(Slope ~., 
                             data = total_North_ran, ntree=200, importance=TRUE,na.action=na.roughfix, mtry=3)
  impToPlot[[i]]<-unname(randomForest::importance(mean_ran_for)[,1])
}
str(impToPlot)
mat_mean<-matrix(unlist(impToPlot), ncol = 7, byrow = TRUE)
mean_clo<-colMeans(mat_mean)

mf <- function(x){sd(mat_mean[,x])}
mean_st_clo<-sapply(1:length(mat_mean[1,]),mf)   # this is two times of standard error

Type_gg<-c('RS','PWQ','PH','SOC','Basal','CV','Age')

# get data.frame
ran_gg<-data.frame(mean_clo,mean_st_clo,Type_gg)

Vari<-c('Rainfall seasonality','PWQ','pH','SOC','Basal area','Size variation','Age')
Random1<-data.frame(cbind(ran_gg,Vari))

Bomperatest<-factor(Random1$Vari)
Random1$Vari<-factor(Bomperatest,levels(Bomperatest)[rev(c(1,2,6,4,5,7,3))])

pdf(file = paste("Figure_3b.pdf",sep=""), width = 4.4, height = 3.6)

p <- ggplot(Random1, aes(x=Vari, y=(mean_clo))) + 
  geom_point(size=3, shape=21,position=position_dodge(.6),color=c("#E7298A","#E7298A","#D95F02","#D95F02",'#66A61E',"#66A61E",'#66A61E')) +
  geom_errorbar(aes(ymin=(mean_clo)-(mean_st_clo), ymax=(mean_clo)+(mean_st_clo)), width=.2,
                position=position_dodge(.6),color=c("#E7298A","#E7298A","#D95F02","#D95F02",'#66A61E',"#66A61E",'#66A61E')) 

p + scale_x_discrete(labels=)+ labs(x=c(),y="%IncMSE") + coord_flip() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        panel.background = element_blank(),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        axis.title=element_text(size=13)) + theme(legend.title=element_blank(),legend.key=element_rect(fill='white')) + theme(legend.position=c(0.13,0.88))    

dev.off()

#for Figure 3c
mean_ran_for<-randomForest(Slope ~., 
                           data = total_North_ran, ntree=200, importance=TRUE,na.action=na.roughfix, mtry=3)


pd <- partial(mean_ran_for, pred.var = c("Age", "RS"))

# Default PDP
pdf(file = paste("Figure_3c.pdf",sep=""), width = 4.4, height = 3.6)

rwb <- colorRampPalette(c("#4575B4", "white", "#D73027"))
plotPartial(pd, contour = TRUE,pdp.lwd = 2, col.regions = rwb, pdp.lty = 2,xlab='Age',ylab ='Rainfall seasonality') 

dev.off()

# get the data with age and plot figure 3d
ind<-which(is.na(total_North$Age))
total_North<-total_North[-c(ind),]

# bin using number of points for biome
# temperate 
ind_Te<-which(total_North$Biome=='Temperate')
total_North_Te<-total_North[ind_Te,]

# sort by age
total_thinn_Te<-total_North_Te[order(total_North_Te$Age),]
# define group
group<-c(rep(1:18,each=30),rep(19,length=20))
total_scaling_Te<-cbind(total_thinn_Te,group)
total_scaling_Te$group<-factor(total_scaling_Te$group)

scaling_mean_Te<-total_scaling_Te %>% group_by(group) %>%
  summarise(Age_bin=mean(Age,na.rm=T),
            mean=mean(Slope,na.rm=T),
            str = std.error(Slope,na.rm=T))
Biom<-rep('Temperate',length=19)

scaling_mean_Te1<-cbind(scaling_mean_Te,Biom)

# boreal
ind_Bo<-which(total_North$Biome=='Boreal')
total_North_Bo<-total_North[ind_Bo,]

# sort by age
total_thinn_Bo<-total_North_Bo[order(total_North_Bo$Age),]
# define group
group<-c(rep(1:19,each=30),rep(20,length=26))
total_scaling_Bo<-cbind(total_thinn_Bo,group)
total_scaling_Bo$group<-factor(total_scaling_Bo$group)

scaling_mean_Bo<-total_scaling_Bo %>% group_by(group) %>%
  summarise(Age_bin=mean(Age,na.rm=T),
            mean=mean(Slope,na.rm=T),
            str = std.error(Slope,na.rm=T))

Biom<-rep('Boreal',length=20)

scaling_mean_Bo1<-cbind(scaling_mean_Bo,Biom)

scaling_mean_bim<-rbind(scaling_mean_Bo1,scaling_mean_Te1)

# get temperate data
ind_Bo<-which(scaling_mean_bim$Biom=='Boreal')
ind_Te<-which(scaling_mean_bim$Biom=='Temperate')

data_Bo<-scaling_mean_bim[ind_Bo,]
data_Te<-scaling_mean_bim[ind_Te,]

mod1<-lmodel2(mean~ Age_bin, data = data_Bo, "interval", "interval", 99)
summary(lm(mean~ Age_bin, data = data_Bo))

mod<-lmodel2(mean~ Age_bin, data = data_Te, "interval", "interval", 99)
summary(lm(mean~ Age_bin, data = data_Te))

Bomperatest<-factor(scaling_mean_bim$Biom)
scaling_mean_bim$Biom<-(factor(Bomperatest,levels(Bomperatest)[(c(2,1))]))

pdf(file = paste("Figure_3d.pdf",sep=""), width = 4.4, height = 3.6)

ggplot(scaling_mean_bim,aes(x=Age_bin,y=mean, color=Biom, shape=Biom)) + 
  geom_point(aes(color=Biom, shape=Biom), size=2.5) + 
  scale_color_manual(values=c('red','blue')) +
  geom_errorbar(data=scaling_mean_bim, aes(x=Age_bin, ymin=mean-str, ymax=mean+str), width=0.09) + 
  labs(x=c('Mean age'),y="Scaling slope") +
  geom_abline(intercept=mod$regression.results[1,2],
              slope=mod$regression.results[1,3],color='red') +
  geom_abline(intercept=mod1$regression.results[1,2],
              slope=mod1$regression.results[1,3],color='blue') +
  scale_fill_viridis(limits=c(0,30),
                     breaks=c(0,15,30)) + theme_bw() +
  theme_classic() + theme(legend.position="bottom") + theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        axis.title=element_text(size=14),
        legend.text=element_text(size=13)) + theme(legend.title=element_blank(),legend.key=element_rect(fill='white')) + theme(legend.position=c(0.2,0.91)) + ylim(-2.5,-0.5)

dev.off()

# For Figure 4a, the code using spatial error model is the same as figure 3a. 

# For Figure 4b

Bomperatest<-factor(total_RMA$Biome)
total_RMA$Biome<-factor(Bomperatest,levels(Bomperatest)[(c(3,2,1))])

pdf(file = paste("Figure_4b.pdf",sep=""), width = 4.6, height = 3.6)

ggplot(total_RMA, aes(x=Slope, y=Sink, color=Biome, shape=Biome))+ labs(x=c("Scaling slope"),y="Biomass accumulation rate") +
  geom_point(aes(size=Biome)) + 
  geom_smooth(method=lm, aes(fill=Biome),alpha=0.4)+
  # geom_hline(yintercept = 0,size = 1,color = "black") +
  scale_shape_manual(values=c(16, 3, 17))+ 
  scale_color_viridis(discrete = TRUE, option = "D")+
  scale_fill_viridis(discrete = TRUE, alpha=0.1) +
  scale_size_manual(values=c(2,2,2))+
  theme_classic() + theme(legend.position="bottom") + theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title=element_text(size=12),
        legend.text=element_text(size=8)) + theme(legend.title=element_blank(),legend.key=element_rect(fill='white'))

dev.off()


