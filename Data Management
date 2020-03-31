library(dplyr)
library(foreign)
library(sjmisc)
library(srvyr)
library(ggplot2)

# 1. Load Data ------------------------------------------------------------

setwd("~\\PISA 2015\\Correlations_PISA 2015_Well-being\\Well-being")
Survey <- c("Option_ICTQ","Option_PQ","ADMINMODE")
Demo <- c("ST004D01T","IMMIG","ESCS")
Student <- c("MOTIVAT","ANXTEST","EMOSUPS","BELONG","TEACHSUP","PV1SCIE","PV2SCIE","PV3SCIE",
             "PV4SCIE","PV5SCIE","PV6SCIE","PV7SCIE","PV8SCIE","PV9SCIE","PV10SCIE")
Well_being <- c("ST016Q01NA")
weight <- c("W_FSTUWT","SENWT")

list <- dir(pattern = "*.sav") 
ldf <- list()
for (k in 1:length(list)){
  ldf[[k]] <- read.spss(list[k],use.value.labels = FALSE,use.missings = TRUE,to.data.frame = TRUE)%>%
    select(Survey,Demo,Student,Well_being,weight )
}

names(ldf) <- list

for(i in 1:length(list)){
  assign(list[i], as.data.frame(ldf[[i]]))
}

# 2. Data Managment by cultures -------------------------------------------
Anglo <- rbind(IRL.sav,USA.sav,GBR.sav)
Latin_Eu <- rbind(BEL.sav,FRA.sav,ITA.sav,PRT.sav,ESP.sav,QES.sav)
Nordic <- rbind(FIN.sav,ISL.sav)
German <- rbind(AUT.sav,DEU.sav,LUX.sav,NLD.sav,CHE.sav)
East_EU <- rbind(CZE.sav,GRC.sav,HUN.sav,LVA.sav,POL.sav,RUS.sav,
                 SVK.sav,BGR.sav,EST.sav,HRV.sav,LTU.sav,MNE.sav,SVN.sav)
Latin_A <- rbind(BRA.sav,MEX.sav,URY.sav,CHL.sav,COL.sav,CRI.sav,DOM.sav,PER.sav)
Middle_E <- rbind(TUN.sav,TUR.sav,ARE.sav,QAT.sav)
South_A <- THA.sav
East_A <- rbind(HKG.sav,JPN.sav,KOR.sav,MAC.sav,QCH.sav,TAP.sav)


# 3. Frequency :"MOTIVAT","ANXTEST","EMOSUPS","BELONG","TEACHSUP", and Well-being -------------
# with wieght
culture <- list(Anglo,Latin_Eu,Nordic,German ,East_EU,Latin_A,Middle_E,South_A,East_A)
Anglo_F <- culture[[1]]%>%as_survey_design(weights=SENWT)%>%
  select(c("MOTIVAT","ANXTEST","EMOSUPS","BELONG","TEACHSUP","ST016Q01NA"))%>%
  summarise_all(list(~ survey_mean(., na.rm = TRUE)))


Freq <- function(Data){
  Data_F <- Data%>%as_survey_design(weights=SENWT)%>%
    select(c("MOTIVAT","ANXTEST","EMOSUPS","BELONG","TEACHSUP","ST016Q01NA"))%>%
    summarise_all(list(~ round(survey_mean(., na.rm = TRUE),digits = 3)))
  return(Data_F)
  }
  


results <- NULL
for( i in 1:length(culture)){
  
  Des <- Freq (Data = culture[[i]])
  results<- rbind(results,Des)
}

Sample <-unlist(lapply(culture, nrow))
results <- cbind(results,Sample)

rownames(results) <- c("Anglo","Latin_Eu","Nordic","German" ,"East_EU","Latin_A","Middle_E","South_A","East_A")
write.csv(results,file="Frequency_Social.csv",sep=",")


#4. 95 CI Plot ----------------------------------------------------------------

lower_ci <- function(mean, se, n, conf_level = 0.95){
  lower_ci <- round(mean - qt(1 - ((1 - conf_level) / 2), n - 1) * se,digits=3)
}
upper_ci <- function(mean, se, n, conf_level = 0.95){
  upper_ci <- round(mean + qt(1 - ((1 - conf_level) / 2), n - 1) * se,digits = 3)
}


# MOTIVAT
max(results$MOTIVAT);min(results$MOTIVAT)#0.708 and -0.334
lower <- NULL
upper <- NULL
for( i in 1:nrow(results)){
  res <- lower_ci(mean=results[i,1],se=results[i,2],n=results[i,13])
  res_u <- upper_ci(mean=results[i,1],se=results[i,2],n=results[i,13])
  lower <- rbind(lower,res)
  upper <- rbind(upper, res_u)
}

Regions <- c("Anglo","Latin Europe","Nordic","German" ,"East Europe","Latin America ","Middle East","South Asia","East Asia")
MOTIVAT <- as.data.frame(cbind(Regions,results$MOTIVAT,results$MOTIVAT_se,lower,upper))
colnames(MOTIVAT) <- c("Regions","Mean","SE","lower","upper")
MOTIVAT[,2:5] <- apply(MOTIVAT[,2:5],2,as.numeric)

ggplot(MOTIVAT,aes(x=as.factor(Regions),y=Mean,label=as.factor(Regions)))+
  geom_point()+
  geom_errorbar(aes(x=as.factor(Regions),ymin=lower,ymax=upper))+
  geom_text(size=4,hjust="center", vjust=3,aes(color=as.factor(Regions)))+
  theme(legend.position = "none")+
  ylab("Achievement Motivation")+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent",colour = NA),
        axis.line = element_line(colour = "black"))+
  scale_y_continuous(limits = c(-0.5, 0.8), breaks = seq(-0.5,0.8, by = 0.1))

# ANXTEST
max(results$ANXTEST);min(results$ANXTEST)#0.379 and -0.313
lower <- NULL
upper <- NULL
for( i in 1:nrow(results)){
  res <- lower_ci(mean=results[i,3],se=results[i,4],n=results[i,13])
  res_u <- upper_ci(mean=results[i,3],se=results[i,4],n=results[i,13])
  lower <- rbind(lower,res)
  upper <- rbind(upper, res_u)
}

Regions <- c("Anglo","Latin Europe","Nordic","German" ,"East Europe","Latin America ","Middle East","South Asia","East Asia")
ANXTEST <- as.data.frame(cbind(Regions,results$ANXTEST,results$ANXTEST_se,lower,upper))
colnames(ANXTEST) <- c("Regions","Mean","SE","lower","upper")
ANXTEST[,2:5] <- apply(ANXTEST[,2:5],2,as.numeric)

ggplot(ANXTEST,aes(x=as.factor(Regions),y=Mean,label=as.factor(Regions)))+
  geom_point()+
  geom_errorbar(aes(x=as.factor(Regions),ymin=lower,ymax=upper))+
  geom_text(size=4,hjust="center", vjust=3,aes(color=as.factor(Regions)))+
  theme(legend.position = "none")+
  ylab("Test Anxiety")+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent",colour = NA),
        axis.line = element_line(colour = "black"))+
  scale_y_continuous(limits = c(-0.5, 0.8), breaks = seq(-0.5,0.8, by = 0.1))


# EMOSUPS
max(results$EMOSUPS);min(results$EMOSUPS)#0.187 and -0.427
lower <- NULL
upper <- NULL
for( i in 1:nrow(results)){
  res <- lower_ci(mean=results[i,5],se=results[i,6],n=results[i,13])
  res_u <- upper_ci(mean=results[i,5],se=results[i,6],n=results[i,13])
  lower <- rbind(lower,res)
  upper <- rbind(upper, res_u)
}

Regions <- c("Anglo","Latin Europe","Nordic","German" ,"East Europe","Latin America ","Middle East","South Asia","East Asia")
EMOSUPS <- as.data.frame(cbind(Regions,results$EMOSUPS,results$EMOSUPS_se,lower,upper))
colnames(EMOSUPS) <- c("Regions","Mean","SE","lower","upper")
EMOSUPS[,2:5] <- apply(EMOSUPS[,2:5],2,as.numeric)

ggplot(EMOSUPS,aes(x=as.factor(Regions),y=Mean,label=as.factor(Regions)))+
  geom_point()+
  geom_errorbar(aes(x=as.factor(Regions),ymin=lower,ymax=upper))+
  geom_text(size=4,hjust="center", vjust=1.5,aes(color=as.factor(Regions)))+
  theme(legend.position = "none")+
  ylab("Emotional Support")+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent",colour = NA),
        axis.line = element_line(colour = "black"))+
  scale_y_continuous(limits = c(-0.5, 0.8), breaks = seq(-0.5,0.8, by = 0.1))


#BELONG
max(results$BELONG);min(results$BELONG)#0.278 and -0.354
lower <- NULL
upper <- NULL
for( i in 1:nrow(results)){
  res <- lower_ci(mean=results[i,7],se=results[i,8],n=results[i,13])
  res_u <- upper_ci(mean=results[i,7],se=results[i,8],n=results[i,13])
  lower <- rbind(lower,res)
  upper <- rbind(upper, res_u)
}

Regions <- c("Anglo","Latin Europe","Nordic","German" ,"East Europe","Latin America ","Middle East","South Asia","East Asia")
BELONG <- as.data.frame(cbind(Regions,results$BELONG,results$BELONG_se,lower,upper))
colnames(BELONG ) <- c("Regions","Mean","SE","lower","upper")
BELONG [,2:5] <- apply(BELONG [,2:5],2,as.numeric)

ggplot(BELONG,aes(x=as.factor(Regions),y=Mean,label=as.factor(Regions)))+
  geom_point()+
  geom_errorbar(aes(x=as.factor(Regions),ymin=lower,ymax=upper))+
  geom_text(size=3.5,hjust="center", vjust=2.5,aes(color=as.factor(Regions)))+
  theme(legend.position = "none")+
  ylab("Belongness ")+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent",colour = NA),
        axis.line = element_line(colour = "black"))+
  scale_y_continuous(limits = c(-0.5, 0.8), breaks = seq(-0.5,0.8, by = 0.1))


#TEACHSUP
max(results$TEACHSUP);min(results$TEACHSUP)#0.391 and -0.346
lower <- NULL
upper <- NULL
for( i in 1:nrow(results)){
  res <- lower_ci(mean=results[i,9],se=results[i,10],n=results[i,13])
  res_u <- upper_ci(mean=results[i,9],se=results[i,10],n=results[i,13])
  lower <- rbind(lower,res)
  upper <- rbind(upper, res_u)
}

Regions <- c("Anglo","Latin Europe","Nordic","German" ,"East Europe","Latin America ","Middle East","South Asia","East Asia")
TEACHSUP <- as.data.frame(cbind(Regions,results$TEACHSUP,results$TEACHSUP_se,lower,upper))
colnames(TEACHSUP) <- c("Regions","Mean","SE","lower","upper")
TEACHSUP[,2:5] <- apply(TEACHSUP[,2:5],2,as.numeric)

ggplot(TEACHSUP,aes(x=as.factor(Regions),y=Mean,label=as.factor(Regions)))+
  geom_point()+
  geom_errorbar(aes(x=as.factor(Regions),ymin=lower,ymax=upper))+
  geom_text(size=4,hjust="center", vjust=2.5,aes(color=as.factor(Regions)))+
  theme(legend.position = "none")+
  ylab("Belongness ")+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent",colour = NA),
        axis.line = element_line(colour = "black"))+
  scale_y_continuous(limits = c(-0.5, 0.8), breaks = seq(-0.5,0.8, by = 0.1))

#ST016Q01NA
max(results$ST016Q01NA);min(results$ST016Q01NA)#7.871 and 6.61
lower <- NULL
upper <- NULL
for( i in 1:nrow(results)){
  res <- lower_ci(mean=results[i,11],se=results[i,12],n=results[i,13])
  res_u <- upper_ci(mean=results[i,11],se=results[i,12],n=results[i,13])
  lower <- rbind(lower,res)
  upper <- rbind(upper, res_u)
}

Regions <- c("Anglo","Latin Europe","Nordic","German" ,"East Europe","Latin America ","Middle East","South Asia","East Asia")
Wellbeing <- as.data.frame(cbind(Regions,results$ST016Q01NA,results$ST016Q01NA_se,lower,upper))
colnames(Wellbeing) <- c("Regions","Mean","SE","lower","upper")
Wellbeing[,2:5] <- apply(Wellbeing[,2:5],2,as.numeric)

ggplot(Wellbeing,aes(x=as.factor(Regions),y=Mean,label=as.factor(Regions)))+
  geom_point()+
  geom_errorbar(aes(x=as.factor(Regions),ymin=lower,ymax=upper))+
  geom_text(size=4,hjust="center", vjust=3,aes(color=as.factor(Regions)))+
  theme(legend.position = "none")+
  ylab("Subjective Well-being")+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent",colour = NA),
        axis.line = element_line(colour = "black"))+
  scale_y_continuous(limits = c(6, 8), breaks = seq(6,8, by = 1))


# 5. Plots between social support and well-being --------------------------
culture <- list(Anglo,Latin_Eu,Nordic,German ,East_EU,Latin_A,Middle_E,South_A,East_A)
ggplot(data=German,aes(x=as.numeric(EMOSUPS),y=as.numeric(BELONG)))+geom_point()


# 4. Function -------------------------------------------------------------

PISA <- function(Data,folder,file){
  
  Demo <- c("ST004D01T","IMMIG","ESCS")
  Student <- c("MOTIVAT","ANXTEST","EMOSUPS","BELONG","TEACHSUP","PV1SCIE","PV2SCIE","PV3SCIE",
               "PV4SCIE","PV5SCIE","PV6SCIE","PV7SCIE","PV8SCIE","PV9SCIE","PV10SCIE")
  Well_being <- c("ST016Q01NA")
  weight <- c("W_FSTUWT","SENWT")
  
  
  Data <- Data%>%
    select(Demo,Student,Well_being,weight)%>%mutate_at("IMMIG",as.factor)%>%mutate(ST004D01T=recode(ST004D01T,'1'="1",
                                                                                                    '2'="0"))
  #Male is a reference group
  Dummy <- to_dummy(Data$IMMIG)%>%select("IMMIG_2","IMMIG_3")%>%mutate_all(as.numeric)
  #Native is a reference group 
  FData <- cbind(Data,Dummy)
  FData <- FData%>%mutate_at(c("IMMIG","IMMIG_2","IMMIG_3"),as.numeric)
  FData <- FData%>%mutate_all(~replace(., is.na(.), 9999))
  colnames(FData) <- c("ST004D01T","IMMIG","ESCS","MOTIVAT","ANXTEST","EMOSUPS","BELONG",
                       "TEACHSUP","PVSCIE1","PVSCIE2","PVSCIE3",
                       "PVSCIE4","PVSCIE5","PVSCIE6","PVSCIE7","PVSCIE8","PVSCIE9","PVSCIE10",
                       "ST016Q01NA","W_FSTUWT","SENWT","IMMIG_2","IMMIG_3")
  
  PV1<- FData%>%select("ST004D01T","IMMIG","ESCS","MOTIVAT","ANXTEST","EMOSUPS","BELONG","TEACHSUP","PVSCIE1",
                       "ST016Q01NA","SENWT","IMMIG_2","IMMIG_3")
  PV2<- FData%>%select("ST004D01T","IMMIG","ESCS","MOTIVAT","ANXTEST","EMOSUPS","BELONG","TEACHSUP","PVSCIE2",
                       "ST016Q01NA","SENWT","IMMIG_2","IMMIG_3")
  PV3<- FData%>%select("ST004D01T","IMMIG","ESCS","MOTIVAT","ANXTEST","EMOSUPS","BELONG","TEACHSUP","PVSCIE3",
                       "ST016Q01NA","SENWT","IMMIG_2","IMMIG_3")
  PV4<- FData%>%select("ST004D01T","IMMIG","ESCS","MOTIVAT","ANXTEST","EMOSUPS","BELONG","TEACHSUP","PVSCIE4",
                       "ST016Q01NA","SENWT","IMMIG_2","IMMIG_3")
  PV5<- FData%>%select("ST004D01T","IMMIG","ESCS","MOTIVAT","ANXTEST","EMOSUPS","BELONG","TEACHSUP","PVSCIE5",
                       "ST016Q01NA","SENWT","IMMIG_2","IMMIG_3")
  PV6<- FData%>%select("ST004D01T","IMMIG","ESCS","MOTIVAT","ANXTEST","EMOSUPS","BELONG","TEACHSUP","PVSCIE6",
                       "ST016Q01NA","SENWT","IMMIG_2","IMMIG_3")
  PV7<- FData%>%select("ST004D01T","IMMIG","ESCS","MOTIVAT","ANXTEST","EMOSUPS","BELONG","TEACHSUP","PVSCIE7",
                       "ST016Q01NA","SENWT","IMMIG_2","IMMIG_3")
  PV8<- FData%>%select("ST004D01T","IMMIG","ESCS","MOTIVAT","ANXTEST","EMOSUPS","BELONG","TEACHSUP","PVSCIE8",
                       "ST016Q01NA","SENWT","IMMIG_2","IMMIG_3")
  PV9<- FData%>%select("ST004D01T","IMMIG","ESCS","MOTIVAT","ANXTEST","EMOSUPS","BELONG","TEACHSUP","PVSCIE9",
                       "ST016Q01NA","SENWT","IMMIG_2","IMMIG_3")
  PV10 <- FData%>%select("ST004D01T","IMMIG","ESCS","MOTIVAT","ANXTEST","EMOSUPS","BELONG","TEACHSUP","PVSCIE10",
                         "ST016Q01NA","SENWT","IMMIG_2","IMMIG_3")
  dataname <- list(PV1,PV2,PV3,PV4,PV5,PV6,PV7,PV8,PV9,PV10)
  names(dataname) <- c("PV1","PV2","PV3","PV4","PV5","PV6","PV7","PV8","PV9","PV10")
  dir.create(paste0("~\\PISA 2015\\Analysis\\",folder))
  for(i in 1:length(dataname)){
    write.table(dataname[i], file = paste0("~\\PISA 2015\\Analysis\\",file,"\\",names(dataname[i]), ".txt"),  sep = "\t",row.names = FALSE,col.names = FALSE,quote = FALSE)
  }
}
Anglo_Data<- PISA(Data =Latin_Eu,folder = "Anglo",file = "Anglo")
Latin_Eu_Data_revised <- PISA(Data =Latin_Eu,folder = "Latin_Europe_Revised",file = "Latin_Europe_Revised")
Nordic_Data <- PISA(Data =Nordic,folder = "Nordic",file = "Nordic")
German_Data <- PISA(Data =German,folder = "German",file = "German")
East_EU_Data <- PISA(Data =East_EU,folder = "Eastern_Europe_Revised",file = "Eastern_Europe_Revised")
Latin_A_Data <- PISA(Data =Latin_A,folder = "Latin_America_Revised",file = "Latin_America_Revised")
Middle_E_Data <- PISA(Data =Middle_E,folder = "Middle_East_Revised",file = "Middle_East_Revised")
South_A_Data <- PISA(Data =South_A,folder = "Southern_Asia",file = "Southern_Asia")
East_A_Data <- PISA(Data =East_A,folder = "Confucian_Asia",file = "Confucian_Asia")
