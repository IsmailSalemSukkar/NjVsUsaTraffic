library(tidyverse)
library(glmmTMB)
library(DHARMa)
library(RMariaDB)
library(emmeans)
library(MuMIn)
library(performance)
library(car)
library(tibble)
library(dplyr)
options(scipen = 999)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

con <- dbConnect(MariaDB(), dbname = "Datasets",host="192.168.0.100",password="1234")

dbListTables(con)

dbRequest <- dbSendQuery(con,"SELECT Severity, State FROM trafficData")
trafData <- dbFetch(dbRequest) 
trafData <- trafData[-1,]





dbClearResult(dbRequest)
dbDisconnect(con)

#To keep the sample consistant, the seed will be set
set.seed(2)
trafDataSample <- trafData %>% group_by(State) %>% slice_sample(prop=.10)

#Clear that the kruskall wallis test will be necassary

trafDataSample$Severity <- as.factor(trafDataSample$Severity)

kruskal.test(Severity~State,data=trafDataSample)
#So state is a significant effect!

library(rcompanion)
epsilonSquared(trafDataSample$Severity, trafDataSample$State)
#Appears that state is a "medium level" effect size


library(pgirmess)
datMC <- as.data.frame(kruskalmc(Severity~State,data=trafDataSample)) %>% 
  rownames_to_column(var="contrast") 
#To determine what states are seperate from each other, a post hoc test 
#was used. While normally a dunnTest should be used, comparing an ordinal
#with a nominal requires kruskalmc


datMC <- datMC %>%
  filter(grepl("NJ",contrast))

#Lets filter out NJ only

datMC$contrast <- gsub('-'," / ", datMC$contrast)



##############

trafDataSample$Severity <- as.integer(trafDataSample$Severity)



fitPois <- glmmTMB(data = trafDataSample,Severity~State,family = "poisson")

#check_overdispersion(fitPois)

#plot(simulateResiduals(fitPois))

#Anova(fitPois)
#summary(fitPois)
emm1<-(emmeans(fitPois,~State,type = "response"))
means<-as.data.frame(emm1)
diff3<-as.data.frame(pairs(emm1,adjust="tukey"))

#NJ

diff4 <- diff3 %>% filter(grepl("NJ",contrast))

diff4[1:29,2] <- 1/diff4[1:29,2]

diff4$safety <- -(1-diff4$ratio)

#############Binomial attempt##########

dat3<-trafDataSample

dat3$Severity[dat3$Severity==1] <- "FALSE"
dat3$Severity[dat3$Severity==2] <- "FALSE"
dat3$Severity[dat3$Severity==3] <- "TRUE"
dat3$Severity[dat3$Severity==4] <- "TRUE"

dat3$Severity<-(as.factor(dat3$Severity))
summary(dat3$Severity)

fitBinom <- glmmTMB(data = dat3,Severity~State,family="binomial")
#plot(simulateResiduals(fitPois))


emm2<-(emmeans(fitBinom,~State,type = "response"))
means2<-as.data.frame(emm2)
diff5<-as.data.frame(pairs(emm2,adjust="tukey"))

diff5 <- diff5 %>% filter(grepl("NJ",contrast))

diff5[1:29,2] <- 1/ diff5[1:29,2]

diff5$percentmore <- -1*(1-diff5$odds.ratio)







FinalTable <- full_join(datMC,diff4,by="contrast") %>% full_join(diff5,by="contrast")

FinalTable <- FinalTable[FinalTable$dif.com.difference == TRUE,]
FinalTable <- FinalTable[c(1,12,18)]

finalTableDraft <- separate(FinalTable, contrast,c("State1","State2"),sep = " / ")


finalTableDraft1 <- left_join(x=finalTableDraft[finalTableDraft$State1!="NJ",],
                             y=means2,
                             by=c("State1"="State")) %>%
                      left_join(
                                y=means,
                                by=c("State1"="State"))
  

finalTableDraft2 <- left_join(x=finalTableDraft[finalTableDraft$State1=="NJ",],
                              y=means2,
                              by=c("State2"="State"))%>%
                    left_join(
                              y=means,
                              by=c("State2"="State"))

finalTableDraft2 <- relocate(finalTableDraft2, State2,.before=State1)

finalTableDraft2 <- rename(finalTableDraft2, State2 = State1, State1=State2)

finalTableDraft <- rbind(finalTableDraft1,finalTableDraft2) 

finalTableDraft <- finalTableDraft[,c(1:5,10)]

ttest<-t.test(trafDataSample$Severity,trafDataSample$Severity[trafDataSample$State=="NJ"])

ratioUSA <- as.data.frame(summary(dat3$Severity))

ratioNJ <- as.data.frame(summary(dat3$Severity[dat3$State=="NJ"]))


usaRow <- c("USA","NJ",-(1-((ttest[["estimate"]][["mean of x"]])/
                              (ttest[["estimate"]][["mean of y"]]))),
            (ratioUSA[2,]/ratioUSA[1,])/(ratioNJ[2,]/ratioNJ[1,]),(ratioUSA[2,]/ratioUSA[1,]),ttest[["estimate"]][["mean of x"]])


njRow <- c("NJ","NJ",NA,NA,(ratioNJ[2,]/ratioNJ[1,]),ttest[["estimate"]][["mean of y"]])

finalTable <- rbind(finalTableDraft,usaRow,njRow)

finalTable <- rename(finalTable, Prob_of_Severe_Crash_State1 = prob , 
                     Avg_Accident_Rating_State1 =rate, 
                     Percent_More_Likely_For_Severe_NJ = percentmore,
                     Percent_Diff_In_Accident_Rating_in_NJ = safety)


finalTable<- finalTable[c(1,2,6,3,5,4)]

write.csv(finalTable, "results.csv")
