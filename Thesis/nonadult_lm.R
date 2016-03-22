library(plyr)
library(dplyr)
library(stats)
library(ggplot2)
library(knitr)
library(xtable)


## prepare the nonadult data


afterlife.data <- read.csv("DataFile_20151010.reformatted.csv")

#filter for non-adults
afterlife.childdata <- filter(afterlife.data, Adult == 0)

#remove variables irrelevant to nonadults
afterlife.childdata <- afterlife.childdata[,c(3,6,7,8,9,16,17,20,27,29,31)]

#convert all necessary variables to factor types
afterlife.childdata[,c(1,2,7,9,10,11)] <- colwise(as.factor)(afterlife.childdata[,c(1,2,7,9,10,11)])


## not enough data to model, so I combined select dummy variables into broader specifics

#religion
afterlife.childdata <- afterlife.childdata %>%
  mutate(Religiosity = ifelse(Religion == 1 | Religion == 2, 
                              "Non-religious", 
                              ifelse(Religion == 4 | Religion == 8, "Christian", "Other")))
afterlife.childdata$Religiosity = as.factor(afterlife.childdata$Religiosity)
#political ideology
afterlife.childdata <- afterlife.childdata %>%
  mutate(PoliticalBelief = ifelse(PI_1 == 1 | PI_1 == 2 | PI_1 == 3, "liberal",
                                  ifelse(PI_1 == 5 | PI_1 == 6 | PI_1 == 7, "conservative", "moderate")))
afterlife.childdata$PoliticalBelief = as.factor(afterlife.childdata$PoliticalBelief)
#parent education
afterlife.childdata <- afterlife.childdata %>%
  mutate(Education_Macro = ifelse(Education == 1 | Education == 2 | Education == 3 | Education == 4, 
                                  "high school or below",
                                  ifelse(Education == 5 | Education == 6, "college",
                                         "graduate degree")))
afterlife.childdata$Education_Macro <- as.factor(afterlife.childdata$Education_Macro)
#parent income
afterlife.childdata <- afterlife.childdata %>%
  mutate(Income_Macro = ifelse(Income == 1 | Income == 2, "40,000 or less",
                               ifelse(Income == 3 | Income == 4, "40,001 to 90,000",
                                      "90,001 or more")))
afterlife.childdata$Income_Macro <- as.factor(afterlife.childdata$Income_Macro)


## remove nulls
afterlife.childdata <- afterlife.childdata[!is.na(afterlife.childdata$Education_Macro),]


## create linear regression model

#model 1 (no interactions) 
overall.child.model1 <- lm(Overall_Summ ~ Prime + HDT + RB_Summ + PoliticalBelief + Education_Macro*HDT + Education_Macro + Income_Macro,
                            data = afterlife.childdata)

#model 2 (with interactions)
overall.child.model2 <- lm(Overall_Summ ~ Prime + HDT + RB_Summ +
                              PoliticalBelief + Education_Macro +
                              Income_Macro + HDT*Education_Macro +
                              Prime*HDT + Prime*RB_Summ +
                              Prime*PoliticalBelief + 
                              Prime*Education_Macro + Prime*Income_Macro,
                            data = afterlife.childdata)

summary(overall.child.model1) #better model
summary(overall.child.model2)



## function for printing a pretty table in latex later

print.xtable.booktabs <- function(x){
  
  print(xtable(x),
        floating=F,
        hline.after=NULL,
        add.to.row=list(pos=list(-1,0, nrow(x)),
                        command=c(
                          '\\toprule\n',
                          '\\midrule\n',
                          '\\bottomrule\n')))
  
}

child.lm.table <- xtable(summary(overall.child.model1))
print.xtable.booktabs(child.lm.table)

