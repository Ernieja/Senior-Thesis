library(plyr)
library(dplyr)
library(stats)
library(ggplot2)
library(knitr)
library(xtable)


## @knitr get_data

afterlife.data <- read.csv("DataFile_20151010.reformatted.csv")

## @knitr prepare_data 

#filter for adults
afterlife.adultdata <- filter(afterlife.data, Adult == 1)

colnames(afterlife.adultdata)

#remove irrelevant variable from adultdata
afterlife.adultdata <- afterlife.adultdata[,-c(7)]

#convert all necessary variables to factor types 
afterlife.adultdata[,c(6,17,26,27,28,30)] <- colwise(as.factor)(afterlife.adultdata[,c(6,17,26,27,28,30)])

#only keep variables wanted in model
afterlife.adultdata <- afterlife.adultdata[,c(6,15,17,19,20,21,23,24,25,26,28,30)]



## @knitr corr
## check for high correlations


#uncomment to print correlation matrix
#cor(afterlife.adultdata[sapply(afterlife.adultdata, is.numeric)])


ggplot(afterlife.adultdata, aes(x=ReligiousBehavior_Score, y = ReligiousFund_Score)) + geom_point()
cor(afterlife.adultdata$ReligiousBehavior_Score, afterlife.adultdata$ReligiousFund_Score) #.691 correlation for religious behavior and fundamentalism
afterlife.adultdata <- afterlife.adultdata[,-c(4)] #remove religious behavior

ggplot(afterlife.adultdata, aes(x=ReligiousCommit_Score, y = ReligiousFund_Score)) + geom_point()
cor(afterlife.adultdata$ReligiousCommit_Score, afterlife.adultdata$ReligiousFund_Score) #.604 correlation for fundamentalism and commitment
afterlife.adultdata <- afterlife.adultdata[,-c(8)] #remove religious commitment



## not enough data to model, so I combined select dummy variables into broader specifics
## @knitr mutate_var


#Political ideology
afterlife.adultdata <- afterlife.adultdata %>%
  mutate(PoliticalBelief = ifelse(PoliticalID == 1 | PoliticalID == 2 | PoliticalID == 3, "liberal",
                                  ifelse(PoliticalID == 4, "moderate", "conservative")))
afterlife.adultdata$PoliticalBelief = as.factor(afterlife.adultdata$PoliticalBelief)
#Education
afterlife.adultdata <- afterlife.adultdata %>%
  mutate(Education_Macro = ifelse(Education == 5 | Education == 6, "College",
                                  "High school or below"))
afterlife.adultdata$Education_Macro <- as.factor(afterlife.adultdata$Education_Macro)
#Income
afterlife.adultdata <- afterlife.adultdata %>%
  mutate(Income_Macro = ifelse(Income == 1 | Income == 2, "40,000 or less",
                               ifelse(Income == 3 | Income == 4, "40,001 to 90,000","90,000 or more")))
afterlife.adultdata$Income_Macro <- as.factor(afterlife.adultdata$Income_Macro)



## @knitr lm
## create linear models

#model 1 (no interactions)
overall.adult.model1 <- lm(Belief_Score ~ Prime + ReligiousFund_Score + ExtrinsicSocial_Score +
                             ExtrinsicPersonal_Score + PoliticalBelief + Income_Macro, data = afterlife.adultdata)


summary(overall.adult.model1)

#model 2 (with interactions)
overall.adult.model2 <- lm(Belief_Score ~ Prime + ReligiousFund_Score + Intrinsic_Score + 
                             ExtrinsicSocial_Score + ExtrinsicPersonal_Score + PoliticalBelief +
                             Income_Macro + Prime*ReligiousFund_Score + Prime*Intrinsic_Score + 
                             Prime*ExtrinsicSocial_Score + Prime*ExtrinsicPersonal_Score + 
                             Prime*PoliticalBelief  + Prime*Income_Macro, data = afterlife.adultdata)

summary(overall.adult.model2) 



## @knitr xtable
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

## prepare the adult data 
## @knitr print

lm.table <- xtable(summary(overall.adult.model2))
print.xtable.booktabs(lm.table)

