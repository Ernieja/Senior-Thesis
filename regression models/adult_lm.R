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

## regression model for adult data 


setwd("C:/Users/Irene/SkyDrive/Documents/thesis/Completed/R/data")
afterlife.data <- read.csv("DataFile_20151010.reformatted.csv")


## filter for adults

afterlife.adultdata <- filter(afterlife.data, Adult == 1)


## remove irrelevant variable from adultdata

afterlife.adultdata <- afterlife.adultdata[,-c(7)]

## convert all necessary variables to factor types 

afterlife.adultdata[,c(6,17,26,27,28,30)] <- colwise(as.factor)(afterlife.adultdata[,c(6,17,26,27,28,30)])

 
## only keep variables wanted in model

afterlife.adultdata <- afterlife.adultdata[,c(6,7,8,15,17,19,20,21,23,24,25,26,27,28,30)]


## check for high correlations

ggplot(afterlife.adultdata, aes(x=RB_Summ, y = RF_Summ)) + geom_point()
cor(afterlife.adultdata$RB_Summ, afterlife.adultdata$RF_Summ) #.691 correlation for religious behavior and fundamentalism
afterlife.adultdata <- afterlife.adultdata[,-c(6)] #remove religious behavior

ggplot(afterlife.adultdata, aes(x=RC_Summ, y = RF_Summ)) + geom_point()
cor(afterlife.adultdata$RC_Summ, afterlife.adultdata$RF_Summ) #.604 correlation for fundamentalism and commitment
afterlife.adultdata <- afterlife.adultdata[,-c(10)] #remove religious commitment

ggplot(afterlife.adultdata, aes(x=Intrinsic_Summ, y = RF_Summ)) + geom_point()
cor(afterlife.adultdata$Intrinsic_Summ, afterlife.adultdata$RF_Summ) #.633 correlation for fundamentalism and intrinsic behavior
afterlife.adultdata <- afterlife.adultdata[,-c(7)] #remove intrinsic


## not enough data to model, so I combined select dummy variables into broader specifics

#Political ideology
afterlife.adultdata <- afterlife.adultdata %>%
  mutate(PoliticalBelief = ifelse(PI_1 == 1 | PI_1 == 2 | PI_1 == 3, "liberal",
                                  ifelse(PI_1 == 4, "moderate", "conservative")))
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


## create linear model

#model 1 (no interactions)
overall.adult.model1 <- lm(Overall_Summ ~ Prime + RF_Summ + Es_Summ +
                             Ep_Summ + PoliticalBelief + Income_Macro,
                           data = afterlife.adultdata)

summary(overall.adult.model1)

#model 2 (with interactions)
overall.adult.model2 <- lm(Overall_Summ ~ Prime + RF_Summ + Es_Summ +
                              Ep_Summ + PoliticalBelief  + Income_Macro +
                              Prime*RF_Summ + Prime*Es_Summ + Prime*Ep_Summ +
                              Prime*PoliticalBelief  + Prime*Income_Macro,
                            data = afterlife.adultdata)
summary(overall.adult.model2) #much better model
lm.beta(overall.adult.model2)

lm.table <- xtable(summary(overall.adult.model2))
print.xtable.booktabs(lm.table)

