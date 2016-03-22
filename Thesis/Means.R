library(RCurl)
library(dplyr)
library(ggplot2)
library(reshape2)
library(grid)
library(tidyr)
library(plyr)
library(ggthemes)

afterlife.data <- read.csv("DataFile_20151010.reformatted.csv")


## I want to compare overall belief scores, grouped by condition and age group


belief.data <- afterlife.data %>% select(Part, Prime, Adult, Belief_Score)

#call ddply() to organize summarized data and corresponding means, SD, and SE
cdata1 <- ddply(belief.data, c("Prime", "Adult"), summarise,
               N = sum(!is.na(Belief_Score)),
               mean = mean(Belief_Score, na.rm = TRUE),
               sd = sd(Belief_Score, na.rm = TRUE),
               se = sd / sqrt(N)
)


## handle the two variables

cdata1$Prime <- factor(cdata1$Prime)
cdata1$Adult <- factor(cdata1$Adult)

#create appropriate labels for age groups 
labels <- list('0' = "Non Adult", '1' = "Adult")
age_lab <- function(variable,value){
  return(labels[value])
}
#designate conditions 
prime_lab <- c("1", "2")


## create bar chart comparing overall belief scores between primes for both age groups

ggplot(cdata1, aes(x=Prime, y=mean, fill=Prime)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
                width=.2,
                position = position_dodge(.9)
  ) +
  xlab("Prime") +
  ylab("Afterlife Belief Score")+
  facet_grid(.~Adult, labeller=age_lab)+
  scale_x_discrete(limits=prime_lab, labels=c("Natural", "Supernatural"))+
  theme_pander()+
  theme(plot.title = element_text(size=22, face="bold", vjust=1),
        axis.title.x=element_text(size=22, face="bold", vjust=0),
        axis.title.y=element_text(size=22, face="bold", vjust=1),
        axis.text.x=element_text(size = 20, vjust = 1),
        axis.text.y=element_text(size = 20),
        legend.position="none",
        strip.text.x=element_text(size=22, face = "bold", vjust = .1)
  )
  
                
                 
                  