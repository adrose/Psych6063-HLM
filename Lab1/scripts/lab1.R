### Load library(s)
library(lmerTest)
library(nlme)


### Read data
in.data <- read.table("./Lab1/data/HSBALL.DAT")
colnames(in.data) <- c("schID", 
                       "minority", 
                       "female", 
                       "ses", 
                       "mathac",
                       "size", 
                       "sector", 
                       "pracad", 
                       "disdim", 
                       "himinty", 
                       "meanse")


## Now look at the data


## train the nlme models
model_0 <- lme(fixed = mathac ~ 1, random=~1|schID, data=in.data)

## Now scale the data
in.data.scale <- data.frame(scale(in.data)[,])

## Now train a model
mod.0 <- lmerTest::lmer(mathac ~ 1 + (1|schID), data=in.data, REML=T)
mod.one <- lmerTest::lmer(mathac ~ female + ses + pracad + (1|schID), data=in.data.scale, REML = T)
mod.two <- lmerTest::lmer(mathac ~ sector*schID + (1|schID), data=in.data)
