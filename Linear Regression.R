rm(list = ls(all= TRUE))

setwd("C:/users/pc/Desktop/Regression")

library(car) # for avplots
library(tidyverse) 
library(psych)
install.packages("glmnet")
library(glmnet)


nyc_census <- read.csv("C:/users/pc/Desktop/nyc_census_tracts.csv")

nyc <- nyc_census %>%
  na.omit()
demographics <- nyc %>%
  select(County,Borough,TotalPop, Hispanic, Asian, Poverty, ChildPoverty, Unemployment)

str(demographics)
pairs.panels(demographics)

model <- glm(Poverty ~ ChildPoverty + Unemployment, data = demographics, family = "gaussian")
par(mfrow=c(2,2))

plot(model)

avPlots(model)

input <- demographics %>%
  select(-Poverty, -County, -Borough) %>%
  as.matrix()

output <- demographics$Poverty

cv_fit <- cv.glmnet(input, output, family = "gaussian")
coef(cv_fit, s = "lambda.min")

summary(model)

m <- glm(Poverty ~ ChildPoverty + Unemployment, data = demographics, family = "gaussian")
Povresid <- resid(m)

mod <- plot(demographics$ChildPoverty, Povresid)
abline(0,0)

mod <- plot(demographics$Unemployment, Povresid)
abline(0,0)

summary(m)
plot(m)
avPlots(m)
levels(demographics$County)

pov <- ggplot(demographics, aes(x = ChildPoverty + Unemployment, y = Poverty)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "gaussian")) +
  geom_jitter() +
  facet_grid(County ~ .) +
  labs(title = "Effect of Child Poverty and Unemployment on overall Poverty",
       x = "Child Poverty and unemployment rate",
       y = "Overall percent of people below poverty line in a County") +
  theme(
    strip.background = element_blank(), # remove strip from top of facets
    strip.text.x = element_blank())
pov
