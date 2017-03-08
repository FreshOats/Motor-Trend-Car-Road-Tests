library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(PerformanceAnalytics)

data("mtcars")
sum(is.na(mtcars))

# Exploratory analysis
chart.Correlation(mtcars, histogram=TRUE, pch=19)

mdata <- mtcars %>% select(mpg,cyl,disp,hp,drat,wt,vs,am)

mdata$cyl <- factor(mdata$cyl)
mdata$vs <- factor(mdata$vs)
mdata$am = factor(mdata$am, labels = c("Automatic","Manual"))

# Summary the selection
head(mdata)
str(mdata)

# Statistical inference

t.test(mpg~am, paired = FALSE, var.equal = FALSE, mdata)

# Regression linear

mpg.lm.global <- lm(mpg ~ ., mdata)
mpg.lm.selection <- step(mpg.lm.global, direction = "both")
mpg.lm.am <- lm(mpg ~ am, mdata)

summary(mpg.lm.selection)
anova(mpg.lm.selection,mpg.lm.am)

