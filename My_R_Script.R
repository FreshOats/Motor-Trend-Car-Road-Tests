library(dplyr)
library(PerformanceAnalytics)


# Processing Data

data("mtcars")
sum(is.na(mtcars))

head(mtcars)
str(mtcars)

# Exploratory analysis
chart.Correlation(mtcars, histogram=TRUE, pch=19)
dev.off()

mdata <- mtcars %>% select(mpg,cyl,disp,hp,drat,wt,vs,am)
mdata$cyl <- factor(mdata$cyl)
mdata$vs <- factor(mdata$vs)
mdata$am = factor(mdata$am, labels = c("Automatic","Manual"))

# Statistical inference

t.test(mpg~am, paired = FALSE, var.equal = FALSE, mdata)

# Regression linear

mpg.lm.am <- lm(mpg ~ am, mdata)
round(sigma(mpg.lm.am),2)
round(summary(mpg.lm.am)$adj.r.squared,3)

mpg.lm.global <- lm(mpg ~ ., mdata)
round(sigma(mpg.lm.global),2)
round(summary(mpg.lm.global)$adj.r.squared,3)

mpg.lm.selection <- step(mpg.lm.global, direction = "both", trace=FALSE)
round(sigma(mpg.lm.selection),2)
round(summary(mpg.lm.selection)$adj.r.squared,3)

anova(mpg.lm.selection,mpg.lm.am)

