setwd('/Users/lvg/Documents/Projects/Nearby')
library(data.table)
library(ggplot2)
install.packages("combinat")
library(combinat)

DT = fread('data.csv')
names(DT) = c("UserId_A", "UserId_B", "Distance", "MessageCount")
setkeyv(DT, c('UserId_A', 'UserId_B'))
head(DT)
finalDT[UserId_A == 35311]
# Creata a Dataframe of Messages between pairs of users
DT2 = merge(DT, DT, all=TRUE, suffixes = c(".AtoB", ".BtoA"),
            by.x=c('UserId_A', 'UserId_B'),
            by.y=c('UserId_B', 'UserId_A'))
head(DT2)
subset(DT, UserId_A==35311)
subset(DT, UserId_B==35311)
subset(DT2, (UserId_A==35311)|(UserId_B==35311))


# Create subset, direct: A to B
dtsub1 = DT[, sum(MessageCount), by = c("UserId_A", "UserId_B")]

# Rename columns to create directed: B to A

tmp = copy(DT)
setnames(tmp, c("UserId_A", "UserId_B"), c("UserId_B", "UserId_A"))
tmp[UserId_B == 35311]

tmp1 = copy(DT)
tmp2 = copy(DT)
setkey(tmp1, UserId_A)
setkey(tmp2, UserId_B)

tmp = tmp2[tmp1, nomatch = 0]

dtsub2 = DT[, sum(MessageCount), by = c("UserId_A", "UserId_B")]

# Stack both subsets on top of each other, then aggregate
fulldt = rbind(dtsub1, dtsub2)
fulldt = fulldt[, sum(V1), by = c("UserId_A", "UserId_B")]
setkey(fulldt, UserId_B, UserId_A)
setkey(DT, UserId_A, UserId_B)

fulldt[UserId_A == 35311]
fulldt[UserId_B == 35311]

# Merge with original Data.Table to get Distance column
finalDT = fulldt[DT]
finalDT[, MessageCount := NULL]
setnames(finalDT, "V1", "MessageCount")
finalDT[UserId_A==35311 & UserId_B==262498]

# Check for unique distances
setkey(finalDT, UserId_A)
uniqueCheck <- unique(finalDT, by = c("UserId_A", "UserId_B", "Distance"))
nrow(finalDT) - nrow(uniqueCheck)

finalDT


#########################

subset(DT, UserId_A==35311)
subset(DT, UserId_B==35311)

library(igraph)
graph = graph.data.frame(DT, directed=TRUE)
ugraph = as.undirected(graph, mode = c("collapse"), edge.attr.comb = list(MessageCount="sum", Distance="min"))
uDT = get.data.frame(ugraph)
setDT(uDT)


subset(uDT, from==35311)
subset(uDT, to==35311)

subset(finalDT, UserId_A==35311)
subset(finalDT, UserId_B==35311)




subset(dtsub1, UserId_A==35311)
subset(dtsub1, UserId_B==35311)
subset(dtsub2, UserId_A==35311)
subset(dtsub2, UserId_B==35311)

# Basic Exploration of Distance/Count relationship
cor(DT$MessageCount, DT$Distance) #[1] 0.00664321


# Need to control for user averages -- discount A->B message count by average A->? message count 
model_lm_01 <- lm(MessageCount~Distance, DT)
summary(model_lm_01)
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 5.395e+00  2.392e-02  225.52   <2e-16 ***
#   Distance    1.259e-04  6.835e-06   18.42   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Residual standard error: 49.49 on 7687369 degrees of freedom
# Multiple R-squared:  4.413e-05,	Adjusted R-squared:  4.4e-05 
# F-statistic: 339.3 on 1 and 7687369 DF,  p-value: < 2.2e-16

# Sample plots

ns <- sample(dim(DT)[1],10000)

hist(model_lm_01$residuals[ns], main="Histogram of Residuals", xlab = "bf residuals")
# non- normal shaped histogram of residuals.. long tail cluster at 0

plot(DT$Distance[ns], model_lm_01$residuals[ns])
#  departures from constant variance & linearity.

# Log trasnformation?
hist(DT$MessageCount[ns])
hist(DT$Distance[ns])

hist(log(DT$MessageCount[ns]))
hist(log(DT$Distance[ns]))

plot(DT$MessageCount[ns], DT$Distance[ns])
plot(log(DT$MessageCount[ns]), DT$Distance[ns])
plot(log(DT$MessageCount[ns]), log(DT$Distance[ns]))
plot((DT$MessageCount[ns]), log(DT$Distance[ns]))


model_lm_01 <- lm(MessageCount~Distance, DT)
summary(model_lm_01)
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 5.395e+00  2.392e-02  225.52   <2e-16 ***
#   Distance    1.259e-04  6.835e-06   18.42   <2e-16 ***
# Multiple R-squared:  4.413e-05,	Adjusted R-squared:  4.4e-05 


# Removing outliers in Message Count
lapply(DT,function(x) quantile(x, probs=seq(.9,1,.01)))
DT_outmess = subset(DT, DT$MessageCount<39)


hist(DT_outmess$MessageCount[ns]) # poison distribution.. therefore poison regression (log-linear)
hist(DT_outmess$Distance[ns])
plot(DT_outmess$MessageCount[ns], DT_outmess$Distance[ns])


hist(log(DT_outmess$MessageCount[ns]))
hist(log(DT_outmess$Distance[ns]))
plot(log(DT_outmess$MessageCount[ns]), DT_outmess$Distance[ns])
plot((DT_outmess$MessageCount[ns]), log(DT_outmess$Distance[ns]))
plot(log(DT$MessageCount[ns]), log(DT$Distance[ns]))


model_glm_01 <- glm(MessageCount~Distance, data=DT, family="poisson")
summary(model_glm_01)
hist(model_glm_01$residuals[ns])
# Deviance residuals are approximately normally distributed if the model is specified correctly.In our example, it shows a little bit of skeweness since median is not quite zero. 