setwd('/Users/lvg/Documents/Projects/Nearby')
library(data.table)
library(ggplot2)
library(igraph)
library(ggthemes)

DT = fread('data.csv')
names(DT) = c("UserId_A", "UserId_B", "Distance", "MessageCount")
setkeyv(DT, c('UserId_A', 'UserId_B'))
head(DT)
finalDT[UserId_A == 35311]
# Creata a Dataframe of Messages between pairs of users

#########################

subset(DT, UserId_A==35311)
subset(DT, UserId_B==35311)

graph = graph.data.frame(DT, directed=TRUE)
ugraph = as.undirected(graph, mode = c("collapse"), edge.attr.comb = list(MessageCount="sum", Distance="min"))
uDT = get.data.frame(ugraph)
setDT(uDT)


subset(uDT, from==35311)
subset(uDT, to==35311)


ns <- sample(dim(uDT)[1],10000)


#####
# recreate brian's plot

uDT_bh1 = uDT[, list(sum(MessageCount), length(MessageCount)), by=Distance]
names(uDT_bh1) <- c("Distance", "sumMessagesSent",       "numTransactions" )

ggplot(subset(uDT_bh1, uDT_bh1$sumMessagesSent<20000)) + 
  geom_point(aes(x=Distance, y=sumMessagesSent, size=numTransactions), 
             alpha=I(.1)) + 
  theme_few() + scale_colour_few()+
  labs(title = "MessageSent Aggragted by Distance",
       y="Total Number of Messages Sent",
       x="Distance Between Conversation Participants") 

ggsave("nearby_messaggdist.jpeg")

########





ggplot() + 
  geom_point(aes(x=uDT[ns]$Distance, y=uDT[ns]$MessageCount), 
             color=("blue"), alpha=I(.5)) + 
  theme_few() + scale_colour_few()+
  labs(title = "10K sample scatterplot,\n Original Data - Directed", 
       x = "Distance",  y = "MessageSent")

ggplot() + 
  geom_point(aes(x=uDT[ns]$Distance, y=log(uDT[ns]$MessageCount)), 
             color=("blue"), alpha=I(.5)) + 
  theme_few() + scale_colour_few()+
  labs(title = "10K sample scatterplot,\n Original Data - Directed", 
       x = "Distance",  y = "ln(MessageSent)")

ggplot() + 
  geom_histogram(aes(x=uDT[ns]$MessageCount), 
             fill=("blue"), alpha=I(.5)) + 
  theme_few() + scale_colour_few()+
  labs(title = "10K sample Histogram,\n Original Data - Directed", 
       x = "MessageSent")


# Basic Exploration of Distance/Count relationship
cor(DT$MessageCount, DT$Distance) #[1] 0.00664321
cor(uDT$MessageCount, uDT$Distance) #[1] 0.00779028





# Need to control for user averages -- discount A->B message count by average A->? message count 
model_lm_01 <- lm(MessageCount~Distance, uDT)
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
hist(model_lm_01$residuals[ns], main="Histogram of Residuals", xlab = "bf residuals")
# non- normal shaped histogram of residuals.. long tail cluster at 0

plot(uDT$Distance[ns], model_lm_01$residuals[ns])
#  departures from constant variance & linearity.

# Log trasnformation?
hist(uDT$MessageCount[ns])
hist(uDT$Distance[ns])

hist(log(uDT$MessageCount[ns]))
hist(log(uDT$Distance[ns]))

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


# Removing outliers in Message Count, 98th percentile
quantile(uDT$MessageCount, probs=seq(.9,1,.01))
uDT_outmess = subset(uDT, uDT$MessageCount<50)


hist(uDT_outmess$MessageCount[ns]) # poison distribution.. therefore poison regression (log-linear)
hist(uDT_outmess$Distance[ns])
plot(uDT_outmess$MessageCount[ns], uDT_outmess$Distance[ns])


hist(log(DT_outmess$MessageCount[ns]))
hist(log(DT_outmess$Distance[ns]))
plot(log(DT_outmess$MessageCount[ns]), DT_outmess$Distance[ns])
plot((DT_outmess$MessageCount[ns]), log(DT_outmess$Distance[ns]))
plot(log(DT$MessageCount[ns]), log(DT$Distance[ns]))


model_glm_01 <- glm(MessageCount~Distance, data=DT, family="poisson")
summary(model_glm_01)
hist(model_glm_01$residuals[ns])
# Deviance residuals are approximately normally distributed if the model is specified correctly.In our example, it shows a little bit of skeweness since median is not quite zero. 


model_glm_02 <- glm(MessageCount~Distance, 
                    data=subset(uDT, uDT$MessageCount<88), family="poisson")
summary(model_glm_02)
# hist(model_glm_01$residuals[ns])