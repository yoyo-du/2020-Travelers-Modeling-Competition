library(tweedie)
library(ggplot2)
library(TDboost)
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggiraph)
library(MLmetrics)
library(patchwork)

data <- read.csv("InsNova_train.csv")
test <- read.csv("InsNova_test.csv")

data$veh_age = as.factor(data$veh_age)
data$dr_age = as.factor(data$dr_age)
data$veh_body = as.factor(data$veh_body)
data$area = as.factor(data$area)

#some explanatory analysis
ggplot(data=data, aes(x=claim_cost))+geom_histogram(bins=50, fill="lightblue")+
  labs(title="Histogram of Claim Cost")

positive <- data[which(data$claim_cost>0),]
zero <- data[which(data$claim_cost==0),]

boxplot(log(positive$claim_cost)~positive$veh_body)
boxplot(log(positive$claim_cost)~positive$dr_age)
boxplot(log(positive$claim_cost)~positive$veh_age)
boxplot(log(positive$claim_cost)~positive$area)
# boxplot(log(positive$claim_cost)~positive$gender) almost the same

plot(density(data$claim_cost))
plot(density((data$exposure)^8))

## split train and test set
dt = sample(nrow(data), nrow(data)*.7)
train <- data[dt,]
validation <- data[-dt,]

table(train$claim_count)
table(train$claim_ind)
table(train$veh_body)

body_claim <- table(train$veh_body, train$claim_ind)# indicate dependence
chisq.test(body_claim)

gender_claim <- table(train$gender, train$claim_ind) #independent 
chisq.test(gender_claim)

area_claim <- table(train$area, train$claim_ind) #independent
chisq.test(area_claim)

vehage_claim <- table(train$veh_age, train$claim_ind) # dependent
chisq.test(vehage_claim)

drage_claim <- table(train$dr_age, train$claim_ind) # dependent
chisq.test(drage_claim)

data %>%
  group_by(claim_count) %>%
  summarize(median_cost = mean(claim_cost)) %>%
  arrange(desc(median_cost)) 
# policyholders have 2 claims have the highest claim cost median

dat<-data%>%group_by(claim_count)%>%summarize(median_cost = mean(claim_cost))
dat

dat2<-dat%>%pivot_longer(cols = 2)
g<-ggplot(dat2,aes(claim_count,value,fill=claim_count))+geom_bar(stat='identity')+facet_wrap(~name,nrow = 2)

print(g)

data %>%
  group_by(veh_body) %>%
  summarize(median_cost = mean(claim_cost)) %>%
  arrange(desc(median_cost))
# policyholders with HDTOP veh_body has the highest claim cost median

data %>%
  group_by(veh_age) %>%
  summarize(median_cost = mean(claim_cost)) %>%
  arrange(desc(median_cost))
# policyholders with the veh_age in group 4 has the highest claim cost median

data %>%
  group_by(dr_age) %>%
  summarize(median_cost = mean(claim_cost)) %>%
  arrange(desc(median_cost))
# policyholders in the youngest age group has the highest claim cost median

data %>%
  group_by(area) %>%
  summarize(median_cost = mean(claim_cost)) %>%
  arrange(desc(median_cost))
# policyholders come from area F has the highest median claim cost

# density plot of claim_cost by area
g<-ggplot(data,aes(x = claim_cost, color = area,fill=area,data_id=area,tooltip=area))+
  geom_density_interactive(alpha = 0.4) +
  theme(legend.position = "bottom",axis.title.y = element_text(angle=0))
g

# separate histogram of claim_cost by area
g<-ggplot(data, aes(x = claim_cost/1000)) +
  geom_histogram(fill = "cornflowerblue",
                 color = "white", bins = 30) +
  facet_wrap(~area) +
  labs(title = "Claim cost histograms by area",x = "Claim cost")
g

# smaller veh_value is associated with larger claim cost
plot(data$claim_cost~data$veh_value)


#find index p for tweedie model - profile likelihood
##estimation of p can be obtained by maximaxizing the saddlepoint profile likelihood for p
p = tweedie.profile(data$claim_cost~1, do.plot=TRUE, p.vec = seq(1.1, 1.8, by=0.1),
                        verbose = 1, control=list(maxit=glm.control()$maxit), 
                        weights = data$exposure)

str(p)
## p = 1.58

## TDboost model 
set.seed(12345)
td_model <- TDboost(claim_cost ~ veh_value + veh_body + veh_age + dr_age + area + exposure, data = data, 
                    distribution = list(name = "EDM", alpha = 1.58), 
                    n.trees = 3000, cv.folds = 5, interaction.depth = 3, shrinkage = .005, bag.fraction = 0.5, 
                    verbose = FALSE)

valid_tweedie <- predict(td_model, validation, n.trees = 3000, type = "response")
train_tweedie <- predict(td_model, train, n.trees = 3000, type = "response")


## gini index of the validation set and training set 
NormalizedGini(valid_tweedie, validation$claim_cost)
NormalizedGini(train_tweedie, train$claim_cost)

## plot the relative influence 
summary(td_model, 3000)

## partial dependence plot of exposure
rel_imp <- summary(td_model)
ggplot(data=rel_imp, aes(x=rel.inf,y=reorder(var, rel.inf))) + 
  geom_bar(stat="identity", color="black", fill="#BC2224") +
  ggtitle("Relative Feature Importance") + xlab("Relative Importance") + ylab("") +
  scale_y_discrete(labels=c("Vehicle Age","Vehicle Body", "Area",
                            "Driver Age", "Vehicle Value", "Exposure"))

dat1 = plot.TDboost(td_model, 6, 3000, return.grid = TRUE)
g_exp <- ggplot(dat1, aes(x = exposure, y = y))+geom_line()+
  #ggtitle("Partial Dependence Plot of Exposure")+
  xlab("Exposure") + ylab("Marginal Effect")

dat1 = plot.TDboost(td_model, 1, 3000, return.grid = TRUE)
g_vval <- ggplot(dat1, aes(x = veh_value, y = y))+geom_line()+
  #ggtitle("Partial Dependence Plot of Vehicle Value") +
  xlab("Vehicle Value") + ylab("Marginal Effect")

dat1 = plot.TDboost(td_model, 4, 3000, return.grid = TRUE)
g_dr <- ggplot(dat1, aes(x = dr_age, y = y))+geom_bar(stat="Identity",fill="#26385E")+
  #ggtitle("Partial Dependence Plot of Driver Age") + 
  xlab("Driver Age") + ylab("Marginal Effect")

dat1 = plot.TDboost(td_model, 5, 3000, return.grid = TRUE)
g_ar <- ggplot(dat1, aes(x = area, y = y))+geom_bar(stat="Identity",fill="#26385E")+
  #ggtitle("Partial Dependence Plot of Driver Age") + 
  xlab("Area") + ylab("Marginal Effect")

g_ar
(g_exp / g_vval) | g_dr

plot.TDboost(td_model, 1, 3000)

plot.TDboost(td_model, c(6,1), 3000)
plot.TDboost(td_model, c(6, 2), 3000)
plot.TDboost(td_model, c(6, 3), 3000)
plot.TDboost(td_model, c(6, 4), 3000)
plot.TDboost(td_model, c(6, 5), 3000)
plot.TDboost(td_model, c(6, 6), 3000)

plot.TDboost(td_model, c(1,5), 3000)
plot.TDboost(td_model, c(4, 5), 3000)


plot.TDboost(td_model, c(5), 3000)

plot.TDboost(td_model, c(2,3), 3000)

plot(density(data$exposure))
hist(data$exposure,breaks=30)
summary(data$veh_value)
summary(test$veh_value)
hist(data$veh_value, breaks=30)

summary(data$claim_cost[data$exposure<0.07])
plot(data$claim_cost ~ data$exposure)

validation$preds <- valid_tweedie

out_exp <- ggplot(data=validation, aes(x=exposure,y=preds)) + geom_point(alpha=.15,color="#26385E") + xlab("Exposure") + ylab("Predicted Claim Cost")
out_drage <- ggplot(data=validation, aes(x=dr_age,y=preds)) + geom_boxplot(color="#26385E") + xlab("Driver Age") + ylab("Predicted Claim Cost")
out_area <- ggplot(data=validation, aes(x=area,y=preds)) + geom_boxplot(color="#26385E") + xlab("Area") + ylab("Predicted Claim Cost")

out_exp | out_drage / out_area

ggplot(data=validation, aes(x=gender,y=claim_cost)) + geom_boxplot(alpha=.5,color="#26385E") + xlab("Exposure") + ylab("Predicted Claim Cost")


plot(valid_tweedie ~ validation$exposure)
plot(valid_tweedie ~ validation$dr_age)
plot(valid_tweedie ~ validation$area)

