# loading required packages
rm(list = ls())
library(arm)
library(pROC)
library(e1071)
library(caret)
library(ggplot2)
require(gridExtra)
library(dplyr)
library(magrittr)
library(stargazer)
library(rms)
library(ggdark)
library(ggeasy)
library(tidyverse)
library(viridis)
library(sjPlot)
library(qqplotr)
library(lme4)


load("/Users/mohammadanas/Downloads/streetrx.RData")

# choosing needed columns (needed)
needed_cols <- c('ppm','state','USA_region','source','api_temp','form_temp','mgstr','bulk_purchase')
streetrx_ncols <- streetrx[,needed_cols]

# Observing the data set
summary(streetrx_ncols)
head(streetrx_ncols)
colnames(streetrx_ncols)

# choosing the required drug
streetrx_Codeine <- streetrx_ncols[streetrx_ncols['api_temp'] == 'codeine',]

# ommiting missing values
streetrx_cleaned <- na.omit(streetrx_Codeine)

# create new variable for source
unique(streetrx_cleaned$source)
streetrx_cleaned$source_F <- 'Internet'
streetrx_cleaned$source_F[streetrx_cleaned$source == 'Personal'] <-  'Personal'
streetrx_cleaned$source_F[streetrx_cleaned$source == 'Heard it'] <-  'Heard it'
streetrx_cleaned$source_F[streetrx_cleaned$source == ''] <-  'Not Indicated'
streetrx_cleaned$source_F <- factor(streetrx_cleaned$source_F, ordered = FALSE)
streetrx_cleaned$source_F <- relevel(streetrx_cleaned$source_F, ref = "Not Indicated")

## TO change labels for Bulk Purchase 

# create new variable for dosage
str(streetrx_cleaned)
streetrx_cleaned$dosage <- 'medium'
streetrx_cleaned$dosage[streetrx_cleaned$mgstr == 15] <-  'low'
streetrx_cleaned$dosage[streetrx_cleaned$mgstr == 60] <-  'high'
streetrx_cleaned$dosage <- factor(streetrx_cleaned$dosage)
streetrx_cleaned$dosage <- relevel(streetrx_cleaned$dosage, ref = "low")

# convert USA state to other
levels(streetrx_cleaned$state)
levels(streetrx_cleaned$state)[50] <-"Other"
unique(streetrx_cleaned$state)


# distribution ppm
p1 = streetrx_cleaned %>%
  ggplot(aes(x = ppm, fill = )) +
  geom_histogram(bins = 40, color = "black", linetype = "dashed", fill = "lightblue") +
  labs(title="Distribution of price per milligram for Codeine",y="Frequency", x= "ppm")+
  theme_classic() + theme(plot.title = element_text(hjust = 0.5,size=10),legend.position="none")

# distribution of log_ppm
p2 = streetrx_cleaned %>%
  ggplot(aes(x = log(ppm))) + 
  geom_histogram(bins = 30,color = "black", linetype = "dashed", fill = "lightblue") +  scale_fill_brewer(palette="Blues") +
  scale_fill_viridis(discrete = TRUE) +
  labs(title="Distribution of Log ppm for Codeine",y="Frequency", x= "Log ppm") + 
  theme_classic() + theme(plot.title = element_text(hjust = 0.5,size=10),legend.position="none")


grid.arrange(p1,p2, ncol=2)

# Source vs log(ppm)
p1 = streetrx_cleaned %>%
  ggplot(aes(x = source_F, y=log(ppm),fill = source_F)) +
  geom_boxplot() + labs(title="ppm vs Source",
                        x="Source",y="Log ppm") + scale_fill_brewer(palette="Set3")+
  theme_classic() + theme(legend.position="none",plot.title = element_text(hjust = 0.5,size=10))

# Dosage vs log(ppm)
p2 = streetrx_cleaned %>%
  ggplot(aes(x = dosage, y=log(ppm),fill = dosage)) +
  geom_boxplot() + labs(title="Log ppm vs Dosage",
                        x="Dosage",y="Log ppm") +scale_fill_brewer(palette="Set3")+
  theme_classic() + theme(legend.position="none",plot.title = element_text(hjust = 0.5,size=10))

# # Bulk-purchase vs log(ppm)
p3 = streetrx_cleaned %>%
  ggplot(aes(x = bulk_purchase, y=log(ppm),fill = bulk_purchase)) +
  geom_boxplot() + labs(title="ppm vs Bulk Purchase",
                        x="Bulk Purchase",y="Log ppm") +scale_fill_brewer(palette="Set3")+
  theme_classic() + theme(legend.position="none",plot.title = element_text(hjust = 0.5,size=10))

grid.arrange(p2, ncol=1)

# Dosage vs Bulk Purchase
p1 = streetrx_cleaned %>%
  ggplot(aes(x = dosage, y=log(ppm),fill = dosage)) +
  geom_boxplot() + facet_wrap(~bulk_purchase) + labs(title="Dosage vs Bulk Purchase",
                                                     x="Dosage",y="Log ppm") + scale_fill_brewer(palette="Set3")+
  theme_classic() + theme(legend.position="none",plot.title = element_text(hjust = 0.5,size=10),axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Dosage vs Source
p2 = streetrx_cleaned %>%
  ggplot(aes(x = dosage, y=log(ppm),fill = dosage)) +
  geom_boxplot() + facet_wrap(~source_F) + labs(title="Dosage vs Source",
                                                x="Dosage",y="Log ppm") +scale_fill_brewer(palette="Set3")+
  theme_classic() + theme(legend.position="none",plot.title = element_text(hjust = 0.5,size=10),axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Bulk-purchase vs Source
p3 = streetrx_cleaned %>%
  ggplot(aes(x = bulk_purchase, y=log(ppm),fill = bulk_purchase)) +
  geom_boxplot() + facet_wrap(~source_F) + labs(title="Bulk Purchase vs Source",
                                                x="Bulk Purchase",y="Log ppm") +scale_fill_brewer(palette="Set3")+
  theme_classic() + theme(legend.position="none",plot.title = element_text(hjust = 0.5,size=10))

grid.arrange(p3, ncol=1)

data <- streetrx_cleaned %>% count(state)

# state vs log(ppm) (random intercept)

sample_state <- data[data['n'] > 50,'state']

ggplot(streetrx_cleaned[is.element(streetrx_cleaned$state,sample_state),], 
       aes(x=state, y=log(ppm), fill=state)) + 
  geom_boxplot() + 
  labs(title="Log ppm levels by state", 
       x="State",y="Log ppm") + theme_classic() + 
  theme(legend.position="none",plot.title = element_text(hjust = 0.5,size=10),axis.text.x = element_text(angle = 90))

# check for slope of source by state 
sample_state <- data[data['n'] > 70,'state']
ggplot(streetrx_cleaned[is.element(streetrx_cleaned$state,sample_state),], 
       aes(x=source_F, y=log(ppm), fill=state)) + 
  geom_boxplot() + facet_wrap(~state)
labs(title="Log ppm levels by state", 
     x="State",y="Log ppm") + theme_classic() + 
  theme(legend.position="none",axis.text.x = element_text(angle = 90))

# dosage vs log(PPM) by state 
sample_state <- data[data['n'] > 70,'state']
ggplot(streetrx_cleaned[is.element(streetrx_cleaned$state,sample_state),], 
       aes(x=dosage, y=log(ppm), fill=state)) + 
  geom_boxplot() + facet_wrap(~state)
labs(title="Log ppm levels by state", 
     x="State",y="Log ppm") + theme_classic() + 
  theme(legend.position="none",axis.text.x = element_text(angle = 90))

# bulk purchase vs log(ppm) by state (ARUSHI)
sample_state <- data[data['n'] > 70,'state']
ggplot(streetrx_cleaned[is.element(streetrx_cleaned$state,sample_state),], 
       aes(x=bulk_purchase, y=log(ppm), fill=state)) + 
  geom_boxplot() + facet_wrap(~state)
labs(title="Log ppm levels by state", 
     x="State",y="Log ppm") + theme_classic() + 
  theme(legend.position="none",axis.text.x = element_text(angle = 90))


## MODEL Building
# base model includes all main effects
base_model <- lm(log(ppm) ~ source_F + bulk_purchase + dosage
                 ,data= streetrx_cleaned)

summary(base_model)
#backward selection
null_model <- lm(log(ppm) ~ source_F, data= streetrx_cleaned)
base_model <- lm(log(ppm) ~ source_F + 
                   bulk_purchase + dosage, data= streetrx_cleaned)
Model_backward <- step(null_model,scope = formula(base_model), direction = "both", trace = 0)
Model_backward$call


# both AIC and BIC come up with the same model which contains bulk_purchase and dosage
# we test with anova to ensure that the we should keep source or not and annova removed it

model_with_source <- lm(formula = log(ppm) ~ dosage + bulk_purchase, data = streetrx_cleaned)
model_without_source <- lm(formula = log(ppm) ~ source_F + dosage + bulk_purchase, data = streetrx_cleaned)
anova(model_without_source, model_with_source)

# use to test interaction that was found interesting in the EDA and annova removes it 
model_without_interaction <- lm(formula = log(ppm) ~ dosage + bulk_purchase, data = streetrx_cleaned)
model_with_interaction <- lm(formula = log(ppm) ~ dosage + bulk_purchase + bulk_purchase*source_F, data = streetrx_cleaned)
anova(model_with_interaction, model_without_interaction)



# we control the intercept of state
model_state_controlled <- lmer(log(ppm) ~  dosage + bulk_purchase + 
                                 (1| state), data = streetrx_cleaned)

# control slope for states and test them with anova
model_state_dosage <- lmer(log(ppm) ~  dosage + bulk_purchase + 
                             (dosage| state), data = streetrx_cleaned)

anova(model_state_controlled,model_state_dosage)
# Random Slope insignificant

# checking for varying slope of bulk_purchase 
model_state_bulk_purchase <- lmer(log(ppm) ~  dosage + bulk_purchase + 
                                    (bulk_purchase| state), data = streetrx_cleaned)

anova(model_state_controlled,model_state_bulk_purchase)
# Random Slope insignificant

# checking for varying intercept of Region 
model_state_USA_region <- lmer(log(ppm) ~  dosage + bulk_purchase + 
                                 (1| state) + (1|USA_region) , data = streetrx_cleaned)

anova(model_state_controlled,model_state_bulk_purchase)
# Random Intercept of USA Region insignificant

# we make final model based after all the anova tests and test assumption
final_model <- lmer(log(ppm) ~  dosage + bulk_purchase + 
                      (1| state), data = streetrx_cleaned)

resid <- residuals(final_model)
fitted <- fitted(final_model)

# check for residual against fitted
ggplot(data = streetrx_cleaned, aes(x = fitted, y = resid)) +
  geom_point() + geom_smooth()+
  labs(title = 'Residuals vs Fitted')
#check for normality
ggplot(mapping = aes(sample = resid )) + stat_qq_point(size =2, color = "blue") + stat_qq_line() + xlab("Theortical Quantiles") + ylab("Sample Quantiles") + ggtitle("Normal QQ Plot") +
  theme(plot.title = element_text(hjust = 0.5) )

# results of the model
summary(final_mode)

# explore random effects
dotplot(ranef(final_model, condVar= TRUE))$state