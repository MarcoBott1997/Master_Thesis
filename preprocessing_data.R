
#Data cleaning
tab = read.table("data_merged.csv", header = TRUE, sep = "\t")

#Remotion of duplicates
tab = distinct(tab)

#Remotion of rows with missing values
tab = na.omit(tab) ##Removed all the observations from 2 year

#Outlier detection
tab = filter(tab, tab$height_cm>30 & tab$height_cm<250)
tab = filter(tab, tab$weight_kg>10 & tab$weight_kg<200)
tab = filter(tab, tab$people_cohabiting < 20)
tab = filter(tab, tab$bmi<100 & tab$bmi>5)   #1 children over, 1 under
tab = filter(tab, tab$waist_cm>20 & tab$waist_cm<200)  #4 under, 3 over
tab = filter(tab, tab$tot_screen_time != 24) #8 children removed. I decided not to remove further since the values near 24 don't look like outliers

balance_years <- function(tab) {
  tab_year1 = tab[tab$eventname == "1_year_follow_up_y_arm_1",]
  tab_year0 = tab[tab$eventname == "baseline_year_1_arm_1",]
  subject_test = unique(tab_year0$subjectkey)
  subject_retest=unique(tab_year1$subjectkey)
  
  tab_new = tab[(tab$subjectkey %in% subject_retest) & (tab$subjectkey %in% subject_test),]
  
  return(tab_new)
}
balance_years(tab)

write.table(tab, file = "data_merged_cleaned.csv", sep = "\t", row.names = F)



rm(list = ls())
library(dplyr)
library(tidyverse)
library(caret)
library(car)
library(MASS)
library(pscl)
library(MLmetrics)
library(lmtest)
#Setting working directory
wd <- "C:/Users/Marco Bottino/Desktop/Marco/Poli/Tesi Magistrale/ABCD"
setwd(wd)

dataset = read.csv("data_merged_cleaned2.csv",header = TRUE)
attach(dataset)


dataset$social_activities_time = social_time + texting_time + videochat_time
dataset$social_activities_time[dataset$social_activities_time > 4] = 4
dataset$race_ethnicity = as.factor(dataset$race_ethnicity)
dataset$parents_income = as.factor(dataset$parents_income)
dataset = dataset[-13086,]

detach(dataset)
attach(dataset)
write.table(dataset, file = "data_merged_cleaned3.csv", sep = "\t", row.names = F)

#Creation of subdatasets

balance_sex <- function(tab) {
  tab_M = tab[tab$sex == "M",]
  tab_F = tab[tab$sex == "F",]
  n = min(nrow(tab_M), nrow(tab_F))
  tab_M_new = sample_n(tab_M, n)
  tab_F_new = sample_n(tab_F, n)
  tab_new = rbind(tab_M_new,tab_F_new)
  
  return(tab_new)
}
dataset_most_screen_time = dataset[tot_screen_time>12,]
dataset_most_screen_time = balance_years(dataset_most_screen_time)
names_most_screen_time = unique(dataset_most_screen_time$subjectkey)
write.table(names_most_screen_time, file="id_most_screen_time.txt", row.names=FALSE, col.names=FALSE, quote = FALSE)

dataset_least_screen_time = dataset[tot_screen_time<=0.5,]
dataset_least_screen_time = balance_years(dataset_least_screen_time)
names_least_screen_time = unique(dataset_least_screen_time$subjectkey)
write.table(names_least_screen_time, file="id_least_screen_time.txt", row.names=FALSE, col.names=FALSE, quote = FALSE)

dataset_tv_time = dataset[tv_time == 4 & tot_screen_time <= 10,]
dataset_tv_time = balance_years(dataset_tv_time)
names_tv_time = unique(dataset_tv_time$subjectkey)
write.table(names_tv_time, file="id_tv_time.txt", row.names=FALSE, col.names=FALSE, quote = FALSE)


dataset_video_time = dataset[video_time == 4 & tot_screen_time <= 10,]
dataset_video_time = balance_years(dataset_video_time)
names_video_time = unique(dataset_video_time$subjectkey)
write.table(names_video_time, file="id_video_time.txt", row.names=FALSE, col.names=FALSE, quote = FALSE)

dataset_videogames_time = dataset[videogames_time == 4 & tot_screen_time <= 10,]
dataset_videogames_time = balance_years(dataset_videogames_time)
names_videogames_time = unique(dataset_videogames_time$subjectkey)
write.table(names_videogames_time, file="id_videogames_time.txt", row.names=FALSE, col.names=FALSE, quote = FALSE)


#MODEL 1: FULL LINEAR REGRESSION ON SIMPLE DATASET
simple_reg = lm(cbcl_score~age_months+sex+parents_income+people_cohabiting+height_cm+weight_kg+waist_cm+bmi+race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+video_time+videogames_time+social_activities_time)
summary(simple_reg)
row_full = c(summary(simple_reg)$coefficients[31,c(1,4)],summary(simple_reg)$coefficients[32,c(1,4)],summary(simple_reg)$coefficients[33,c(1,4)],summary(simple_reg)$coefficients[34,c(1,4)])

simple_reg_glm = glm(cbcl_score~age_months+sex+parents_income+people_cohabiting+height_cm+weight_kg+
                       waist_cm+bmi+race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+
                       video_time+videogames_time+social_activities_time, family = gaussian)
summary(simple_reg_glm)
drop1(simple_reg_glm, test = 'Chi')
    #removing PEOPLE_COHABITING
simple_reg_glm = glm(cbcl_score~age_months+sex+parents_income+height_cm+weight_kg+
                       waist_cm+bmi+race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+
                       video_time+videogames_time+social_activities_time, family = gaussian)
drop1(simple_reg_glm, test = 'Chi')
    #removing WEIGHT_KG
simple_reg_glm = glm(cbcl_score~age_months+sex+parents_income+height_cm+
                       waist_cm+bmi+race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+
                       video_time+videogames_time+social_activities_time, family = gaussian)
drop1(simple_reg_glm, test = 'Chi')
    #removing SOCIAL_ACTIVITIES_TIME
simple_reg_glm = glm(cbcl_score~age_months+sex+parents_income+height_cm+
                       waist_cm+bmi+race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+
                     video_time+videogames_time, family = gaussian)
drop1(simple_reg_glm, test = 'Chi')
    #removing BMI
simple_reg_glm = glm(cbcl_score~age_months+sex+parents_income+height_cm+
                       waist_cm+race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+
                       video_time+videogames_time, family = gaussian)
drop1(simple_reg_glm, test = 'Chi')
    #removing VIDEO_TIME
simple_reg_glm = glm(cbcl_score~age_months+sex+parents_income+height_cm+
                       waist_cm+race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+
                       videogames_time, family = gaussian)
drop1(simple_reg_glm, test = 'Chi')
summary(simple_reg_glm)

par(mfrow = c(2, 2))
plot(simple_reg_glm)
par(mfrow = c(1,1))

#MODEL 2: FULL LINEAR REGRESSION ON STD DATASET
std_dataset <- dataset %>% mutate_at(c("tot_screen_time","sleep_disturb","anx_depr","with_depr","som_comp","social_pr","thought_pr","att_pr","rule_br_bh","agg_bh","internalizing_score","externalizing_score","cbcl_score","age_months","people_cohabiting","physical_activity","height_cm","weight_kg","waist_cm","bmi","DIMS","SBD","DA","SWTD","DOES","SHY","tv_time","video_time","videogames_time","social_activities_time"), ~(scale(.) %>% as.vector))
simple_reg_std = lm(std_dataset$cbcl_score~std_dataset$age_months+std_dataset$sex+std_dataset$parents_income+std_dataset$people_cohabiting+std_dataset$height_cm+std_dataset$weight_kg+std_dataset$waist_cm+std_dataset$bmi+std_dataset$race_ethnicity+std_dataset$DIMS+std_dataset$SBD+std_dataset$DA+std_dataset$SWTD+std_dataset$DOES+std_dataset$SHY+std_dataset$physical_activity+std_dataset$tv_time+std_dataset$video_time+std_dataset$videogames_time+std_dataset$social_activities_time)
summary(simple_reg_std)

#MODEL 3: FULL LINEAR REGRESSION ON SIMPLE DATASET WITH INTERACTION
interaction_reg = step(lm(cbcl_score~(age_months+sex+parents_income+people_cohabiting+height_cm+weight_kg+waist_cm+bmi+race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity)*(tv_time+video_time+videogames_time+social_activities_time)))
summary(interaction_reg)

#MODEL 4: GLM WITH POISSON FAMILY
mod_poisson = glm(cbcl_score~age_months+sex+parents_income+people_cohabiting+height_cm+weight_kg+waist_cm+
                    bmi+race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+video_time+
                    videogames_time+social_activities_time, family = poisson)
summary(mod_poisson)
drop1(mod_poisson)
    #removing SBD and SOCIAL_ACTIVITIES_TIME
mod_poisson = glm(cbcl_score~age_months+sex+parents_income+people_cohabiting+height_cm+weight_kg+waist_cm+
                    bmi+race_ethnicity+DIMS+DA+SWTD+DOES+SHY+physical_activity+tv_time+video_time+
                    videogames_time, family = poisson)
summary(mod_poisson)

par(mfrow = c(2, 2))
plot(mod_poisson)
par(mfrow = c(1,1))

#MODEL 5: GLM WITH POISSON FAMILY WITH OVERDISPERSION
mod_quasipoisson = glm(cbcl_score~age_months+sex+parents_income+people_cohabiting+height_cm+weight_kg+
                       waist_cm+bmi+race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+
                        video_time+videogames_time+social_activities_time, family = quasipoisson)
summary(mod_quasipoisson)
drop1(mod_quasipoisson, test = 'F')
    #removing SBD and SOCIAL_ACTIVITIES_TIME
mod_quasipoisson = glm(cbcl_score~age_months+sex+parents_income+people_cohabiting+height_cm+weight_kg+
                         waist_cm+bmi+race_ethnicity+DIMS+DA+SWTD+DOES+SHY+physical_activity+tv_time+
                         video_time+videogames_time, family = quasipoisson)
drop1(mod_quasipoisson, test = 'F')
    #removing WEIGHT_KG
mod_quasipoisson = glm(cbcl_score~age_months+sex+parents_income+people_cohabiting+height_cm+
                         waist_cm+bmi+race_ethnicity+DIMS+DA+SWTD+DOES+SHY+physical_activity+tv_time+
                         video_time+videogames_time, family = quasipoisson)
drop1(mod_quasipoisson, test = 'F')
    #removing BMI
mod_quasipoisson = glm(cbcl_score~age_months+sex+parents_income+people_cohabiting+height_cm+
                         waist_cm+race_ethnicity+DIMS+DA+SWTD+DOES+SHY+physical_activity+tv_time+
                         video_time+videogames_time, family = quasipoisson)
drop1(mod_quasipoisson, test = 'F') 
    #removing VIDEOGAMES_TIME
mod_quasipoisson = glm(cbcl_score~age_months+sex+parents_income+people_cohabiting+height_cm+
                         waist_cm+race_ethnicity+DIMS+DA+SWTD+DOES+SHY+physical_activity+tv_time+
                         video_time, family = quasipoisson)
drop1(mod_quasipoisson, test = 'F')
summary(mod_quasipoisson)

par(mfrow = c(2, 2))
plot(mod_quasipoisson)
par(mfrow = c(1,1))

#MODEL 6: GLM WITH NEGATIVE BINOMIAL FAMILY
mod_negbin = glm.nb(cbcl_score~age_months+sex+parents_income+people_cohabiting+height_cm+weight_kg+
                                    waist_cm+bmi+race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+
                                    video_time+videogames_time+social_activities_time, link = "log")
summary(mod_negbin, cor = FALSE)
drop1(mod_negbin, test = "Chi")
    #removing SOCIAL_ACTIVITIES_TIME
mod_negbin = glm.nb(cbcl_score~age_months+sex+parents_income+people_cohabiting+height_cm+weight_kg+
                                    waist_cm+bmi+race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+
                                    video_time+videogames_time, link = "log")
drop1(mod_negbin, test = "Chi") 
    #removing AGE_MONTHS
mod_negbin = glm.nb(cbcl_score~sex+parents_income+people_cohabiting+height_cm+weight_kg+
                                    waist_cm+bmi+race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+
                                    video_time+videogames_time, link = "log")
drop1(mod_negbin, test = "Chi")
    #removing VIDEOGAMES_TIME
mod_negbin = glm.nb(cbcl_score~sex+parents_income+people_cohabiting+height_cm+weight_kg+
                                    waist_cm+bmi+race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+
                                    video_time, link = "log")
summary(mod_negbin)

par(mfrow = c(2, 2))
plot(mod_negbin)
par(mfrow = c(1,1))

#MODEL 7 : GLM ZERO INFLATED MODELS
mod_zip <- zeroinfl(cbcl_score~age_months+sex+parents_income+people_cohabiting+height_cm+weight_kg+
                                    waist_cm+bmi+race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+
                                    video_time+videogames_time+social_activities_time | age_months+sex+people_cohabiting+height_cm+weight_kg+
                                    waist_cm+bmi+race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity,
                                    dist = "poisson", link = "logit")
summary(mod_zip)

mod_zinb <- zeroinfl(cbcl_score~age_months+sex+parents_income+people_cohabiting+height_cm+weight_kg+
                                     waist_cm+bmi+race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+
                                     video_time+videogames_time+social_activities_time, dist = "negbin", link = "logit")
summary(mod_zinb)
lrtest(mod_zip, mod_zinb)
  #model with only significant features in the binomial part
mod_zinb_1 <- zeroinfl(cbcl_score~age_months+sex+parents_income+people_cohabiting+height_cm+weight_kg+
                                       waist_cm+bmi+race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+
                                       video_time+videogames_time+social_activities_time | people_cohabiting+race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY,
                                      dist = "negbin", link = "logit")
summary(mod_zinb_1)
  #model with only significant features in both the parts
mod_zinb_2 <- zeroinfl(cbcl_score~age_months+sex+parents_income+height_cm+weight_kg+
                                       waist_cm+bmi+race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+
                                       video_time+videogames_time | people_cohabiting+race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY,
                                     dist = "negbin", link = "logit")
summary(mod_zinb_2)
lrtest(mod_zinb, mod_zinb_1)
lrtest(mod_zinb, mod_zinb_2)

plot(cbcl_score,residuals(mod_zinb))


# Model selection
RsquaredAdj = c(
summary(simple_reg)$adj.r.squared,
summary(simple_reg_std)$adj.r.squared,
summary(interaction_reg)$adj.r.squared
)
Rsquared = c(
summary(simple_reg)$r.squared,
summary(simple_reg_std)$r.squared,
summary(interaction_reg)$r.squared
)
Aic = AIC(simple_reg,simple_reg_std,interaction_reg)[,2]
ModelChoice = data.frame(
Mod=c("Simple Regression","Simple Regression - Std Dataset","Regression with interaction"),
RsquaredAdj=RsquaredAdj,
Rsquared=Rsquared,
Aic = Aic
)
ModelChoice # il nuovo modello è il migliore
write.table(ModelChoice, file = "ModelChoice.txt", sep = ",", quote = FALSE, row.names = T)


fit_glm = predict(simple_reg_glm, newdata = dataset)
fit_poiss = predict(mod_poisson, newdata = dataset)
fit_qpoiss = predict(mod_quasipoisson, newdata = dataset)
fit_nb = predict(mod_negbin, newdata = dataset)
fit_zip = predict(mod_zip, newdata = dataset)
fit_zinb = predict(mod_zinb, newdata = dataset)
r_corr = c(
  cor(fit_glm, cbcl_score, method = "pearson"),
  cor(fit_poiss, cbcl_score, method = "pearson"),
  cor(fit_qpoiss, cbcl_score, method = "pearson"),
  cor(fit_nb, cbcl_score, method = "pearson"),
  cor(fit_zip, cbcl_score, method = "pearson"),
  cor(fit_zinb, cbcl_score, method = "pearson")
  )
p_corr = c(
  cor(fit_glm, cbcl_score, method = "spearman"),
  cor(fit_poiss, cbcl_score, method = "spearman"),
  cor(fit_qpoiss, cbcl_score, method = "spearman"),
  cor(fit_nb, cbcl_score, method = "spearman"),
  cor(fit_zip, cbcl_score, method = "spearman"),
  cor(fit_zinb, cbcl_score, method = "spearman")
  )
#Intercept = c(
#  coef(lm(cbcl_score~fit_glm))[1],
#  coef(lm(cbcl_score~fit_poiss))[1],
#  coef(lm(cbcl_score~fit_qpoiss))[1],
#  coef(lm(cbcl_score~fit_nb))[1],
#  coef(lm(cbcl_score~fit_zip))[1],
#  coef(lm(cbcl_score~fit_zinb))[1]
#  )
#Slope = c(
#  coef(lm(cbcl_score~fit_glm))[2],
#  coef(lm(cbcl_score~fit_poiss))[2],
#  coef(lm(cbcl_score~fit_qpoiss))[2],
#  coef(lm(cbcl_score~fit_nb))[2],
#  coef(lm(cbcl_score~fit_zip))[2],
#  coef(lm(cbcl_score~fit_zinb))[2]
#  )
MSE = c(
  MSE(cbcl_score,fit_glm),
  MSE(cbcl_score,fit_poiss),
  MSE(cbcl_score,fit_qpoiss),
  MSE(cbcl_score,fit_nb),
  MSE(cbcl_score,fit_zip),
  MSE(cbcl_score,fit_zinb)
  )
mae = c(
  MAE(cbcl_score,fit_glm),
  MAE(cbcl_score,fit_poiss),
  MAE(cbcl_score,fit_qpoiss),
  MAE(cbcl_score,fit_nb),
  MAE(cbcl_score,fit_zip),
  MAE(cbcl_score,fit_zinb)
  )
Aic = AIC(simple_reg_glm,mod_poisson,mod_quasipoisson,mod_negbin,mod_zip,mod_zinb)
ModelSelection = data.frame(
  Mod=c("Gaussian","Poisson", "Poisson with overdispersion", "Negative Binomial", "Zero Inflated Poisson", "Zero Inflated Negative Binomial"),
  Pearson_correlation = r_corr,
  Spearman_correlation = p_corr,
#  Intercept = Intercept,
#  Slope = Slope,
  MSE = MSE,
  MAE = mae,
  AIC = Aic[,2],
  Df = Aic[,1]
  )
ModelSelection



#anx_depr
simple_reg_anx_depr = lm(anx_depr~age_months+sex+parents_income+people_cohabiting+height_cm+weight_kg+waist_cm+bmi+race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+video_time+videogames_time+social_activities_time)
summary(simple_reg_anx_depr)
row_anx_depr = c(summary(simple_reg_anx_depr)$coefficients[31,c(1,4)],summary(simple_reg_anx_depr)$coefficients[32,c(1,4)],summary(simple_reg_anx_depr)$coefficients[33,c(1,4)],summary(simple_reg_anx_depr)$coefficients[34,c(1,4)])
#with_depr
simple_reg_with_depr = lm(with_depr~age_months+sex+parents_income+people_cohabiting+height_cm+weight_kg+waist_cm+bmi+race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+video_time+videogames_time+social_activities_time)
summary(simple_reg_with_depr)
row_with_depr = c(summary(simple_reg_with_depr)$coefficients[31,c(1,4)],summary(simple_reg_with_depr)$coefficients[32,c(1,4)],summary(simple_reg_with_depr)$coefficients[33,c(1,4)],summary(simple_reg_with_depr)$coefficients[34,c(1,4)])
#som_comp
simple_reg_som_comp = lm(som_comp~age_months+sex+parents_income+people_cohabiting+height_cm+weight_kg+waist_cm+bmi+race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+video_time+videogames_time+social_activities_time)
summary(simple_reg_som_comp)
row_som_comp = c(summary(simple_reg_som_comp)$coefficients[31,c(1,4)],summary(simple_reg_som_comp)$coefficients[32,c(1,4)],summary(simple_reg_som_comp)$coefficients[33,c(1,4)],summary(simple_reg_som_comp)$coefficients[34,c(1,4)])
#social_pr
simple_reg_social_pr = lm(social_pr~age_months+sex+parents_income+people_cohabiting+height_cm+weight_kg+waist_cm+bmi+race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+video_time+videogames_time+social_activities_time)
summary(simple_reg_social_pr)
row_social_pr = c(summary(simple_reg_social_pr)$coefficients[31,c(1,4)],summary(simple_reg_social_pr)$coefficients[32,c(1,4)],summary(simple_reg_social_pr)$coefficients[33,c(1,4)],summary(simple_reg_social_pr)$coefficients[34,c(1,4)])
#thought_pr
simple_reg_thought_pr = lm(thought_pr~age_months+sex+parents_income+people_cohabiting+height_cm+weight_kg+waist_cm+bmi+race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+video_time+videogames_time+social_activities_time)
summary(simple_reg_thought_pr)
row_thought_pr = c(summary(simple_reg_thought_pr)$coefficients[31,c(1,4)],summary(simple_reg_thought_pr)$coefficients[32,c(1,4)],summary(simple_reg_thought_pr)$coefficients[33,c(1,4)],summary(simple_reg_thought_pr)$coefficients[34,c(1,4)])
#att_pr
simple_reg_att_pr = lm(att_pr~age_months+sex+parents_income+people_cohabiting+height_cm+weight_kg+waist_cm+bmi+race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+video_time+videogames_time+social_activities_time)
summary(simple_reg_att_pr)
row_att_pr = c(summary(simple_reg_att_pr)$coefficients[31,c(1,4)],summary(simple_reg_att_pr)$coefficients[32,c(1,4)],summary(simple_reg_att_pr)$coefficients[33,c(1,4)],summary(simple_reg_att_pr)$coefficients[34,c(1,4)])
#rule_br_bh
simple_reg_rule_br_bh = lm(rule_br_bh~age_months+sex+parents_income+people_cohabiting+height_cm+weight_kg+waist_cm+bmi+race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+video_time+videogames_time+social_activities_time)
summary(simple_reg_rule_br_bh)
row_rule_br_bh = c(summary(simple_reg_rule_br_bh)$coefficients[31,c(1,4)],summary(simple_reg_rule_br_bh)$coefficients[32,c(1,4)],summary(simple_reg_rule_br_bh)$coefficients[33,c(1,4)],summary(simple_reg_rule_br_bh)$coefficients[34,c(1,4)])
#agg_bh
simple_reg_agg_bh = lm(agg_bh~age_months+sex+parents_income+people_cohabiting+height_cm+weight_kg+waist_cm+bmi+race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+video_time+videogames_time+social_activities_time)
summary(simple_reg_agg_bh)
row_agg_bh = c(summary(simple_reg_agg_bh)$coefficients[31,c(1,4)],summary(simple_reg_agg_bh)$coefficients[32,c(1,4)],summary(simple_reg_agg_bh)$coefficients[33,c(1,4)],summary(simple_reg_agg_bh)$coefficients[34,c(1,4)])
#internalizing_score
simple_reg_internalizing_score = lm(internalizing_score~age_months+sex+parents_income+people_cohabiting+height_cm+weight_kg+waist_cm+bmi+race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+video_time+videogames_time+social_activities_time)
summary(simple_reg_internalizing_score)
row_internalizing_score = c(summary(simple_reg_internalizing_score)$coefficients[31,c(1,4)],summary(simple_reg_internalizing_score)$coefficients[32,c(1,4)],summary(simple_reg_internalizing_score)$coefficients[33,c(1,4)],summary(simple_reg_internalizing_score)$coefficients[34,c(1,4)])
#externalizing_score
simple_reg_externalizing_score = lm(externalizing_score~age_months+sex+parents_income+people_cohabiting+height_cm+weight_kg+waist_cm+bmi+race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+video_time+videogames_time+social_activities_time)
summary(simple_reg_externalizing_score)
row_externalizing_score = c(summary(simple_reg_externalizing_score)$coefficients[31,c(1,4)],summary(simple_reg_externalizing_score)$coefficients[32,c(1,4)],summary(simple_reg_externalizing_score)$coefficients[33,c(1,4)],summary(simple_reg_externalizing_score)$coefficients[34,c(1,4)])
matrix_estimates = rbind(row_full, row_anx_depr, row_with_depr, row_som_comp, row_social_pr, row_thought_pr, row_att_pr, row_rule_br_bh, row_agg_bh, row_internalizing_score, row_externalizing_score)
rownames(matrix_estimates) = c("CBCL score", "Anxious / Depressed", "Withdrawn / Depressed", "Somatic Complaints", "Social Problems", "Thought Problems", "Attention Problems", "Rule-Breaking Behavior", "Aggressive Behavior", "Internalizing score", "Externalizing score")
colnames(matrix_estimates) = c("TV time (Estimate)", "TV time (p-value)", "Video time (Estimate)", "Video time (p-value)","Videogames time (Estimate)", "Videogames time (p-value)","Social activities time (Estimate)", "Social activities time (p-value)")
matrix_estimates
get_symbol <- function(mod_summary_sign) {
  mod_summary_stars <- NA                             # Named vector with significance stars
  mod_summary_stars[mod_summary_sign < 0.1] <- "."
  mod_summary_stars[mod_summary_sign < 0.05] <- "*"
  mod_summary_stars[mod_summary_sign < 0.01] <- "**"
  mod_summary_stars[mod_summary_sign < 0.001] <- "***"
  mod_summary_stars[is.na(mod_summary_stars)] <- "n.s."
  names(mod_summary_stars) <- names(mod_summary_sign)
  mod_summary_stars
  return(mod_summary_stars)
  }
matrix_estimates[,c(2,4,5,8)] = get_symbol(matrix_estimates[,c(2,4,6,8)])
write.table(matrix_estimates, file = "matrix_estimates.txt", sep = ",", quote = FALSE, row.names = T)



