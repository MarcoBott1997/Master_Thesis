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
library(ggplot2)
library(tidyverse)
library(caret)
library(car)
library(MASS)
library(pscl)
library(MLmetrics)
library(lmtest)
library(reticulate)
#Setting working directory
wd <- "C:/Users/Marco Bottino/Desktop/Marco/Poli/Tesi Magistrale/ABCD"
setwd(wd)

dataset = read.csv("data_merged_cleaned_pca.csv",header = TRUE)
attach(dataset)


#dataset$social_activities_time = social_time + texting_time + videochat_time
#dataset$social_activities_time[dataset$social_activities_time > 4] = 4
dataset$race_ethnicity = as.factor(dataset$race_ethnicity)
dataset$parents_income = as.factor(dataset$parents_income)
#dataset = dataset[-13086,]

detach(dataset)
attach(dataset)
#write.table(dataset, file = "data_merged_cleaned3.csv", sep = "\t", row.names = F)

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


#### MODELS FOR CBCL_SCORE

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
mod_poisson_cbcl = glm(cbcl_score~age_months+sex+parents_income+people_cohabiting+size+slenderness+
                         race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+video_time+
                         videogames_time+social_activities_time, family = poisson)
summary(mod_poisson_cbcl)
drop1(mod_poisson_cbcl)
#removing SBD and SOCIAL_ACTIVITIES_TIME
mod_poisson_cbcl = glm(cbcl_score~age_months+sex+parents_income+people_cohabiting+size+slenderness+
                         race_ethnicity+DIMS+DA+SWTD+DOES+SHY+physical_activity+tv_time+video_time+
                         videogames_time, family = poisson)
summary(mod_poisson_cbcl)

par(mfrow = c(2, 2))
plot(mod_poisson_cbcl)
par(mfrow = c(1,1))

#MODEL 5: GLM WITH POISSON FAMILY WITH OVERDISPERSION
mod_quasipoisson_cbcl = glm(cbcl_score~age_months+sex+parents_income+people_cohabiting+size+slenderness+
                              race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+
                              video_time+videogames_time+social_activities_time, family = quasipoisson)
summary(mod_quasipoisson_cbcl)
drop1(mod_quasipoisson_cbcl, test = 'F')
#removing SBD and SOCIAL_ACTIVITIES_TIME
mod_quasipoisson_cbcl = glm(cbcl_score~age_months+sex+parents_income+people_cohabiting+size+slenderness+
                              race_ethnicity+DIMS+DA+SWTD+DOES+SHY+physical_activity+tv_time+
                              video_time+videogames_time, family = quasipoisson)
drop1(mod_quasipoisson_cbcl, test = 'F')
#removing SIZE
mod_quasipoisson_cbcl = glm(cbcl_score~age_months+sex+parents_income+people_cohabiting+slenderness+
                              race_ethnicity+DIMS+DA+SWTD+DOES+SHY+physical_activity+tv_time+
                              video_time+videogames_time, family = quasipoisson)
drop1(mod_quasipoisson_cbcl, test = 'F')
#removing VIDEOGAMES_TIME
mod_quasipoisson_cbcl = glm(cbcl_score~age_months+sex+parents_income+people_cohabiting+slenderness+
                              race_ethnicity+DIMS+DA+SWTD+DOES+SHY+physical_activity+tv_time+
                              video_time, family = quasipoisson)
drop1(mod_quasipoisson_cbcl, test = 'F')
summary(mod_quasipoisson_cbcl)
par(mfrow = c(2, 2))
plot(mod_quasipoisson_cbcl)
par(mfrow = c(1,1))

#MODEL 6: GLM WITH NEGATIVE BINOMIAL FAMILY
mod_negbin_cbcl = glm.nb(cbcl_score~age_months+sex+parents_income+people_cohabiting+size+slenderness+
                           race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+
                           video_time+videogames_time+social_activities_time, link = "log")
summary(mod_negbin, cor = FALSE)
drop1(mod_negbin, test = "Chi")
#removing SOCIAL_ACTIVITIES_TIME
mod_negbin_cbcl = glm.nb(cbcl_score~age_months+sex+parents_income+people_cohabiting+size+slenderness+
                           race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+
                           video_time+videogames_time, link = "log")
drop1(mod_negbin_cbcl, test = "Chi") 
#removing AGE_MONTHS
mod_negbin_cbcl = glm.nb(cbcl_score~sex+parents_income+people_cohabiting+size+slenderness+
                           race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+
                           video_time+videogames_time, link = "log")
drop1(mod_negbin_cbcl, test = "Chi")
#removing VIDEOGAMES_TIME
mod_negbin_cbcl = glm.nb(cbcl_score~sex+parents_income+people_cohabiting+size+slenderness+
                           race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+
                           video_time, link = "log")
summary(mod_negbin_cbcl)
#removing SIZE
mod_negbin_cbcl = glm.nb(cbcl_score~sex+parents_income+people_cohabiting+slenderness+
                           race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+
                           video_time, link = "log")
summary(mod_negbin_cbcl)

par(mfrow = c(2, 2))
plot(mod_negbin_cbcl)
par(mfrow = c(1,1))

#MODEL 7 : GLM ZERO INFLATED MODELS
mod_zip_cbcl <- zeroinfl(cbcl_score~age_months+sex+parents_income+people_cohabiting+size+slenderness+
                           race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+
                           video_time+videogames_time+social_activities_time | sex+people_cohabiting+
                           race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY,
                         dist = "poisson", link = "logit")
summary(mod_zip_cbcl)

#model after pca: introduction of size and slenderness in place of original body measures
mod_zinb_cbcl <- zeroinfl(cbcl_score~age_months+sex+parents_income+slenderness+race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+
                            video_time+videogames_time| people_cohabiting + race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY, dist = "negbin", link = "logit")
summary(mod_zinb_cbcl)
#Model interpretation

ordered_factor <- function(fact_var) {
  ord_fact <- factor(fact_var, ordered=TRUE)
  categories <- levels(fact_var)
  n_cat <- length(categories)
  cont <- matrix(0, n_cat, n_cat-1)
  cont[col(cont)<row(cont)] <- 1
  rownames(cont) <- categories
  colnames(cont) <- paste(categories[2:n_cat], categories[1:(n_cat-1)],
                          sep=" vs. ")
  contrasts(ord_fact) <- cont
  return(ord_fact)
}

parents_income = ordered_factor(parents_income)

mod_zinb_agg_bh <- zeroinfl(agg_bh~age_months+sex+parents_income+people_cohabiting+size+slenderness+
                              race_ethnicity+DIMS+DA+SWTD+DOES+SHY+tv_time+
                              social_activities_time | sex+
                              race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY, dist = "negbin", link = "logit")
summary(mod_zinb_agg_bh)

table_construction <- function(n1,n2) {
  race_table = data.frame(1,2,3,4,5,6)
  
  for (i in 1:5) {
    contrasts(race_ethnicity) = contr.treatment(5, base = i)
    contrasts(dataset$race_ethnicity) = contr.treatment(5, base = i)
    mod <- zeroinfl(agg_bh~age_months+sex+parents_income+people_cohabiting+size+slenderness+
                      race_ethnicity+DIMS+DA+SWTD+DOES+SHY+tv_time+
                      social_activities_time | sex+
                      race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY, dist = "negbin", link = "logit")
    coeffs_cbcl = c(head(summary(mod)$coefficients$count[,1],-1), summary(mod)$coefficients$zero[,1])
    confint_cbcl = confint(mod)
    t = cbind (coeffs_cbcl, confint(mod))
    race_table[((i-1)*4 + 1) : ((i-1)*4 + 4),] = rbind(c(t[n1,],t[n2,]),c(t[n1+1,],t[n2+1,]),c(t[n1+2,],t[n2+2,]),c(t[n1+3,],t[n2+3,]))
  }
  #Matrix construction
  
  rt_string_count = c()
  rt_numb_count = c()
  diag = c(1,7,13,19,25)
  j = 0
  for (i in 1:25) {
    if (i %in% diag) {
      rt_string_count[i] = "-"
      rt_numb_count[i] = 0
    }
    else {
      j = j+1
      if (race_table[j,2]*race_table[j,3]>0 || is.na(race_table[j,2]) || is.na(race_table[j,3])) {
        rt_string_count[i] = sprintf("%.3f [%.3f %.3f]", race_table[j,1],race_table[j,2],race_table[j,3])
        rt_numb_count[i] = race_table[j,1]
      }
      else {
        rt_string_count[i] = "n.s."
        rt_numb_count[i] = 0
      }
      
    }
  }
  
  rt_string_zero = c()
  rt_numb_zero = c()
  diag = c(1,7,13,19,25)
  j = 0
  for (i in 1:25) {
    if (i %in% diag) {
      rt_string_zero[i] = "-"
      rt_numb_zero[i] = 0
    }
    else {
      j = j+1
      if (race_table[j,5]*race_table[j,6]>0|| is.na(race_table[j,5]) || is.na(race_table[j,6])) {
        rt_string_zero[i] = sprintf("%.3f [%.3f %.3f]", race_table[j,4],race_table[j,5],race_table[j,6])
        rt_numb_zero[i] = race_table[j,4]
      }
      else {
        rt_string_zero[i] = "n.s."
        rt_numb_zero[i] = 0
      }
      
    }
    
  }
  list_of_matrices = c(rt_string_count, rt_numb_count, rt_string_zero, rt_numb_zero)
  return(list_of_matrices)
}

matrices = table_construction(18,31)
races = c("White", "Black/Afr.Amer.", "Hispanic", "Asian", "Other/Mixed")
rt_string_count = array_reshape(matrices[1:25], c(5,5))
rownames(rt_string_count) = races
colnames(rt_string_count) = races
write.table(rt_string_count, file = "races_cbcl_count.csv", sep = ",", quote = FALSE, row.names = T)
rt_numb_count = array_reshape(matrices[26:50], c(5,5))
rownames(rt_numb_count) = races
colnames(rt_numb_count) = races
write.table(rt_numb_count, file = "races_cbcl_count2.csv", sep = ",", quote = FALSE, row.names = T)
rt_string_zero= array_reshape(matrices[51:75], c(5,5))
rownames(rt_string_zero) = races
colnames(rt_string_zero) = races
write.table(rt_string_zero, file = "races_cbcl_zero.csv", sep = ",", quote = FALSE, row.names = T)
rt_numb_zero= array_reshape(matrices[76:100], c(5,5))
rownames(rt_numb_zero) = races
colnames(rt_numb_zero) = races
write.table(rt_numb_zero, file = "races_cbcl_zero2.csv", sep = ",", quote = FALSE, row.names = T)

#Residual plots
coeffs <- coef(mod_zinb_cbcl,model = "zero")
Z <- model.matrix(mod_zinb_cbcl,model = "zero")
g <- Z %*% coeffs
p <- exp(g) / (1 + exp(g))

coeffs_count <- coef(mod_zinb_cbcl, model = "count")
X <- model.matrix(mod_zinb_cbcl, model = "count")
g2 <- X %*% coeffs_count
mu1 <- exp(g2)

mu <- (1 - p) * mu1

res_cbcl = residuals(mod_zinb_cbcl)
pears_res_cbcl = res_cbcl / sqrt(mu*(1+mu1*p))

sig_features = c(cbcl_score, age_months, people_cohabiting, size, slenderness, DIMS, SBD, DA, SWTD, DOES, SHY, physical_activity, tv_time, video_time, videogames_time) 

p1 = ggplot(dataset, aes(x=age_months, y=res_cbcl)) + 
  geom_point()+
  labs(x="Age months", y = "Raw residuals")+
  theme_classic() 
p2 = ggplot(dataset, aes(x= people_cohabiting, y=res_cbcl)) + 
  geom_point()+
  labs(x="People cohabiting", y = "Raw residuals")+
  theme_classic() 
p3 = ggplot(dataset, aes(x= size, y=res_cbcl)) + 
  geom_point()+
  labs(x="Size", y = "Raw residuals")+
  theme_classic()
p4 = ggplot(dataset, aes(x= slenderness, y=res_cbcl)) + 
  geom_point()+
  labs(x="Slenderness", y = "Raw residuals")+
  theme_classic() 
p5 = ggplot(dataset, aes(x= DIMS, y=res_cbcl)) + 
  geom_point()+
  labs(x="DIMS", y = "Raw residuals")+
  theme_classic() 
p6 = ggplot(dataset, aes(x=SBD, y=res_cbcl)) + 
  geom_point()+
  labs(x="SBD", y = "Raw residuals")+
  theme_classic() 
p7 = ggplot(dataset, aes(x= DA, y=res_cbcl)) + 
  geom_point()+
  labs(x="DA", y = "Raw residuals")+
  theme_classic() 
p8 = ggplot(dataset, aes(x= DOES, y=res_cbcl)) + 
  geom_point()+
  labs(x="DOES", y = "Raw residuals")+
  theme_classic() 
p9 = ggplot(dataset, aes(x= physical_activity, y=res_cbcl)) + 
  geom_point()+
  labs(x="Physical activity", y = "Raw residuals")+
  theme_classic() 
p10 = ggplot(dataset, aes(x=tv_time, y=res_cbcl)) + 
  geom_point()+
  labs(x="TV time", y = "Raw residuals")+
  theme_classic() 
p11 = ggplot(dataset, aes(x=video_time, y=res_cbcl)) + 
  geom_point()+
  labs(x="Video time", y = "Raw residuals")+
  theme_classic() 
p12 = ggplot(dataset, aes(x=videogames_time, y=res_cbcl)) + 
  geom_point()+
  labs(x="Videogames time", y = "Raw residuals")+
  theme_classic() 
grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, nrow = 4, ncol = 3)

ggplot(dataset, aes(x=cbcl_score, y=res_cbcl)) + 
  geom_point()+
  labs(title="Raw residuals for CBCL score with ZINB model",
       x="CBCL score", y = "Raw residuals")+
  theme_classic() 

ggplot(dataset, aes(x=cbcl_score, y=pears_res_cbcl)) + 
  geom_point()+
  labs(title="Pearson residuals for CBCL score with ZINB model",
       x="CBCL score", y = "Pearson residuals")+
  theme_classic() 


#### MODELS FOR ANX_DEPR

#GLM WITH POISSON FAMILY
mod_poisson_anx_depr = glm(anx_depr~age_months+sex+parents_income+people_cohabiting+size+slenderness+
                             race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+video_time+
                             videogames_time+social_activities_time, family = poisson)
summary(mod_poisson_anx_depr)
drop1(mod_poisson_anx_depr)
#removing VIDEOGAMES_TIME
mod_poisson_anx_depr = glm(anx_depr~age_months+sex+parents_income+people_cohabiting+size+slenderness+
                             race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+
                             video_time+social_activities_time, family = poisson)
summary(mod_poisson_anx_depr)
drop1(mod_poisson_anx_depr)
#removing VIDEO_TIME
mod_poisson_anx_depr = glm(anx_depr~age_months+sex+parents_income+people_cohabiting+size+slenderness+
                             race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+
                             social_activities_time, family = poisson)
summary(mod_poisson_anx_depr)
drop1(mod_poisson_anx_depr)
#removing AGE_MONTHS
mod_poisson_anx_depr = glm(anx_depr~sex+parents_income+people_cohabiting+size+slenderness+
                             race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+
                             social_activities_time, family = poisson)
summary(mod_poisson_anx_depr)
drop1(mod_poisson_anx_depr)
#removing SBD
mod_poisson_anx_depr = glm(anx_depr~sex+parents_income+people_cohabiting+size+slenderness+
                             race_ethnicity+DIMS+DA+SWTD+DOES+SHY+physical_activity+tv_time+
                             social_activities_time, family = poisson)
summary(mod_poisson_anx_depr)
drop1(mod_poisson_anx_depr)
#removing TV_TIME
mod_poisson_anx_depr = glm(anx_depr~sex+parents_income+people_cohabiting+size+slenderness+
                             race_ethnicity+DIMS+DA+SWTD+DOES+SHY+physical_activity+
                             social_activities_time, family = poisson)
summary(mod_poisson_anx_depr)

par(mfrow = c(2, 2))
plot(mod_poisson_anx_depr)
par(mfrow = c(1,1))

#GLM WITH POISSON FAMILY WITH OVERDISPERSION
mod_quasipoisson_anx_depr = glm(anx_depr~age_months+sex+parents_income+people_cohabiting+size+slenderness+
                                  race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+video_time+
                                  videogames_time+social_activities_time, family = quasipoisson)
summary(mod_quasipoisson_anx_depr)
drop1(mod_quasipoisson_anx_depr, test = 'F')

#removing AGE_MONTHS, VIDEO_TIME and VIDEOGAMES_TIME
mod_quasipoisson_anx_depr = glm(anx_depr~sex+parents_income+people_cohabiting+size+slenderness+
                                  race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+
                                  social_activities_time, family = quasipoisson)
summary(mod_quasipoisson_anx_depr)
drop1(mod_quasipoisson_anx_depr, test = 'F')
#removing PARENTS_INCOME
mod_quasipoisson_anx_depr = glm(anx_depr~sex+people_cohabiting+size+slenderness+
                                  race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+
                                  social_activities_time, family = quasipoisson)
summary(mod_quasipoisson_anx_depr)
drop1(mod_quasipoisson_anx_depr, test = 'F')
#removing TV_TIME and SBD
mod_quasipoisson_anx_depr = glm(anx_depr~sex+people_cohabiting+size+slenderness+
                                  race_ethnicity+DIMS+DA+SWTD+DOES+SHY+physical_activity+
                                  social_activities_time, family = quasipoisson)
summary(mod_quasipoisson_anx_depr)
drop1(mod_quasipoisson_anx_depr, test = 'F')
#removing PHYSICAL ACTIVITY
mod_quasipoisson_anx_depr = glm(anx_depr~sex+people_cohabiting+size+slenderness+
                                  race_ethnicity+DIMS+DA+SWTD+DOES+SHY+
                                  social_activities_time, family = quasipoisson)
summary(mod_quasipoisson_anx_depr)
drop1(mod_quasipoisson_anx_depr, test = 'F')
#removing SIZE
mod_quasipoisson_anx_depr = glm(anx_depr~sex+people_cohabiting+slenderness+
                                  race_ethnicity+DIMS+DA+SWTD+DOES+SHY+
                                  social_activities_time, family = quasipoisson)
summary(mod_quasipoisson_anx_depr)
drop1(mod_quasipoisson_anx_depr, test = 'F')

par(mfrow = c(2, 2))
plot(mod_quasipoisson_anx_depr)
par(mfrow = c(1,1))


#GLM WITH NEGATIVE BINOMIAL FAMILY
mod_negbin_anx_depr = glm.nb(anx_depr~age_months+sex+parents_income+people_cohabiting+size+slenderness+
                               race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+video_time+
                               videogames_time+social_activities_time, link = "log")
summary(mod_negbin_anx_depr, cor = FALSE)
drop1(mod_negbin_anx_depr, test = "Chi")

#removing AGE_MONTHS, VIDEO_TIME and VIDEOGAMES_TIME
mod_negbin_anx_depr = glm.nb(anx_depr~sex+parents_income+people_cohabiting+size+slenderness+
                               race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+
                               social_activities_time, link = "log")
summary(mod_negbin_anx_depr, cor = FALSE)
drop1(mod_negbin_anx_depr, test = "Chi")
#removing PARENTS_INCOME
mod_negbin_anx_depr = glm.nb(anx_depr~sex+people_cohabiting+size+slenderness+
                               race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+
                               social_activities_time, link = "log")
summary(mod_negbin_anx_depr, cor = FALSE)
drop1(mod_negbin_anx_depr, test = "Chi")
#removing TV_TIME
mod_negbin = glm.nb(anx_depr~sex+people_cohabiting+size+slenderness+
                      race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+
                      social_activities_time, link = "log")
summary(mod_negbin, cor = FALSE)
drop1(mod_negbin, test = "Chi")
#removing SIZE and SLENDERNESS
mod_negbin_anx_depr = glm.nb(anx_depr~sex+people_cohabiting+
                               race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+
                               social_activities_time, link = "log")
summary(mod_negbin_anx_depr, cor = FALSE)
drop1(mod_negbin_anx_depr, test = "Chi")

par(mfrow = c(2, 2))
plot(mod_negbin_anx_depr)
par(mfrow = c(1,1))

#GLM ZERO INFLATED MODELS
mod_zip_anx_depr <- zeroinfl(anx_depr~-1+size+slenderness+
                               race_ethnicity+DIMS+DA+SWTD+DOES+SHY+physical_activity
                             +tv_time| sex+people_cohabiting+race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY
                             +social_activities_time ,
                             dist = "poisson", link = "logit")
summary(mod_zip_anx_depr)

mod_zinb_anx_depr <- zeroinfl(anx_depr~race_ethnicity+DIMS+DA+SWTD+DOES+SHY+physical_activity+
                                social_activities_time | sex+people_cohabiting+
                                race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY, dist = "negbin", link = "logit")
summary(mod_zinb_anx_depr)

lrtest(mod_zip_anx_depr, mod_zinb_anx_depr)

coeffs <- coef(mod_zinb_anx_depr,model = "zero")
Z <- model.matrix(mod_zinb_anx_depr,model = "zero")
g <- Z %*% coeffs
p <- exp(g) / (1 + exp(g))

coeffs_count <- coef(mod_zinb_anx_depr, model = "count")
X <- model.matrix(mod_zinb_anx_depr, model = "count")
g2 <- X %*% coeffs_count
mu1 <- exp(g2)

mu <- (1 - p) * mu1

res_anx_depr = residuals(mod_zinb_anx_depr)
pears_res_anx_depr = res_anx_depr / sqrt(mu*(1+mu1*p))  
plot(res_anx_depr,anx_depr) 
plot(pears_res_anx_depr, anx_depr) #•there's still heteroschedasticity


#### MODELS FOR WITH_DEPR
#GLM WITH POISSON FAMILY
mod_poisson_with_depr = glm(with_depr~age_months+sex+parents_income+people_cohabiting+size+slenderness+
                              race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+video_time+
                              videogames_time+social_activities_time, family = poisson)
summary(mod_poisson_with_depr)
drop1(mod_poisson_with_depr)
#removing RACE_ETHNICITY
mod_poisson_with_depr = glm(with_depr~age_months+sex+parents_income+people_cohabiting+size+slenderness+
                              DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+video_time+
                              videogames_time+social_activities_time, family = poisson)
summary(mod_poisson_with_depr)
drop1(mod_poisson_with_depr)
#removing PEOPLE_COHABITING
mod_poisson_with_depr = glm(with_depr~age_months+sex+parents_income+size+slenderness+
                              DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+video_time+
                              videogames_time+social_activities_time, family = poisson)
summary(mod_poisson_with_depr)
drop1(mod_poisson_with_depr)
#removing TV_TIME
mod_poisson_with_depr = glm(with_depr~age_months+sex+parents_income+size+slenderness+
                              DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+video_time+
                              videogames_time+social_activities_time, family = poisson)
summary(mod_poisson_with_depr)

par(mfrow = c(2, 2))
plot(mod_poisson_with_depr)
par(mfrow = c(1,1))

#GLM WITH POISSON FAMILY WITH OVERDISPERSION
mod_quasipoisson_with_depr = glm(with_depr~age_months+sex+parents_income+people_cohabiting+size+slenderness+
                                   race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+video_time+
                                   videogames_time+social_activities_time, family = quasipoisson)
summary(mod_quasipoisson_with_depr)
drop1(mod_quasipoisson_with_depr, test = 'F')
#removing PEOPLE_COHABITING
mod_quasipoisson_with_depr = glm(with_depr~age_months+sex+parents_income+size+slenderness+
                                   race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+video_time+
                                   videogames_time+social_activities_time, family = quasipoisson)
summary(mod_quasipoisson_with_depr)
drop1(mod_quasipoisson_with_depr, test = 'F')
#removing RACE_ETHNICITY
mod_quasipoisson_with_depr = glm(with_depr~age_months+sex+parents_income+size+slenderness+
                                   +DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+video_time+
                                   videogames_time+social_activities_time, family = quasipoisson)
summary(mod_quasipoisson_with_depr)
drop1(mod_quasipoisson_with_depr, test = 'F')
#removing TV_TIME
mod_quasipoisson_with_depr = glm(with_depr~age_months+sex+parents_income+size+slenderness+
                                   +DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+video_time+
                                   videogames_time+social_activities_time, family = quasipoisson)
summary(mod_quasipoisson_with_depr)
drop1(mod_quasipoisson_with_depr, test = 'F')
#removing SLENDERNESS
mod_quasipoisson_with_depr = glm(with_depr~age_months+sex+parents_income+size+
                                   +DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+video_time+
                                   videogames_time+social_activities_time, family = quasipoisson)
summary(mod_quasipoisson_with_depr)
drop1(mod_quasipoisson_with_depr, test = 'F')
#removing SBD
mod_quasipoisson_with_depr = glm(with_depr~age_months+sex+parents_income+size+
                                   +DIMS+DA+SWTD+DOES+SHY+physical_activity+video_time+
                                   videogames_time+social_activities_time, family = quasipoisson)
summary(mod_quasipoisson_with_depr)
drop1(mod_quasipoisson_with_depr, test = 'F')
#removing DA
mod_quasipoisson_with_depr = glm(with_depr~age_months+sex+parents_income+size+
                                   +DIMS+SWTD+DOES+SHY+physical_activity+video_time+
                                   videogames_time+social_activities_time, family = quasipoisson)
summary(mod_quasipoisson_with_depr)
#removing DA
mod_quasipoisson_with_depr = glm(with_depr~age_months+sex+parents_income+size+
                                   +DIMS+SWTD+DOES+SHY+physical_activity+video_time+
                                   social_activities_time, family = quasipoisson)
summary(mod_quasipoisson_with_depr)

par(mfrow = c(2, 2))
plot(mod_quasipoisson_with_depr)
par(mfrow = c(1,1))

#GLM WITH NEGATIVE BINOMIAL FAMILY
mod_negbin_with_depr = glm.nb(with_depr~age_months+sex+parents_income+people_cohabiting+size+slenderness+
                                race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+video_time+
                                videogames_time+social_activities_time, link = "log")
summary(mod_negbin_with_depr, cor = FALSE)
drop1(mod_negbin_with_depr, test = "Chi")

#removing SBD
mod_negbin_with_depr = glm.nb(with_depr~age_months+sex+parents_income+people_cohabiting+size+slenderness+
                                race_ethnicity+DIMS+DA+SWTD+DOES+SHY+physical_activity+tv_time+video_time+
                                videogames_time+social_activities_time, link = "log")
summary(mod_negbin_with_depr, cor = FALSE)
drop1(mod_negbin_with_depr, test = "Chi")
#removing PEOPLE_COHABITING
mod_negbin_with_depr = glm.nb(with_depr~age_months+sex+parents_income+size+slenderness+
                                race_ethnicity+DIMS+DA+SWTD+DOES+SHY+physical_activity+tv_time+video_time+
                                videogames_time+social_activities_time, link = "log")
summary(mod_negbin_with_depr, cor = FALSE)
drop1(mod_negbin_with_depr, test = "Chi")
#removing TV_TIME
mod_negbin_with_depr = glm.nb(with_depr~age_months+sex+parents_income+size+slenderness+
                                race_ethnicity+DIMS+DA+SWTD+DOES+SHY+physical_activity+video_time+
                                videogames_time+social_activities_time, link = "log")
summary(mod_negbin_with_depr, cor = FALSE)
drop1(mod_negbin_with_depr, test = "Chi")

par(mfrow = c(2, 2))
plot(mod_negbin_with_depr)
par(mfrow = c(1,1))

#GLM ZERO INFLATED MODELS
mod_zip_with_depr <- zeroinfl(with_depr~age_months+sex+parents_income+size+
                                DIMS+SBD+SWTD+DOES+SHY+physical_activity+
                                videogames_time+social_activities_time|age_months+parents_income+
                                DIMS+SBD+DA+SWTD+DOES+physical_activity+video_time+
                                social_activities_time, dist = "poisson", link = "logit")
summary(mod_zip_with_depr)

mod_zinb_with_depr <- zeroinfl(with_depr~age_months+sex+size+
                                 race_ethnicity+DIMS+DA+SWTD+DOES+SHY+physical_activity+video_time+
                                 videogames_time+social_activities_time | size+
                                 SBD+SWTD+DOES+physical_activity, dist = "negbin", link = "logit")
summary(mod_zinb_with_depr)
lrtest(mod_zip_with_depr, mod_zinb_with_depr)


coeffs <- coef(mod_zinb_with_depr,model = "zero")
Z <- model.matrix(mod_zinb_with_depr,model = "zero")
g <- Z %*% coeffs
p <- exp(g) / (1 + exp(g))

coeffs_count <- coef(mod_zinb_with_depr, model = "count")
X <- model.matrix(mod_zinb_with_depr, model = "count")
g2 <- X %*% coeffs_count
mu1 <- exp(g2)

mu <- (1 - p) * mu1

res_with_depr = residuals(mod_zinb_with_depr)
pears_res_with_depr = res_with_depr / sqrt(mu*(1+mu1*p))  
plot(res_with_depr,with_depr) 
plot(pears_res_with_depr, with_depr) #•there's still heteroschedasticity



#### MODELS FOR SOM_COMP

#GLM WITH POISSON FAMILY
mod_poisson_som_comp = glm(som_comp~age_months+sex+parents_income+people_cohabiting+size+slenderness+
                             race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+video_time+
                             videogames_time+social_activities_time, family = poisson)
summary(mod_poisson_som_comp)
drop1(mod_poisson_som_comp)
#removing TV_TIME
mod_poisson_som_comp = glm(som_comp~age_months+sex+parents_income+people_cohabiting+size+slenderness+
                             race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+video_time+
                             videogames_time+social_activities_time, family = poisson)
summary(mod_poisson_som_comp)
drop1(mod_poisson_som_comp)
#removing VIDEOGAMES_TIME
mod_poisson_som_comp = glm(som_comp~age_months+sex+parents_income+people_cohabiting+size+slenderness+
                             race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+video_time+
                             social_activities_time, family = poisson)
summary(mod_poisson_som_comp)
drop1(mod_poisson_som_comp)
#removing PHYSICAL ACTIVITY and AGE_MONTHS
mod_poisson_som_comp = glm(som_comp~sex+parents_income+people_cohabiting+size+slenderness+
                             race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+video_time+
                             social_activities_time, family = poisson)
summary(mod_poisson_som_comp)
drop1(mod_poisson_som_comp)
#removing PHYSICAL ACTIVITY and AGE_MONTHS
mod_poisson_som_comp = glm(som_comp~sex+parents_income+people_cohabiting+size+slenderness+
                             race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+video_time, family = poisson)
summary(mod_poisson_som_comp)
drop1(mod_poisson_som_comp)

par(mfrow = c(2, 2))
plot(mod_poisson_som_comp)
par(mfrow = c(1,1))

#GLM WITH POISSON FAMILY WITH OVERDISPERSION
mod_quasipoisson_som_comp = glm(som_comp~age_months+sex+parents_income+people_cohabiting+size+slenderness+
                                  race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+video_time+
                                  videogames_time+social_activities_time, family = quasipoisson)
summary(mod_quasipoisson_som_comp)
drop1(mod_quasipoisson_som_comp, test = 'F')
#removing TV_TIME, VIDEOGAMES_TIME, SOCIAL_ACTIVITIES_TIME
mod_quasipoisson_som_comp = glm(som_comp~age_months+sex+parents_income+people_cohabiting+size+slenderness+
                                  race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+
                                  physical_activity+video_time, family = quasipoisson)
summary(mod_quasipoisson_som_comp)
drop1(mod_quasipoisson_som_comp, test = 'F')
#removing AGE_MONTHS
mod_quasipoisson_som_comp = glm(som_comp~sex+parents_income+people_cohabiting+size+slenderness+
                                  race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+
                                  physical_activity+video_time, family = quasipoisson)
summary(mod_quasipoisson_som_comp)
drop1(mod_quasipoisson_som_comp, test = 'F')
#removing PHYSICAL_ACTIVITY
mod_quasipoisson_som_comp = glm(som_comp~sex+parents_income+people_cohabiting+size+slenderness+
                                  race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+video_time, family = quasipoisson)
summary(mod_quasipoisson_som_comp)
drop1(mod_quasipoisson_som_comp, test = 'F')

par(mfrow = c(2, 2))
plot(mod_quasipoisson_som_comp)
par(mfrow = c(1,1))

#GLM WITH NEGATIVE BINOMIAL FAMILY
mod_negbin_som_comp = glm.nb(som_comp~age_months+sex+parents_income+people_cohabiting+size+slenderness+
                               race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+video_time+
                               videogames_time+social_activities_time, link = "log")
summary(mod_negbin_som_comp, cor = FALSE)
drop1(mod_negbin_som_comp, test = "Chi")

#removing TV_TIME, VIDEOGAMES_TIME, SOCIAL_ACTIVITIES_TIME
mod_negbin_som_comp = glm.nb(som_comp~age_months+sex+parents_income+people_cohabiting+size+slenderness+
                               race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+video_time, link = "log")
summary(mod_negbin_som_comp, cor = FALSE)
drop1(mod_negbin_som_comp, test = "Chi")
#removing AGE_MONTHS
mod_negbin_som_comp = glm.nb(som_comp~sex+parents_income+people_cohabiting+size+slenderness+
                               race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+video_time, link = "log")
summary(mod_negbin_som_comp, cor = FALSE)
drop1(mod_negbin_som_comp, test = "Chi")
#removing PHYSICAL_ACTIVITY
mod_negbin_som_comp = glm.nb(som_comp~sex+parents_income+people_cohabiting+size+slenderness+
                               race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+video_time, link = "log")
summary(mod_negbin_som_comp, cor = FALSE)
drop1(mod_negbin_som_comp, test = "Chi")

par(mfrow = c(2, 2))
plot(mod_negbin_som_comp)
par(mfrow = c(1,1))

#GLM ZERO INFLATED MODELS
mod_zip_som_comp <- zeroinfl(som_comp~-1+sex+parents_income+people_cohabiting+size+slenderness+
                               race_ethnicity+DIMS+DA+SWTD+DOES+SHY+video_time|sex+people_cohabiting+
                               race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY, dist = "poisson", link = "logit")
summary(mod_zip_som_comp)

mod_zinb_som_comp <- zeroinfl(som_comp~age_months+sex+parents_income+people_cohabiting+size+slenderness+
                                race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+video_time | age_months+people_cohabiting+
                                race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity, dist = "negbin", link = "logit")

summary(mod_zinb_som_comp)
lrtest(mod_zip_som_comp, mod_zinb_som_comp)


coeffs <- coef(mod_zinb_som_comp,model = "zero")
Z <- model.matrix(mod_zinb_som_comp,model = "zero")
g <- Z %*% coeffs
p <- exp(g) / (1 + exp(g))

coeffs_count <- coef(mod_zinb_som_comp, model = "count")
X <- model.matrix(mod_zinb_som_comp, model = "count")
g2 <- X %*% coeffs_count
mu1 <- exp(g2)

mu <- (1 - p) * mu1

res_som_comp = residuals(mod_zinb_som_comp)
pears_res_som_comp = res_som_comp / sqrt(mu*(1+mu1*p))  
plot(res_som_comp,som_comp) 
plot(pears_res_som_comp,som_comp) #•there's still heteroschedasticity

#### MODELS FOR SOCIAL_PR

#GLM WITH POISSON FAMILY
mod_poisson_social_pr = glm(social_pr~age_months+sex+parents_income+people_cohabiting+size+slenderness+
                              race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+video_time+
                              videogames_time+social_activities_time, family = poisson)
summary(mod_poisson_social_pr)
drop1(mod_poisson_social_pr)
#removing SOCIAL_ACTIVITIES_TIME
mod_poisson_social_pr = glm(social_pr~age_months+sex+parents_income+people_cohabiting+size+slenderness+
                              race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+video_time+
                              videogames_time, family = poisson)
summary(mod_poisson_social_pr)
drop1(mod_poisson_social_pr)
#removing VIDEO_TIME
mod_poisson_social_pr = glm(social_pr~age_months+sex+parents_income+people_cohabiting+size+slenderness+
                              race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+
                              videogames_time, family = poisson)
summary(mod_poisson_social_pr)
drop1(mod_poisson_social_pr)

par(mfrow = c(2, 2))
plot(mod_poisson_social_pr)
par(mfrow = c(1,1))

#GLM WITH POISSON FAMILY WITH OVERDISPERSION
mod_quasipoisson_social_pr = glm(social_pr~age_months+sex+parents_income+people_cohabiting+size+slenderness+
                                   race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+video_time+
                                   videogames_time+social_activities_time, family = quasipoisson)
summary(mod_quasipoisson_social_pr)
drop1(mod_quasipoisson_social_pr, test = 'F')
#removing VIDEO_TIME, VIDEOGAMES_TIME, SOCIAL_ACTIVITIES_TIME
mod_quasipoisson_social_pr = glm(social_pr~age_months+sex+parents_income+people_cohabiting+size+slenderness+
                                   race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time, family = quasipoisson)
summary(mod_quasipoisson_social_pr)
drop1(mod_quasipoisson_social_pr, test = 'F')
#removing PEOPLE_COHABITING
mod_quasipoisson_social_pr = glm(social_pr~age_months+sex+parents_income+size+slenderness+
                                   race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time, family = quasipoisson)
summary(mod_quasipoisson_social_pr)
drop1(mod_quasipoisson_social_pr, test = 'F')
#removing SBD
mod_quasipoisson_social_pr = glm(social_pr~age_months+sex+parents_income+size+slenderness+
                                   race_ethnicity+DIMS+DA+SWTD+DOES+SHY+physical_activity+tv_time, family = quasipoisson)
summary(mod_quasipoisson_social_pr)
drop1(mod_quasipoisson_social_pr, test = 'F')

par(mfrow = c(2, 2))
plot(mod_quasipoisson_social_pr)
par(mfrow = c(1,1))

#GLM WITH NEGATIVE BINOMIAL FAMILY
mod_negbin_social_pr = glm.nb(social_pr~age_months+sex+parents_income+people_cohabiting+size+slenderness+
                                race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+video_time+
                                videogames_time+social_activities_time, link = "log")
summary(mod_negbin_social_pr, cor = FALSE)
drop1(mod_negbin_social_pr, test = "Chi")

#removing VIDEO_TIME, VIDEOGAMES_TIME, SOCIAL_ACTIVITIES_TIME
mod_negbin_social_pr = glm.nb(social_pr~age_months+sex+parents_income+people_cohabiting+size+slenderness+
                                race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time, link = "log")
summary(mod_negbin_social_pr, cor = FALSE)
drop1(mod_negbin_social_pr, test = "Chi")

#removing PEOPLE_COHABITING
mod_negbin_social_pr = glm.nb(social_pr~age_months+sex+parents_income+size+slenderness+
                                race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time, link = "log")
summary(mod_negbin_social_pr, cor = FALSE)
drop1(mod_negbin_social_pr, test = "Chi")

par(mfrow = c(2, 2))
plot(mod_negbin_social_pr)
par(mfrow = c(1,1))

#GLM ZERO INFLATED MODELS
mod_zip_social_pr <- zeroinfl(social_pr~age_months+sex+parents_income+size+slenderness+
                                race_ethnicity+DIMS+DA+SWTD+DOES+SHY+physical_activity+tv_time+
                                videogames_time | age_months+parents_income+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity, dist = "poisson", link = "logit")
summary(mod_zip_social_pr)

mod_zinb_social_pr <- zeroinfl(social_pr~age_months+sex+parents_income+people_cohabiting+size+slenderness+
                                 race_ethnicity+DIMS+DA+SWTD+DOES+SHY+physical_activity+tv_time+
                                 videogames_time | age_months+DIMS+SBD+DA+SWTD+DOES+SHY+
                                 physical_activity, dist = "negbin", link = "logit")
summary(mod_zinb_social_pr)
lrtest(mod_zip_social_pr, mod_zinb_social_pr)


coeffs <- coef(mod_zinb_social_pr,model = "zero")
Z <- model.matrix(mod_zinb_social_pr,model = "zero")
g <- Z %*% coeffs
p <- exp(g) / (1 + exp(g))

coeffs_count <- coef(mod_zinb_social_pr, model = "count")
X <- model.matrix(mod_zinb_social_pr, model = "count")
g2 <- X %*% coeffs_count
mu1 <- exp(g2)

mu <- (1 - p) * mu1

res_social_pr = residuals(mod_zinb_social_pr)
pears_res_social_pr = res_social_pr / sqrt(mu*(1+mu1*p))  
plot(res_social_pr,social_pr) 
plot(pears_res_social_pr,social_pr) #•there's still heteroschedasticity


#### MODELS FOR THOUGHT_PR

#GLM WITH POISSON FAMILY
mod_poisson_thought_pr = glm(thought_pr~age_months+sex+parents_income+people_cohabiting+size+slenderness+
                               race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+video_time+
                               videogames_time+social_activities_time, family = poisson)
summary(mod_poisson_thought_pr)
drop1(mod_poisson_thought_pr)
#removing SOCIAL_ACTIVITIES_TIME
mod_poisson_thought_pr = glm(thought_pr~age_months+sex+parents_income+people_cohabiting+size+slenderness+
                               race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+video_time+
                               videogames_time, family = poisson)
summary(mod_poisson_thought_pr)
drop1(mod_poisson_thought_pr)
#removing SBD
mod_poisson_thought_pr = glm(thought_pr~age_months+sex+parents_income+people_cohabiting+size+slenderness+
                               race_ethnicity+DIMS+DA+SWTD+DOES+SHY+physical_activity+tv_time+video_time+
                               videogames_time, family = poisson)
summary(mod_poisson_thought_pr)
drop1(mod_poisson_thought_pr)
#removing AGE_MONTHS
mod_poisson_thought_pr = glm(thought_pr~sex+parents_income+people_cohabiting+size+slenderness+
                               race_ethnicity+DIMS+DA+SWTD+DOES+SHY+physical_activity+tv_time+video_time+
                               videogames_time, family = poisson)
summary(mod_poisson_thought_pr)
drop1(mod_poisson_thought_pr)
#removing PHYSICAL_ACTIVITY
mod_poisson_thought_pr = glm(thought_pr~sex+parents_income+people_cohabiting+size+slenderness+
                               race_ethnicity+DIMS+DA+SWTD+DOES+SHY+tv_time+video_time+
                               videogames_time, family = poisson)
summary(mod_poisson_thought_pr)
drop1(mod_poisson_thought_pr)
#removing VIDEO_TIME
mod_poisson_thought_pr = glm(thought_pr~sex+parents_income+people_cohabiting+size+slenderness+
                               race_ethnicity+DIMS+DA+SWTD+DOES+SHY+tv_time+
                               videogames_time, family = poisson)
summary(mod_poisson_thought_pr)
drop1(mod_poisson_thought_pr)


par(mfrow = c(2, 2))
plot(mod_poisson_thought_pr)
par(mfrow = c(1,1))

#GLM WITH POISSON FAMILY WITH OVERDISPERSION
mod_quasipoisson_thought_pr = glm(thought_pr~age_months+sex+parents_income+people_cohabiting+size+slenderness+
                                    race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+video_time+
                                    videogames_time+social_activities_time, family = quasipoisson)
summary(mod_quasipoisson_thought_pr)
drop1(mod_quasipoisson_thought_pr, test = 'F')
#removing VIDEO_TIME, SOCIAL_ACTIVITIES_TIME
mod_quasipoisson_thought_pr = glm(thought_pr~age_months+sex+parents_income+people_cohabiting+size+slenderness+
                                    race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+videogames_time, family = quasipoisson)
summary(mod_quasipoisson_thought_pr)
drop1(mod_quasipoisson_thought_pr, test = 'F')
#removing PHYSICAL_ACTIVITY
mod_quasipoisson_thought_pr = glm(thought_pr~age_months+sex+parents_income+people_cohabiting+size+slenderness+
                                    race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+tv_time+videogames_time, family = quasipoisson)
summary(mod_quasipoisson_thought_pr)
drop1(mod_quasipoisson_thought_pr, test = 'F')
#removing SBD and AGE_MONTHS
mod_quasipoisson_thought_pr = glm(thought_pr~sex+parents_income+people_cohabiting+size+slenderness+
                                    race_ethnicity+DIMS+DA+SWTD+DOES+SHY+tv_time+videogames_time, family = quasipoisson)
summary(mod_quasipoisson_thought_pr)
drop1(mod_quasipoisson_thought_pr, test = 'F')

par(mfrow = c(2, 2))
plot(mod_quasipoisson_thought_pr)
par(mfrow = c(1,1))

#GLM WITH NEGATIVE BINOMIAL FAMILY
mod_negbin_thought_pr = glm.nb(thought_pr~age_months+sex+parents_income+people_cohabiting+size+slenderness+
                                 race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+video_time+
                                 videogames_time+social_activities_time, link = "log")
summary(mod_negbin_thought_pr, cor = FALSE)
drop1(mod_negbin_thought_pr, test = "Chi")

#removing AGE_MONTHS
mod_negbin_thought_pr = glm.nb(thought_pr~sex+parents_income+people_cohabiting+size+slenderness+
                                 race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+video_time+
                                 videogames_time+social_activities_time, link = "log")
summary(mod_negbin_thought_pr, cor = FALSE)
drop1(mod_negbin_thought_pr, test = "Chi")

#removing SOCIAL_ACTIVITIES_TIME
mod_negbin_thought_pr = glm.nb(thought_pr~sex+parents_income+people_cohabiting+size+slenderness+
                                 race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+video_time+
                                 videogames_time, link = "log")
summary(mod_negbin_thought_pr, cor = FALSE)
drop1(mod_negbin_thought_pr, test = "Chi")

#removing PHYSICAL_ACTIVITY
mod_negbin_thought_pr = glm.nb(thought_pr~sex+parents_income+people_cohabiting+size+slenderness+
                                 race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+tv_time+video_time+
                                 videogames_time, link = "log")
summary(mod_negbin_thought_pr, cor = FALSE)
drop1(mod_negbin_thought_pr, test = "Chi")
#removing VIDEO_TIME
mod_negbin_thought_pr = glm.nb(thought_pr~sex+parents_income+people_cohabiting+size+slenderness+
                                 race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+tv_time+
                                 videogames_time, link = "log")
summary(mod_negbin_thought_pr, cor = FALSE)
drop1(mod_negbin_thought_pr, test = "Chi")

par(mfrow = c(2, 2))
plot(mod_negbin_thought_pr)
par(mfrow = c(1,1))

#GLM ZERO INFLATED MODELS
mod_zip_thought_pr <- zeroinfl(thought_pr~sex+parents_income+size+slenderness+
                                 race_ethnicity+DIMS+DA+SWTD+DOES+SHY+physical_activity+tv_time+
                                 videogames_time | sex+people_cohabiting+
                                 race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY, dist = "poisson", link = "logit")
summary(mod_zip_thought_pr)

mod_zinb_thought_pr <- zeroinfl(thought_pr~sex+parents_income+size+slenderness+
                                  race_ethnicity+DIMS+DA+SWTD+DOES+SHY+physical_activity+tv_time+
                                  videogames_time | sex+people_cohabiting+
                                  race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY, dist = "negbin", link = "logit")
summary(mod_zinb_thought_pr)
lrtest(mod_zip_thought_pr, mod_zinb_thought_pr)


coeffs <- coef(mod_zinb_thought_pr,model = "zero")
Z <- model.matrix(mod_zinb_thought_pr,model = "zero")
g <- Z %*% coeffs
p <- exp(g) / (1 + exp(g))

coeffs_count <- coef(mod_zinb_thought_pr, model = "count")
X <- model.matrix(mod_zinb_thought_pr, model = "count")
g2 <- X %*% coeffs_count
mu1 <- exp(g2)

mu <- (1 - p) * mu1

res_thought_pr = residuals(mod_zinb_thought_pr)
pears_res_thought_pr = res_thought_pr / sqrt(mu*(1+mu1*p))  

feat = DIMS
name_feat = deparse(substitute(feat))
ggplot(dataset, aes(x=feat, y=res_cbcl)) + 
  geom_point()+
  labs(title="Raw residuals with ZINB model",
       x=name_feat, y = "Raw residuals")+
  theme_classic() 


ggplot(dataset, aes(x=feat, y=pears_res_cbcl)) + 
  geom_point()+
  labs(title="Pearson residuals for CBCL score with ZINB model",
       x=name_feat, y = "Pearson residuals")+
  theme_classic() 



ggplot(dataset, aes(x=thought_pr, y=res_thought_pr)) + 
  geom_point()+
  labs(title="Raw residuals for thought problem score with ZINB model",
       x="Thought problem score", y = "Raw residuals")+
  theme_classic() 

ggplot(dataset, aes(x=thought_pr, y=pears_res_thought_pr)) + 
  geom_point()+
  labs(title="Pearson residuals for thought problem score with ZINB model",
       x="Thought problem score", y = "Pearson residuals")+
  theme_classic() 

#### MODELS FOR ATT_PR

#GLM WITH POISSON FAMILY
mod_poisson_att_pr = glm(att_pr~age_months+sex+parents_income+people_cohabiting+size+slenderness+
                           race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+video_time+
                           videogames_time+social_activities_time, family = poisson)
summary(mod_poisson_att_pr)
drop1(mod_poisson_att_pr)
#removing SOCIAL_ACTIVITIES_TIME
mod_poisson_att_pr = glm(att_pr~age_months+sex+parents_income+people_cohabiting+size+slenderness+
                           race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+video_time+
                           videogames_time, family = poisson)
summary(mod_poisson_att_pr)
drop1(mod_poisson_att_pr)
#removing SBD
mod_poisson_att_pr = glm(att_pr~age_months+sex+parents_income+people_cohabiting+size+slenderness+
                           race_ethnicity+DIMS+DA+SWTD+DOES+SHY+physical_activity+tv_time+video_time+
                           videogames_time, family = poisson)
summary(mod_poisson_att_pr)
drop1(mod_poisson_att_pr)


par(mfrow = c(2, 2))
plot(mod_poisson_att_pr)
par(mfrow = c(1,1))

#GLM WITH POISSON FAMILY WITH OVERDISPERSION
mod_quasipoisson_att_pr = glm(att_pr~age_months+sex+parents_income+people_cohabiting+size+slenderness+
                                race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+video_time+
                                videogames_time+social_activities_time, family = quasipoisson)
summary(mod_quasipoisson_att_pr)
drop1(mod_quasipoisson_att_pr, test = 'F')
#removing SOCIAL_ACTIVITIES_TIME
mod_quasipoisson_att_pr = glm(att_pr~age_months+sex+parents_income+people_cohabiting+size+slenderness+
                                race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+videogames_time+video_time, family = quasipoisson)
summary(mod_quasipoisson_att_pr)
drop1(mod_quasipoisson_att_pr, test = 'F')
#removing SBD
mod_quasipoisson_att_pr = glm(att_pr~age_months+sex+parents_income+people_cohabiting+size+slenderness+
                                race_ethnicity+DIMS+DA+SWTD+DOES+SHY+physical_activity+tv_time+videogames_time+video_time, family = quasipoisson)
summary(mod_quasipoisson_att_pr)
drop1(mod_quasipoisson_att_pr, test = 'F')
#removing AGE_MONTHS
mod_quasipoisson_att_pr = glm(att_pr~sex+parents_income+people_cohabiting+size+slenderness+
                                race_ethnicity+DIMS+DA+SWTD+DOES+SHY+physical_activity+tv_time+videogames_time+video_time, family = quasipoisson)
summary(mod_quasipoisson_att_pr)
drop1(mod_quasipoisson_att_pr, test = 'F')

par(mfrow = c(2, 2))
plot(mod_quasipoisson_att_pr)
par(mfrow = c(1,1))

#GLM WITH NEGATIVE BINOMIAL FAMILY
mod_negbin_att_pr = glm.nb(att_pr~age_months+sex+parents_income+people_cohabiting+size+slenderness+
                             race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+video_time+
                             videogames_time+social_activities_time, link = "log")
summary(mod_negbin_att_pr, cor = FALSE)
drop1(mod_negbin_att_pr, test = "Chi")

#removing AGE_MONTHS
mod_negbin_att_pr = glm.nb(att_pr~sex+parents_income+people_cohabiting+size+slenderness+
                             race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+video_time+
                             videogames_time+social_activities_time, link = "log")
summary(mod_negbin_att_pr, cor = FALSE)
drop1(mod_negbin_att_pr, test = "Chi")

#removing SOCIAL_ACTIVITIES_TIME
mod_negbin_att_pr = glm.nb(att_pr~sex+parents_income+people_cohabiting+size+slenderness+
                             race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+video_time+
                             videogames_time, link = "log")
summary(mod_negbin_att_pr, cor = FALSE)
drop1(mod_negbin_att_pr, test = "Chi")


par(mfrow = c(2, 2))
plot(mod_negbin_att_pr)
par(mfrow = c(1,1))

#GLM ZERO INFLATED MODELS
mod_zip_att_pr <- zeroinfl(att_pr~sex+parents_income+people_cohabiting+size+slenderness+
                             race_ethnicity+DIMS+DA+SWTD+DOES+SHY+physical_activity+tv_time+video_time+
                             videogames_time | age_months+sex+people_cohabiting+size+
                             race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+
                             videogames_time, dist = "poisson", link = "logit")
summary(mod_zip_att_pr)

mod_zinb_att_pr <- zeroinfl(att_pr~sex+parents_income+people_cohabiting+size+slenderness+
                              race_ethnicity+DIMS+DA+SWTD+DOES+SHY+physical_activity+tv_time+video_time+
                              videogames_time | age_months+sex+people_cohabiting+
                              DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity, dist = "negbin", link = "logit")
summary(mod_zinb_att_pr)

lrtest(mod_zip_att_pr, mod_zinb_att_pr)

coeffs <- coef(mod_zinb_att_pr,model = "zero")
Z <- model.matrix(mod_zinb_att_pr,model = "zero")
g <- Z %*% coeffs
p <- exp(g) / (1 + exp(g))

coeffs_count <- coef(mod_zinb_att_pr, model = "count")
X <- model.matrix(mod_zinb_att_pr, model = "count")
g2 <- X %*% coeffs_count
mu1 <- exp(g2)

mu <- (1 - p) * mu1

res_att_pr = residuals(mod_zinb_att_pr)
pears_res_att_pr = res_att_pr / sqrt(mu*(1+mu1*p))  
plot(res_att_pr,att_pr) 
plot(pears_res_att_pr,att_pr) #•there's still heteroschedasticity


#### MODELS FOR RULE_BR_BH

#GLM WITH POISSON FAMILY
mod_poisson_rule_br_bh = glm(rule_br_bh~age_months+sex+parents_income+people_cohabiting+size+slenderness+
                               race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+video_time+
                               videogames_time+social_activities_time, family = poisson)
summary(mod_poisson_rule_br_bh)
drop1(mod_poisson_rule_br_bh)
#removing SIZE
mod_poisson_rule_br_bh = glm(rule_br_bh~age_months+sex+parents_income+people_cohabiting+slenderness+
                               race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+video_time+
                               videogames_time+social_activities_time, family = poisson)
summary(mod_poisson_rule_br_bh)
drop1(mod_poisson_rule_br_bh)
#removing VIDEO_TIME and VIDEOGAMES_TIME
mod_poisson_rule_br_bh = glm(rule_br_bh~age_months+sex+parents_income+people_cohabiting+slenderness+
                               race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+
                               tv_time++social_activities_time, family = poisson)
summary(mod_poisson_rule_br_bh)
drop1(mod_poisson_rule_br_bh)
#removing PHYSICAL_ACTIVITY
mod_poisson_rule_br_bh = glm(rule_br_bh~age_months+sex+parents_income+people_cohabiting+slenderness+
                               race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+
                               tv_time++social_activities_time, family = poisson)
summary(mod_poisson_rule_br_bh)
drop1(mod_poisson_rule_br_bh)

par(mfrow = c(2, 2))
plot(mod_poisson)
par(mfrow = c(1,1))

#GLM WITH POISSON FAMILY WITH OVERDISPERSION
mod_quasipoisson_rule_br_bh = glm(rule_br_bh~age_months+sex+parents_income+people_cohabiting+size+slenderness+
                                    race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+video_time+
                                    videogames_time+social_activities_time, family = quasipoisson)
summary(mod_quasipoisson_rule_br_bh)
drop1(mod_quasipoisson_rule_br_bh, test = 'F')
#removing SIZE and SLENDERNESS
mod_quasipoisson_rule_br_bh = glm(rule_br_bh~age_months+sex+parents_income+people_cohabiting+
                                    race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+video_time+
                                    videogames_time+social_activities_time, family = quasipoisson)
summary(mod_quasipoisson_rule_br_bh)
drop1(mod_quasipoisson_rule_br_bh, test = 'F')
#removing VIDEO_TIME and VIDEOGAMES_TIME
mod_quasipoisson_rule_br_bh = glm(rule_br_bh~age_months+sex+parents_income+people_cohabiting+
                                    race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+
                                    social_activities_time, family = quasipoisson)
summary(mod_quasipoisson_rule_br_bh)
drop1(mod_quasipoisson_rule_br_bh, test = 'F')
#removing SHY and PHYSICAL ACTIVITY
mod_quasipoisson_rule_br_bh = glm(rule_br_bh~age_months+sex+parents_income+people_cohabiting+
                                    race_ethnicity+DIMS+SBD+DA+SWTD+DOES+tv_time+
                                    social_activities_time, family = quasipoisson)
summary(mod_quasipoisson_rule_br_bh)
drop1(mod_quasipoisson_rule_br_bh, test = 'F')

par(mfrow = c(2, 2))
plot(mod_quasipoisson)
par(mfrow = c(1,1))

#GLM WITH NEGATIVE BINOMIAL FAMILY
mod_negbin_rule_br_bh = glm.nb(rule_br_bh~age_months+sex+parents_income+people_cohabiting+size+slenderness+
                                 race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+video_time+
                                 videogames_time+social_activities_time, link = "log")
summary(mod_negbin_rule_br_bh, cor = FALSE)
drop1(mod_negbin_rule_br_bh, test = "Chi")

#removing SIZE and SLENDERNESS
mod_negbin_rule_br_bh = glm.nb(rule_br_bh~age_months+sex+parents_income+people_cohabiting+
                                 race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+video_time+
                                 videogames_time+social_activities_time, link = "log")
summary(mod_negbin_rule_br_bh, cor = FALSE)
drop1(mod_negbin_rule_br_bh, test = "Chi")

#removing VIDEO_TIME, VIDEOGAMES_TIME
mod_negbin_rule_br_bh = glm.nb(rule_br_bh~age_months+sex+parents_income+people_cohabiting+
                                 race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+
                                 social_activities_time, link = "log")
summary(mod_negbin_rule_br_bh, cor = FALSE)
drop1(mod_negbin_rule_br_bh, test = "Chi")
#removing PHYSICAL_ACTIVITY
mod_negbin_rule_br_bh = glm.nb(rule_br_bh~age_months+sex+parents_income+people_cohabiting+
                                 race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+tv_time+
                                 social_activities_time, link = "log")
summary(mod_negbin_rule_br_bh, cor = FALSE)
drop1(mod_negbin_rule_br_bh, test = "Chi")

par(mfrow = c(2, 2))
plot(mod_negbin_rule_br_bh)
par(mfrow = c(1,1))

#GLM ZERO INFLATED MODELS
mod_zip_rule_br_bh <- zeroinfl(rule_br_bh~sex+parents_income+people_cohabiting+
                                 race_ethnicity+DIMS+DA+SWTD+DOES+physical_activity+tv_time
                               +social_activities_time | age_months+sex+parents_income+people_cohabiting+
                                 race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+video_time+
                                 social_activities_time, dist = "poisson", link = "logit")
summary(mod_zip_rule_br_bh)

mod_zinb_rule_br_bh <- zeroinfl(rule_br_bh~age_months+sex+parents_income+people_cohabiting+
                                  race_ethnicity+DIMS+DA+SWTD+DOES+tv_time+
                                  social_activities_time | age_months+sex+people_cohabiting+
                                  DIMS+SBD+DA+SWTD+DOES+SHY, dist = "negbin", link = "logit")
summary(mod_zinb_rule_br_bh)

lrtest(mod_zip_rule_br_bh, mod_zinb_rule_br_bh)


coeffs <- coef(mod_zinb_rule_br_bh,model = "zero")
Z <- model.matrix(mod_zinb_rule_br_bh,model = "zero")
g <- Z %*% coeffs
p <- exp(g) / (1 + exp(g))

coeffs_count <- coef(mod_zinb_rule_br_bh, model = "count")
X <- model.matrix(mod_zinb_rule_br_bh, model = "count")
g2 <- X %*% coeffs_count
mu1 <- exp(g2)

mu <- (1 - p) * mu1

res_rule_br_bh = residuals(mod_zinb_rule_br_bh)
pears_res_rule_br_bh = res_rule_br_bh / sqrt(mu*(1+mu1*p))  
plot(res_rule_br_bh,rule_br_bh) 
plot(pears_res_rule_br_bh,rule_br_bh) #•there's still heteroschedasticity

#### MODELS FOR AGG_BH

#GLM WITH POISSON FAMILY
mod_poisson_agg_bh = glm(agg_bh~age_months+sex+parents_income+people_cohabiting+size+slenderness+
                           race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+video_time+
                           videogames_time+social_activities_time, family = poisson)
summary(mod_poisson_agg_bh)
drop1(mod_poisson_agg_bh)
#removing VIDEO_TIME
mod_poisson_agg_bh = glm(agg_bh~age_months+sex+parents_income+people_cohabiting+size+slenderness+
                           race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+
                           videogames_time+social_activities_time, family = poisson)
summary(mod_poisson_agg_bh)
drop1(mod_poisson_agg_bh)
#removing SBD
mod_poisson_agg_bh = glm(agg_bh~age_months+sex+parents_income+people_cohabiting+size+slenderness+
                           race_ethnicity+DIMS+DA+SWTD+DOES+SHY+physical_activity+tv_time+
                           videogames_time+social_activities_time, family = poisson)
summary(mod_poisson_agg_bh)
drop1(mod_poisson_agg_bh)
#removing VIDEOGAMES_TIME
mod_poisson_agg_bh = glm(agg_bh~age_months+sex+parents_income+people_cohabiting+size+slenderness+
                           race_ethnicity+DIMS+DA+SWTD+DOES+SHY+physical_activity+tv_time+
                           social_activities_time, family = poisson)
summary(mod_poisson_agg_bh)
drop1(mod_poisson_agg_bh)

par(mfrow = c(2, 2))
plot(mod_poisson_agg_bh)
par(mfrow = c(1,1))

#GLM WITH POISSON FAMILY WITH OVERDISPERSION
mod_quasipoisson_agg_bh = glm(agg_bh~age_months+sex+parents_income+people_cohabiting+size+slenderness+
                                race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+video_time+
                                videogames_time+social_activities_time, family = quasipoisson)
summary(mod_quasipoisson_agg_bh)
drop1(mod_quasipoisson_agg_bh, test = 'F')
#removing VIDEO_TIME and VIDEOGAMES_TIME
mod_quasipoisson_agg_bh = glm(agg_bh~age_months+sex+parents_income+people_cohabiting+size+slenderness+
                                race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+
                                social_activities_time, family = quasipoisson)
summary(mod_quasipoisson_agg_bh)
drop1(mod_quasipoisson_agg_bh, test = 'F')
#removing SBD and PHYSICAL_ACTIVITY
mod_quasipoisson_agg_bh = glm(agg_bh~age_months+sex+parents_income+people_cohabiting+size+slenderness+
                                race_ethnicity+DIMS+DA+SWTD+DOES+SHY+tv_time+
                                social_activities_time, family = quasipoisson)
summary(mod_quasipoisson_agg_bh)
drop1(mod_quasipoisson_agg_bh, test = 'F')


par(mfrow = c(2, 2))
plot(mod_quasipoisson_agg_bh)
par(mfrow = c(1,1))

#GLM WITH NEGATIVE BINOMIAL FAMILY
mod_negbin_agg_bh = glm.nb(agg_bh~age_months+sex+parents_income+people_cohabiting+size+slenderness+
                             race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+video_time+
                             videogames_time+social_activities_time, link = "log")
summary(mod_negbin_agg_bh, cor = FALSE)
drop1(mod_negbin_agg_bh, test = "Chi")

#removing VIDEO_TIME, VIDEOGAMES_TIME
mod_negbin_agg_bh = glm.nb(agg_bh~age_months+sex+parents_income+people_cohabiting+size+slenderness+
                             race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+physical_activity+tv_time+
                             social_activities_time, link = "log")
summary(mod_negbin_agg_bh, cor = FALSE)
drop1(mod_negbin_agg_bh, test = "Chi")

#removing PHYSICAL_ACTIVITY
mod_negbin_agg_bh = glm.nb(agg_bh~age_months+sex+parents_income+people_cohabiting+size+slenderness+
                             race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+tv_time+
                             social_activities_time, link = "log")
summary(mod_negbin_agg_bh, cor = FALSE)
drop1(mod_negbin_agg_bh, test = "Chi")


par(mfrow = c(2, 2))
plot(mod_negbin_agg_bh)
par(mfrow = c(1,1))

#GLM ZERO INFLATED MODELS
mod_zip_agg_bh <- zeroinfl(agg_bh~age_months+sex+parents_income+people_cohabiting+size+slenderness+
                             race_ethnicity+DIMS+DA+SWTD+DOES+SHY+tv_time+
                             videogames_time+social_activities_time | age_months+sex+parents_income+people_cohabiting+
                             race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+tv_time, dist = "poisson", link = "logit")
summary(mod_zip)

mod_zinb_agg_bh <- zeroinfl(agg_bh~age_months+sex+parents_income+people_cohabiting+size+slenderness+
                              race_ethnicity+DIMS+DA+SWTD+DOES+SHY+tv_time+
                              videogames_time+social_activities_time | sex+parents_income+
                              race_ethnicity+DIMS+SBD+DA+SWTD+DOES+SHY+tv_time+
                              videogames_time, dist = "negbin", link = "logit")
summary(mod_zinb_agg_bh)

lrtest(mod_zip_agg_bh, mod_zinb_agg_bh)

coeffs <- coef(mod_zinb_agg_bh,model = "zero")
Z <- model.matrix(mod_zinb_agg_bh,model = "zero")
g <- Z %*% coeffs
p <- exp(g) / (1 + exp(g))

coeffs_count <- coef(mod_zinb_agg_bh, model = "count")
X <- model.matrix(mod_zinb_agg_bh, model = "count")
g2 <- X %*% coeffs_count
mu1 <- exp(g2)

mu <- (1 - p) * mu1

res_agg_bh = residuals(mod_zinb_agg_bh)
pears_res_agg_bh = res_agg_bh / sqrt(mu*(1+mu1*p))  
plot(res_agg_bh,agg_bh) 
plot(pears_res_agg_bh,agg_bh) #•there's still heteroschedasticity



### Model selection

# RsquaredAdj = c(
# summary(simple_reg)$adj.r.squared,
# summary(simple_reg_std)$adj.r.squared,
# summary(interaction_reg)$adj.r.squared
# )
# Rsquared = c(
# summary(simple_reg)$r.squared,
# summary(simple_reg_std)$r.squared,
# summary(interaction_reg)$r.squared
# )
# Aic = AIC(simple_reg,simple_reg_std,interaction_reg)[,2]
# ModelChoice = data.frame(
# Mod=c("Simple Regression","Simple Regression - Std Dataset","Regression with interaction"),
# RsquaredAdj=RsquaredAdj,
# Rsquared=Rsquared,
# Aic = Aic
# )
# ModelChoice # il nuovo modello è il migliore
# write.table(ModelChoice, file = "ModelChoice.txt", sep = ",", quote = FALSE, row.names = T)


model_selection <- function(mod,mod_poisson,mod_quasipoisson,mod_negbin,mod_zip,mod_zinb) {
  fit_poiss = predict(mod_poisson, newdata = dataset)
  fit_qpoiss = predict(mod_quasipoisson, newdata = dataset)
  fit_nb = predict(mod_negbin, newdata = dataset)
  fit_zip = predict(mod_zip, newdata = dataset)
  fit_zinb = predict(mod_zinb, newdata = dataset)
  r_corr = c(
    cor(fit_poiss, cbcl_score, method = "pearson"),
    cor(fit_qpoiss, cbcl_score, method = "pearson"),
    cor(fit_nb, cbcl_score, method = "pearson"),
    cor(fit_zip, cbcl_score, method = "pearson"),
    cor(fit_zinb, cbcl_score, method = "pearson")
  )
  p_corr = c(
    cor(fit_poiss, cbcl_score, method = "spearman"),
    cor(fit_qpoiss, cbcl_score, method = "spearman"),
    cor(fit_nb, cbcl_score, method = "spearman"),
    cor(fit_zip, cbcl_score, method = "spearman"),
    cor(fit_zinb, cbcl_score, method = "spearman")
  )
  MSE = c(
    MSE(cbcl_score,fit_poiss),
    MSE(cbcl_score,fit_qpoiss),
    MSE(cbcl_score,fit_nb),
    MSE(cbcl_score,fit_zip),
    MSE(cbcl_score,fit_zinb)
  )
  mae = c(
    MAE(cbcl_score,fit_poiss),
    MAE(cbcl_score,fit_qpoiss),
    MAE(cbcl_score,fit_nb),
    MAE(cbcl_score,fit_zip),
    MAE(cbcl_score,fit_zinb)
  )
  Aic = AIC(mod_poisson,mod_quasipoisson,mod_negbin,mod_zip,mod_zinb)
  ModelSelection = data.frame(
    Mod=c("Poisson", "Poisson with overdispersion", "Negative Binomial", "Zero Inflated Poisson", "Zero Inflated Negative Binomial"),
    Pearson_correlation = r_corr,
    Spearman_correlation = p_corr,
    MSE = MSE,
    MAE = mae,
    AIC = Aic[,2],
    Df = Aic[,1]
  )
  colnames(ModelSelection) = c(paste("Model for", mod), "Pearson corr.", "Spearman corr.", "MSE", "MAE", "AIC", "Df")
  return(ModelSelection)
}

ModelSelection_cbcl = model_selection("cbcl_score",mod_poisson_cbcl,mod_quasipoisson_cbcl,mod_negbin_cbcl,mod_zip_cbcl,mod_zinb_cbcl)
write.table(ModelSelection_cbcl, file = "ModelSelection_cbcl.csv", sep = ",", quote = FALSE)
ModelSelection_anx_depr = model_selection("anx_depr",mod_poisson_anx_depr,mod_quasipoisson_anx_depr,mod_negbin_anx_depr,mod_zip_anx_depr,mod_zinb_anx_depr)
write.table(ModelSelection_anx_depr, file = "ModelSelection_anx_depr.csv", sep = ",", quote = FALSE)
ModelSelection_with_depr = model_selection("with_depr",mod_poisson_with_depr,mod_quasipoisson_with_depr,mod_negbin_with_depr,mod_zip_with_depr,mod_zinb_with_depr)
write.table(ModelSelection_with_depr, file = "ModelSelection_with_depr.csv", sep = ",", quote = FALSE)
ModelSelection_som_comp = model_selection("som_comp",mod_poisson_som_comp,mod_quasipoisson_som_comp,mod_negbin_som_comp,mod_zip_som_comp,mod_zinb_som_comp)
write.table(ModelSelection_som_comp, file = "ModelSelection_som_comp.csv", sep = ",", quote = FALSE)
ModelSelection_social_pr = model_selection("social_pr",mod_poisson_social_pr,mod_quasipoisson_social_pr,mod_negbin_social_pr,mod_zip_social_pr,mod_zinb_social_pr)
write.table(ModelSelection_social_pr, file = "ModelSelection_social_pr.csv", sep = ",", quote = FALSE)
ModelSelection_thought_pr = model_selection("thought_pr",mod_poisson_thought_pr,mod_quasipoisson_thought_pr,mod_negbin_thought_pr,mod_zip_thought_pr,mod_zinb_thought_pr)
write.table(ModelSelection_thought_pr, file = "ModelSelection_thought_pr.csv", sep = ",", quote = FALSE)
ModelSelection_att_pr = model_selection("att_pr",mod_poisson_att_pr,mod_quasipoisson_att_pr,mod_negbin_att_pr,mod_zip_att_pr,mod_zinb_att_pr)
write.table(ModelSelection_att_pr, file = "ModelSelection_att_pr.csv", sep = ",", quote = FALSE)
ModelSelection_rule_br_bh = model_selection("rule_br_bh",mod_poisson_rule_br_bh,mod_quasipoisson_rule_br_bh,mod_negbin_rule_br_bh,mod_zip_rule_br_bh,mod_zinb_rule_br_bh)
write.table(ModelSelection_rule_br_bh, file = "ModelSelection_rule_br_bh.csv", sep = ",", quote = FALSE)
ModelSelection_agg_bh = model_selection("agg_bh",mod_poisson_agg_bh,mod_quasipoisson_agg_bh,mod_negbin_agg_bh,mod_zip_agg_bh,mod_zinb_agg_bh)
write.table(ModelSelection_agg_bh, file = "ModelSelection_agg_bh.csv", sep = ",", quote = FALSE)

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


dataset_year1 = dataset[dataset$eventname == "1_year_follow_up_y_arm_1",]
dataset_year0 = dataset[dataset$eventname == "baseline_year_1_arm_1",]
diff_dataset = dataset_year1 - dataset_year0




