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
library(MuMIn)
library(gridExtra)
#Setting working directory
wd <- "C:/Users/Marco Bottino/Desktop/Marco/Poli/Tesi Magistrale/ABCD"
setwd(wd)

dataset = read.csv("data_merged_cleaned_pca.csv",header = TRUE)
attach(dataset)

dataset$race_ethnicity = as.factor(dataset$race_ethnicity)
dataset$parents_income = as.factor(dataset$parents_income)

detach(dataset)
attach(dataset)

surface_data = read.csv("Datasets/CorticalMeasuresABCD_SurfAvg_clean.csv",header = TRUE)
region_names = read.csv("list_regions.csv",header = FALSE)[1:70,]
thickness_data = read.csv("Datasets/CorticalMeasuresABCD_ThickAvg_clean.csv",header = TRUE)



surface_data$subjectkey = as.factor(paste(substr(surface_data$SubjID,5,8), "_", substr(surface_data$SubjID,9,19), sep = ""))
surface_data$eventname = substr(surface_data$SubjID,21,26)
thickness_data$subjectkey = as.factor(paste(substr(thickness_data$SubjID,5,8), "_", substr(thickness_data$SubjID,9,19), sep = ""))
thickness_data$eventname = substr(thickness_data$SubjID,21,26)


convert_eventname = function(x) {
  if (x == "run-01") {return ("1_year_follow_up_y_arm_1")} 
  else {return("baseline_year_1_arm_1")}
}

surface_data$eventname = modify(surface_data$eventname, convert_eventname)
surface_data$eventname = as.factor(surface_data$eventname)
thickness_data$eventname = modify(thickness_data$eventname, convert_eventname)
thickness_data$eventname = as.factor(thickness_data$eventname)

balance_years <- function(tab) {
  tab_year1 = tab[tab$eventname == "1_year_follow_up_y_arm_1",]
  tab_year0 = tab[tab$eventname == "baseline_year_1_arm_1",]
  subject_test = unique(tab_year0$subjectkey)
  subject_retest=unique(tab_year1$subjectkey)
  tab_new = tab[(tab$subjectkey %in% subject_retest) & (tab$subjectkey %in% subject_test),]
  
  return(tab_new)
}
surface_data = balance_years(surface_data)
thickness_data = balance_years(thickness_data)
 
d0 = merge(x = surface_data, y = dataset, by = c("subjectkey","eventname"))
d0_t = merge(x = thickness_data, y = dataset, by = c("subjectkey","eventname"))
d1 = d0[,-c(1:71,81:84,92:103,108:112)]
d2 = d0[,-c(1:71,101,102,112:115)]
write.table(d2, file = "neuro_data_small2.csv", sep = "\t", row.names = F)
#write.table(d0, file = "thick_data.csv", sep = "\t",row.names = F)
detach(d1)
attach(d1)

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

d0$parents_income = ordered_factor(d0$parents_income)
d0_t$parents_income = ordered_factor(d0_t$parents_income)
d1$parents_income = ordered_factor(d1$parents_income)



###Anatomical models
mod = lm(LSurfArea ~ age_months + sex + size + slenderness + 
           race_ethnicity + SWTD + physical_activity + social_activities_time, 
         data = d1)

summary(mod)

table_construction <- function(n1) {
  race_table = data.frame(1,2,3)
  
  for (i in 1:5) {
    contrasts(race_ethnicity) = contr.treatment(5, base = i)
    contrasts(d1$race_ethnicity) = contr.treatment(5, base = i)
    mod <- lm(LSurfArea ~ age_months + sex + size + slenderness + 
                race_ethnicity + SWTD + physical_activity + social_activities_time, 
              data = d1)
    coeffs_mod = summary(mod)$coefficients[,1]
    t = cbind (coeffs_mod, confint(mod))
    race_table[((i-1)*4 + 1) : ((i-1)*4 + 4),] = rbind(t[n1,],t[n1+1,],t[n1+2,],t[n1+3,])
  }
  #Matrix construction
  
  rt_string = c()
  rt_numb = c()
  diag = c(1,7,13,19,25)
  j = 0
  for (i in 1:25) {
    if (i %in% diag) {
      rt_string[i] = "-"
      rt_numb[i] = 0
    }
    else {
      j = j+1
      if (race_table[j,2]*race_table[j,3]>0 || is.na(race_table[j,2]) || is.na(race_table[j,3])) {
        rt_string[i] = sprintf("%.3f [%.3f %.3f]", race_table[j,1],race_table[j,2],race_table[j,3])
        rt_numb[i] = race_table[j,1]
      }
      else {
        rt_string[i] = "n.s."
        rt_numb[i] = 0
      }
      
    }
  }

  list_of_matrices = c(rt_string, rt_numb)
  return(list_of_matrices)
}

matrices = table_construction(6)
races = c("White", "Black/Afr.Amer.", "Hispanic", "Asian", "Other/Mixed")
rt_string = array_reshape(matrices[1:25], c(5,5))
rownames(rt_string) = races
colnames(rt_string) = races
write.table(rt_string, file = "races_LSurf.csv", sep = ",", quote = FALSE, row.names = T)
rt_numb = array_reshape(matrices[26:50], c(5,5))
rownames(rt_numb) = races
colnames(rt_numb) = races
write.table(rt_numb, file = "races_RSurf2.csv", sep = ",", quote = FALSE, row.names = T)

round(t[t[,2]*t[,3]>0,],3)
#ICV
simple_lm = lm(ICV~ age_months + sex + size + slenderness + parents_income + race_ethnicity + physical_activity + SBD   ,data = d1)
summary(simple_lm)

par(mfrow = c(2, 2))
mtext("Residuals plots for ICV", outer = TRUE, cex = 1.5)
plot(simple_lm)
par(mfrow = c(1,1))

res_ICV = simple_lm$residuals 
p1 = ggplot(d1, aes(x=age_months, y=res_ICV)) + 
  geom_point()+
  labs(x="Age months", y = "Raw residuals")+
  theme_classic() 
p2 = ggplot(d1, aes(x= size, y=res_ICV)) + 
  geom_point()+
  labs(x="Size", y = "Raw residuals")+
  theme_classic()
p3 = ggplot(d1, aes(x= slenderness, y=res_ICV)) + 
  geom_point()+
  labs(x="Slenderness", y = "Raw residuals")+
  theme_classic() 
p4 = ggplot(d1, aes(x=SBD, y=res_ICV)) + 
  geom_point()+
  labs(x="SBD", y = "Raw residuals")+
  theme_classic() 
p5 = ggplot(d1, aes(x= physical_activity, y=res_ICV)) + 
  geom_point()+
  labs(x="Physical activity", y = "Raw residuals")+
  theme_classic() 
grid.arrange(p1, p2, p3, p4, p5, nrow = 2, ncol = 3)



#LThickness
simple_lm_LT = lm(LThickness~race_ethnicity + videogames_time + size)
summary(simple_lm_LT)

par(mfrow = c(2, 2))
plot(simple_lm_LT)
par(mfrow = c(1,1))

res_LT = simple_lm_LT$residuals 
p1 = ggplot(d1, aes(x=race_ethnicity, y=res_LT)) + 
  geom_point()+
  labs(x="Ethnicity", y = "Raw residuals")+
  theme_classic() 
p2 = ggplot(d1, aes(x= size, y=res_LT)) + 
  geom_point()+
  labs(x="Size", y = "Raw residuals")+
  theme_classic()
p3 = ggplot(d1, aes(x=videogames_time, y=res_LT)) + 
  geom_point()+
  labs(x="Videogames time", y = "Raw residuals")+
  theme_classic() 
grid.arrange(p1, p2, p3,nrow = 2, ncol = 2)


#RThickness
simple_lm_RT = lm(RThickness~ sex + race_ethnicity + SWTD + videogames_time + size,data = d1)
summary(simple_lm_RT)

par(mfrow = c(2, 2))
plot(simple_lm_RT)
par(mfrow = c(1,1))

#LSurface
simple_lm_LS = lm(LSurfArea ~ age_months + sex + size + slenderness + 
                         race_ethnicity + SWTD + physical_activity + social_activities_time, 
                       data = d1)
summary(simple_lm_LS)

par(mfrow = c(2, 2))
plot(simple_lm_LS)
par(mfrow = c(1,1))


#RSurface
simple_lm_RS = lm(RSurfArea ~ age_months + sex + size + slenderness + race_ethnicity + 
                    SWTD + video_time, data = d1)
summary(simple_lm_RS)

par(mfrow = c(2, 2))
plot(simple_lm_RS)
par(mfrow = c(1,1))


s_roi = read.csv("surface_data_roi.csv",header = TRUE)
s_roi$race_ethnicity = as.factor(s_roi$race_ethnicity)
s_roi$parents_income = as.factor(s_roi$parents_income)
t_roi = read.csv("thickness_data_roi.csv",header = TRUE)
t_roi$race_ethnicity = as.factor(t_roi$race_ethnicity)
t_roi$parents_income = as.factor(t_roi$parents_income)

#Dataset big for surface
simple_lm_sroi = lm(surface~age_months + sex + parents_income + people_cohabiting + size + 
                      slenderness + race_ethnicity + DIMS + SBD + SWTD +SHY + 
                      physical_activity + region + tv_time:region + 
                      video_time:region + videogames_time:region + 
                      social_activities_time:region, data = s_roi)
res = summary(simple_lm_sroi)$residuals
plot(s_roi$tv_time*s_roi$region, res)

summary(simple_lm_sroi)
par(mfrow = c(2, 2))
plot(simple_lm_sroi)
par(mfrow = c(1,1))

coeffs = c()
for (i in 1:dim(summary(simple_lm_sroi)$coefficients)[1]) {
  if (summary(simple_lm_sroi)$coefficients[i,4] < 0.05) {
    coeffs = rbind(coeffs,i)
  }
}
summary(simple_lm_sroi)$coefficients[coeffs,]
simple_lm_sroi2 = lm(d0[,4]~age_months + sex + parents_income + people_cohabiting + size + 
                       slenderness + race_ethnicity + DIMS + SBD + DA + SWTD + DOES + SHY + 
                       physical_activity + tv_time +  videogames_time + video_time +
                       social_activities_time, data = d0)


#Effects map for surface
for (i in 4:37) {
  d0[,i] = d0[,i]/d0$LSurfArea
}
for (i in 38:71) {
  d0[,i] = d0[,i]/d0$RSurfArea
}
lower = ~tv_time +  videogames_time + video_time +
  social_activities_time
upper = ~age_months + sex + parents_income + people_cohabiting + size + 
  slenderness + race_ethnicity + DIMS + SBD + DA + SWTD + DOES + SHY + 
  physical_activity + tv_time +  videogames_time + video_time +
  social_activities_time
simple_lm_sroi2 = step(lm(d0[,4]~age_months + sex + parents_income + people_cohabiting + size + 
                            slenderness + race_ethnicity + DIMS + SBD + DA + SWTD + DOES + SHY + 
                            physical_activity + tv_time +  videogames_time + video_time +
                            social_activities_time, data = d0), trace = 0, scope = list(upper = upper, lower = lower))

eff_map = cbind(summary(simple_lm_sroi2)$coefficients[c("tv_time","video_time","videogames_time", "social_activities_time"),],confint(simple_lm_sroi2)[c("tv_time","video_time","videogames_time", "social_activities_time"),])


for (i in 4:71) {
  print((i-3)/0.68)
  simple_lm_sroi2 = step(lm(d0[,i]~age_months + sex + parents_income + people_cohabiting + size + 
                         slenderness + race_ethnicity + DIMS + SBD + DA + SWTD + DOES + SHY + 
                         physical_activity + tv_time +  videogames_time + video_time +
                         social_activities_time, data = d0), trace = 0, scope = list(upper = upper, lower = lower))
  tmp = cbind(summary(simple_lm_sroi2)$coefficients[c("tv_time","video_time","videogames_time", "social_activities_time"),],confint(simple_lm_sroi2)[c("tv_time","video_time","videogames_time", "social_activities_time"),])
  eff_map = rbind(eff_map,tmp)
}
eff_map = eff_map[-c(1,2,3,4),]
p_fdr = p.adjust(eff_map[,4], 'bonferroni')
p_fdr[p_fdr<0.05]


Eff_map = as.data.frame(eff_map[,c(1,2,5,6,4)])
Eff_map_videogames = Eff_map[seq(3,272,4),]
Eff_map_videogames$pAdj_bonf =  p.adjust(Eff_map_videogames[,5], 'bonferroni')
Eff_map_videogames$pAdj_fdr =  p.adjust(Eff_map_videogames[,5], 'fdr')
Eff_map_videogames[Eff_map_videogames$pAdj_fdr<0.05,]
Eff_map_videogames = as.data.frame(rbind(Eff_map_videogames[1:3,],c(NaN,NaN,NaN,NaN,NaN,NaN,NaN),Eff_map_videogames[4:37,],c(NaN,NaN,NaN,NaN,NaN,NaN,NaN),Eff_map_videogames[38:68,]))
Eff_map_videogames$Region  = region_names

Eff_map_video = Eff_map[seq(2,272,4),]
Eff_map_video$pAdj_bonf =  p.adjust(Eff_map_video[,5], 'bonferroni')
Eff_map_video$pAdj_fdr =  p.adjust(Eff_map_video[,5], 'fdr')
rownames(Eff_map_video) = list(region_names)
Eff_map_video[Eff_map_video$pAdj_fdr<0.05,]

Eff_map_social = Eff_map[seq(4,272,4),]
Eff_map_social$pAdj_bonf =  p.adjust(Eff_map_social[,5], 'bonferroni')
Eff_map_social$pAdj_fdr =  p.adjust(Eff_map_social[,5], 'fdr')
rownames(Eff_map_social) = region_names
Eff_map_social[Eff_map_social$pAdj_fdr<0.05,]

Eff_map_tv = Eff_map[seq(1,272,4),]
Eff_map_tv$pAdj_bonf =  p.adjust(Eff_map_tv[,5], 'bonferroni')
Eff_map_tv$pAdj_fdr =  p.adjust(Eff_map_tv[,5], 'fdr')
rownames(Eff_map_tv) = region_names
Eff_map_tv[Eff_map_tv$pAdj_fdr<0.05,]








write.csv(Eff_map, file = "map_surf_norm.csv",row.names = T)





#Dataset big for thickness
simple_lm_troi = lm(thickness~sex + parents_income + size + 
                      slenderness + race_ethnicity + DIMS + SBD + DA + SWTD + DOES + 
                      tv_time +  videogames_time + video_time +
                      social_activities_time +  region + tv_time:region + 
                      video_time:region + videogames_time:region + 
                      social_activities_time:region, data = t_roi)
summary(simple_lm_troi)
coeffs_t = c()
for (i in 1:dim(summary(simple_lm_troi)$coefficients)[1]) {
  if (summary(simple_lm_troi)$coefficients[i,4] < 0.05) {
    coeffs_t = rbind(coeffs_t,i)
  }
}
summary(simple_lm_troi)$coefficients[coeffs_t,]
plot(simple_lm_sroi)

#Effects map for thickness
for (i in 4:37) {
  d0_t[,i] = d0_t[,i]/d0_t$LSurfArea
}
for (i in 38:71) {
  d0_t[,i] = d0_t[,i]/d0_t$RSurfArea
}

simple_lm_troi2 = lm(d0_t[,4]~age_months + sex + parents_income + people_cohabiting + size + 
                       slenderness + race_ethnicity + DIMS + SBD + DA + SWTD + DOES + SHY + 
                       physical_activity + tv_time +  videogames_time + video_time +
                       social_activities_time, data = d0_t)
eff_map_t = cbind(summary(simple_lm_troi2)$coefficients[c("tv_time","video_time","videogames_time", "social_activities_time"),],confint(simple_lm_troi2)[c("tv_time","video_time","videogames_time", "social_activities_time"),])




for (i in 4:71) {
  print((i-3)/0.68)
  simple_lm_troi2 = step(lm(d0_t[,i]~age_months + sex + parents_income + people_cohabiting + size + 
                         slenderness + race_ethnicity + DIMS + SBD + DA + SWTD + DOES + SHY + 
                         physical_activity + tv_time +  videogames_time + video_time +
                         social_activities_time, data = d0_t), trace = 0, scope = list(upper = upper, lower = lower))
  tmp = cbind(summary(simple_lm_troi2)$coefficients[c("tv_time","video_time","videogames_time", "social_activities_time"),],confint(simple_lm_troi2)[c("tv_time","video_time","videogames_time", "social_activities_time"),])
  eff_map_t = rbind(eff_map_t,tmp)
}

eff_map_t = eff_map_t[-c(1,2,3,4),]


Eff_map_t = as.data.frame(eff_map_t[,c(1,2,5,6,4)])
Eff_map_videogames = Eff_map_t[seq(3,272,4),]
Eff_map_videogames$pAdj_bonf =  p.adjust(Eff_map_videogames[,5], 'bonferroni')
Eff_map_videogames$pAdj_fdr =  p.adjust(Eff_map_videogames[,5], 'fdr')
Eff_map_videogames[Eff_map_videogames$pAdj_fdr<0.05,]
Eff_map_videogames = as.data.frame(rbind(Eff_map_videogames[1:3,],c(NaN,NaN,NaN,NaN,NaN,NaN,NaN),Eff_map_videogames[4:37,],c(NaN,NaN,NaN,NaN,NaN,NaN,NaN),Eff_map_videogames[38:68,]))
Eff_map_videogames$Region  = region_names

eff_videogames = Eff_map_videogames[,c(1,7)]

Eff_map_video = Eff_map_t[seq(2,272,4),]
Eff_map_video$pAdj_bonf =  p.adjust(Eff_map_video[,5], 'bonferroni')
Eff_map_video$pAdj_fdr =  p.adjust(Eff_map_video[,5], 'fdr')
rownames(Eff_map_video) = region_names
Eff_map_video[Eff_map_video$pAdj_fdr<0.05,]

Eff_map_social = Eff_map_t[seq(4,272,4),]
Eff_map_social$pAdj_bonf =  p.adjust(Eff_map_social[,5], 'bonferroni')
Eff_map_social$pAdj_fdr =  p.adjust(Eff_map_social[,5], 'fdr')
rownames(Eff_map_social) = region_names
Eff_map_social[Eff_map_social$pAdj_fdr<0.05,]

Eff_map_tv = Eff_map_t[seq(1,272,4),]
Eff_map_tv$pAdj_bonf =  p.adjust(Eff_map_tv[,5], 'bonferroni')
Eff_map_tv$pAdj_fdr =  p.adjust(Eff_map_tv[,5], 'fdr')
rownames(Eff_map_tv) = region_names
Eff_map_tv[Eff_map_tv$pAdj_fdr<0.05,]

write.csv(eff_videogames, file = "eff_videogames.csv",row.names = T)


#Difference statistics
anat = d1[,c(1,2,3,4,5)]
anat_base = anat[d0$eventname=="baseline_year_1_arm_1",]
anat_foll = anat[d0$eventname=="1_year_follow_up_y_arm_1",]
anat_diff = (anat_foll -anat_base) / anat_base
colnames(anat_diff) = c("LThick_diff","RThick_diff","LSurf_diff","RSurf_diff","ICV_diff")
summary(anat_diff)
d1_base = d1[d0$eventname=="baseline_year_1_arm_1",]
d1_base[c("LThick_diff","RThick_diff","LSurf_diff","RSurf_diff","ICV_diff")] = anat_diff

hist(d1_base$ICV_diff,100)


simple_lm_LS_diff = step(lm(ICV_diff~age_months + sex + parents_income + people_cohabiting + size + 
                         slenderness + race_ethnicity + DIMS + SBD + DA + SWTD + DOES + SHY + 
                         physical_activity + tv_time +  videogames_time + video_time +
                         social_activities_time, data = d1_base), scope = list(upper=upper,lower = lower))
simple_lm_LS_diff = lm(ICV_diff~age_months + sex +people_cohabiting + size + 
                         slenderness + race_ethnicity + DIMS + SBD + DA + SWTD + DOES + SHY + 
                         physical_activity + tv_time +  videogames_time + video_time +
                         social_activities_time, data = d1_base)
summary(simple_lm_LS_diff)
step()


