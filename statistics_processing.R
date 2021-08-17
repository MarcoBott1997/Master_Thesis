rm(list = ls())
library(tidyverse)
#Setting working directory
wd <- "C:/Users/Marco Bottino/Desktop/Marco/Poli/Tesi Magistrale/ABCD"
setwd(wd)

pdem02 <- read.delim("C:/Users/Marco Bottino/Desktop/Marco/Poli/Tesi Magistrale/ABCD/Datasets/pdem02.txt", na.strings="")
abcd_ant01 <- read.delim("C:/Users/Marco Bottino/Desktop/Marco/Poli/Tesi Magistrale/ABCD/Datasets/abcd_ant01.txt", na.strings="")
acspsw03 <- read.delim("C:/Users/Marco Bottino/Desktop/Marco/Poli/Tesi Magistrale/ABCD/Datasets/acspsw03.txt", na.strings="")
abcd_sds01 <- read.delim("C:/Users/Marco Bottino/Desktop/Marco/Poli/Tesi Magistrale/ABCD/Datasets/abcd_sds01.txt", na.strings="")
abcd_cbcl01 <- read.delim("C:/Users/Marco Bottino/Desktop/Marco/Poli/Tesi Magistrale/ABCD/Datasets/abcd_cbcl01.txt", na.strings="")
abcd_stq01 <- read.delim("C:/Users/Marco Bottino/Desktop/Marco/Poli/Tesi Magistrale/ABCD/Datasets/abcd_stq01.txt", na.strings="")
abcd_yrb01 <- read.delim("C:/Users/Marco Bottino/Desktop/Marco/Poli/Tesi Magistrale/ABCD/Datasets/abcd_yrb01.txt", na.strings="")



#########
#First dataset: pdem02, for sex, parents income and number of people cohabiting
#########
d1 =pdem02
d1 = select(d1,subjectkey,sex,demo_comb_income_v2,demo_roster_v2)
colnames(d1) = c("subjectkey","sex","parents_income", "people_cohabiting")
d1 = d1[-1,]
View(d1)

#########
#Second dataset: abcd_ant01, for anatomical info
#########
d2 =abcd_ant01
d2 = select(d2,"subjectkey","interview_age","eventname","anthroheightcalc","anthroweightcalc", "anthro_waist_cm")
colnames(d2) = c("subjectkey","age_months","eventname", "height_cm","weight_kg", "waist_cm")
d2 = d2[-1,]
d2$age_months = as.numeric(as.character(d2$age_months))
d2$height_cm = as.numeric(as.character(d2$height_cm))
d2$height_cm = d2$height_cm * 2.54
summary(d2$height_cm)
d2$weight_kg = as.numeric(as.character(d2$weight_kg))
d2$weight_kg = d2$weight_kg * 0.453
summary(d2$weight_kg)
d2$waist_cm = as.numeric(as.character(d2$waist_cm))
d2$waist_cm = d2$waist_cm * 2.54
summary(d2$waist_cm)
d2$bmi = d2$weight / (d2$height / 100)^2
summary(d2$bmi)
View(d2)

#########
#Third dataset: acspsw03, for race
#########
d3 = acspsw03[acspsw03$eventname == "baseline_year_1_arm_1",c(4,10)]
View(d3)

#########
#Fourth dataset: abcd_sds01, for sleep quality features
#########
d4 = abcd_sds01
d4 = d4[-1,]
d4 = d4[order(d4$subjectkey, d4$interview_age),]
d4 = d4[seq(1, nrow(d4), 2), ]
col_names = colnames(d4)[11:36]

for (col in col_names) {
  d4[,c(col)] = as.integer(as.character(d4[,c(col)]))
}

d4$DIMS = d4$sleepdisturb1_p +  d4$sleepdisturb2_p +  d4$sleepdisturb3_p +  d4$sleepdisturb4_p +  d4$sleepdisturb5_p +  d4$sleepdisturb10_p +  d4$sleepdisturb11_p
d4$SBD = d4$sleepdisturb13_p +  d4$sleepdisturb14_p +  d4$sleepdisturb15_p 
d4$DA = d4$sleepdisturb17_p +  d4$sleepdisturb20_p +  d4$sleepdisturb21_p
d4$SWTD = d4$sleepdisturb6_p +  d4$sleepdisturb7_p +  d4$sleepdisturb8_p +  d4$sleepdisturb12_p +  d4$sleepdisturb18_p +  d4$sleepdisturb19_p
d4$DOES = d4$sleepdisturb22_p +  d4$sleepdisturb23_p +  d4$sleepdisturb24_p +  d4$sleepdisturb25_p +  d4$sleepdisturb26_p
d4$SHY = d4$sleepdisturb9_p +  d4$sleepdisturb16_p
d4$sleep_disturb = d4$DIMS + d4$SBD + d4$DA + d4$SWTD + d4$DOES + d4$SHY
d4 = select(d4,"subjectkey","eventname","DIMS","SBD", "DA", "SWTD", "DOES", "SHY", "sleep_disturb")
colnames(d4) = c("subjectkey","eventname","DIMS","SBD", "DA", "SWTD", "DOES", "SHY", "sleep_disturb")

View(d4)


#########
#Fifth dataset: abcd_cbcl01, for children behaviour features
#########
d5 = abcd_cbcl01
d5 = d5[-1,]
d5 = d5[order(d5$subjectkey, d5$interview_age),]
col_names = colnames(d5)
d5$interview_age = as.integer(as.character(d5$interview_age))
for (col in col_names[10:128]) {
  d5[,c(col)] = as.integer(as.character(d5[,c(col)]))
  
}

for (col in c(10:128)) {
  summary(d5[,c(col)])
}

#Anxiety/depression columns
anx_depr = c(14,29,30,31,32,33,35,45,50,52,71,91,112)
anx_depr = anx_depr + 9
anx_depr[which(anx_depr > 65)] = anx_depr[which(anx_depr > 65)] + 7
col_anx_depr = col_names[anx_depr]

n = integer(length(nrow(d5)))
for (col in col_anx_depr) {
  n = n + d5[,c(col)]
}
d5$anx_depr = n


#Withdrawn/depression columns
with_depr = c(5,42,65,69,75,102,103,111)
with_depr = with_depr + 9
with_depr[which(with_depr > 65)] = with_depr[which(with_depr > 65)] + 7
col_with_depr = col_names[with_depr]

n = integer(length(nrow(d5)))
for (col in col_with_depr) {
  n = n + d5[,c(col)]
}
d5$with_depr = n


#Somatic complaints columns
som_comp = c(47,49,51,54,56,57,58,59,60,61,62)
som_comp = som_comp + 9
col_som_comp = col_names[som_comp]

n = integer(length(nrow(d5)))
for (col in col_som_comp) {
  n = n + d5[,c(col)]
}
d5$som_comp = n


#Social problems columns
social_pr = c(11,12,25,27,34,36,38,48,62,64,79)
social_pr = social_pr + 9
social_pr[which(social_pr > 65)] = social_pr[which(social_pr > 65)] + 7
col_social_pr = col_names[social_pr]

n = integer(length(nrow(d5)))
for (col in col_social_pr) {
  n = n + d5[,c(col)]
}
d5$social_pr = n


#Thought problems columns
thought_pr = c(9,18,40,46,58,59,60,66,70,76,83,84,85,92,100)
thought_pr = thought_pr + 9
thought_pr[which(thought_pr > 65)] = thought_pr[which(thought_pr > 65)] + 7
col_thought_pr = col_names[thought_pr]

n = integer(length(nrow(d5)))
for (col in col_thought_pr) {
  n = n + d5[,c(col)]
}
d5$thought_pr = n


#Attention problems columns
att_pr = c(1,4,8,10,13,17,41,61,78,80)
att_pr = att_pr + 9
att_pr[which(att_pr > 65)] = att_pr[which(att_pr > 65)] + 7
col_att_pr = col_names[att_pr]

n = integer(length(nrow(d5)))
for (col in col_att_pr) {
  n = n + d5[,c(col)]
}
d5$att_pr = n


#Rule breaking behaviour columns
rule_br_bh = c(2,26,28,39,43,63,67,72,73,81,82,90,96,99,101,105,106)
rule_br_bh = rule_br_bh + 9
rule_br_bh[which(rule_br_bh > 65)] = rule_br_bh[which(rule_br_bh > 65)] + 7
col_rule_br_bh = col_names[rule_br_bh]

n = integer(length(nrow(d5)))
for (col in col_rule_br_bh) {
  n = n + d5[,c(col)]
}
d5$rule_br_bh = n


#Aggressive behaviour columns
agg_bh = c(3,16,19,20,21,22,23,37,57,68,86,87,88,89,94,95,97,104)
agg_bh = agg_bh + 9
agg_bh[which(agg_bh > 65)] = agg_bh[which(agg_bh > 65)] + 7
col_agg_bh = col_names[agg_bh]

n = integer(length(nrow(d5)))
for (col in col_agg_bh) {
  n = n + d5[,c(col)]
}
d5$agg_bh = n

d5$internalizing_score = d5$anx_depr + d5$with_depr + d5$som_comp
d5$externalizing_score = d5$rule_br_bh + d5$agg_bh
d5$cbcl_score = d5$anx_depr + d5$with_depr + d5$som_comp + d5$social_pr + d5$thought_pr + d5$att_pr + d5$rule_br_bh + d5$agg_bh
d5 = select(d5,"subjectkey","interview_age","anx_depr","with_depr", "som_comp", "social_pr", "thought_pr", "att_pr", "rule_br_bh", "agg_bh", "internalizing_score", "externalizing_score", "cbcl_score")
colnames(d5) = c("subjectkey","age_months","anx_depr","with_depr", "som_comp", "social_pr", "thought_pr", "att_pr", "rule_br_bh", "agg_bh", "internalizing_score", "externalizing_score", "cbcl_score")

View(d5)

#Sixth dataset: abcd_yrb01, physical activity
d6 = abcd_yrb01[,c(4,9,10)]
d6 = d6[-1,]
colnames(d6) = c("subjectkey", "eventname", "physical_activity")
d6$physical_activity = as.numeric(as.character(d6$physical_activity))

#Target dataset: abcd_stq01, screen time activity
d7 = abcd_stq01[,c(4,9:21)]
head(d7,1)
d7 = d7[-1,]
col_names = colnames(d7)[3:14]
for (col in col_names) {
  d7[,c(col)] = as.numeric(as.character(d7[,c(col)]))
}
d7$tv_time = (d7$screen1_wkdy_y*5+d7$screen7_wknd_y*2)/7
d7$video_time = (d7$screen2_wkdy_y*5+d7$screen8_wknd_y*2)/7
d7$videogames_time = (d7$screen3_wkdy_y*5+d7$screen9_wknd_y*2)/7
d7$texting_time = (d7$screen4_wkdy_y*5+d7$screen10_wknd_y*2)/7
d7$social_time = (d7$screen5_wkdy_y*5+d7$screen11_wknd_y*2)/7
d7$videochat_time = (d7$screen_wkdy_y*5+d7$screen12_wknd_y*2)/7
d7$tot_screen_time = d7$tv_time + d7$video_time + d7$videogames_time + d7$texting_time + d7$social_time + d7$videochat_time
d7 = select(d7,"subjectkey","eventname","tv_time","video_time", "videogames_time", "texting_time", "social_time", "videochat_time", "tot_screen_time")



d0 = merge(x = d1, y = d2, by = "subjectkey", all = TRUE)
d0 = merge(x = d0, y = d3, by = "subjectkey", all = TRUE)
d0 = merge(x = d0, y = d4, by = c("subjectkey","eventname"), all = TRUE)
d0 = merge(x = d0, y = d5, by = c("subjectkey","age_months"), all = TRUE)
d0 = merge(x = d0, y = d6, by = c("subjectkey","eventname"), all = TRUE)
d0 = merge(x = d0, y = d7, by = c("subjectkey","eventname"), all = TRUE)
d0 = filter(d0, d0$subjectkey != "NDAR_INVMFMZ8KKG")

for (sub in levels(d0$subjectkey)) {
if (!counter%%20) {print(counter)}
sub_data = d0[d0$subjectkey==sub,]
year2 = sub_data[sub_data$eventname == "2_year_follow_up_y_arm_1",]
if (length(year2$physical_activity) == 0) {
  d0[d0$eventname == "1_year_follow_up_y_arm_1" & d0$subjectkey == sub, c(30)] = year0$physical_activity
} else {
  d0[d0$eventname == "1_year_follow_up_y_arm_1" & d0$subjectkey == sub, c(30)] = floor((year0$physical_activity + year2$physical_activity)/2)
  }
}

view(d0)

d0 = distinct(d0)

write.table(d0, file = "data_merged.csv", sep = "\t", row.names = F)
tab = read.table("data_merged.csv", header = TRUE, sep = "\t")



summary(d0)
