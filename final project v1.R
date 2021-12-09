
####### MARKOV MODEL Version 1 ######

#rm(list = ls())

## Based on Treeage Markov model lecture by Lindsey Falk

## Install package to do this

#if("heemod" %in% rownames(installed.packages()) == FALSE) {install.packages("heemod")}
library(heemod)
library(readxl)
library(tidyverse)


## Reference HEEMOD (Filipovic-Pierucci A, Zarca K and Durand-Zaleski I (2017). "Markov Models for Health Economic Evaluations: The R Package heemod." ArXiv e-prints. R package version 0.8.0, 1702.03252)

## Read in Transition probabilities 
pap_normal_transition <- readxl::read_xlsx("By age Transition Probabilities.xlsx", sheet = "PAP_normal")
pap_normal_transition  <- pap_normal_transition %>% remove_rownames %>% column_to_rownames(var="Age")


hpv_normal_transition <- readxl::read_xlsx("By age Transition Probabilities.xlsx", sheet = "HPV_normal")
hpv_normal_transition  <- hpv_normal_transition %>% remove_rownames %>% column_to_rownames(var="Age")

pap_decisiontree_transition <- readxl::read_xlsx("By age Transition Probabilities.xlsx", sheet = "PAP_decisiontree")
pap_decisiontree_transition  <- pap_decisiontree_transition %>% remove_rownames %>% column_to_rownames(var="Age")

hpv_decisiontree_transition <- readxl::read_xlsx("By age Transition Probabilities.xlsx", sheet = "HPV_decisiontree")
hpv_decisiontree_transition  <- hpv_decisiontree_transition %>% remove_rownames %>% column_to_rownames(var="Age")

hpv_beforetest_transition <- readxl::read_xlsx("By age Transition Probabilities.xlsx", sheet = "HPV_beforetesting")
hpv_beforetest_transition  <- hpv_beforetest_transition %>% remove_rownames %>% column_to_rownames(var="Age")

#### Place all your model parameters here, it will be helpful for the sensitivity analyses  ####

param<-define_parameters(
  disc=0.015,
  
  ## Effects
  
  #eW=1,  ## effect of well state
  eCIN1=0.91,  ## effect of CIN1
  eCIN23=0.87,  ## effect CIN23
  eICC1=0.935,  ## effect of ICC1 - treated
  eICC2=0.75, ## effect of ICC2 - treated
  eRem=0.94,  ## effect of remission
  eD=0, ## effect of death 
  
  ## Costs

  cCIN1=151,  ## cost of CIN1
  cCIN23=897.62,  ## cost CIN23
  cICC1=18525.99,  ## cost of ICC1
  cICC2=44201.74, ## cost of ICC2 
  cRem=113,  ## cost of remission
  cD=0,  ## cost of death
  
  ##AGE
  
  age_base = 20,
  age_cycle = model_time + age_base,
  age_start_hpv = 21,
  age_start_pap = 21
)

param <- heemod::modify(param, 
  
  ## Transition Probabilities
  ## PAP
  # Well
  pWelltoCIN1_PAP = ifelse(age_cycle <age_start_pap, hpv_beforetest_transition[age_cycle, "pWelltoCIN1"],
                           ifelse(age_cycle == age_start_pap, pap_decisiontree_transition[age_cycle, "pWelltoCIN1"], 
                                  pap_normal_transition[age_cycle, "pWelltoCIN1"])),
  pWelltoCIN23_PAP = ifelse(age_cycle <age_start_pap, hpv_beforetest_transition[age_cycle, "pWelltoCIN23"],
                            ifelse(age_cycle == age_start_pap, pap_decisiontree_transition[age_cycle,"pWelltoCIN23"],
                                   pap_normal_transition[age_cycle, "pWelltoCIN23"])),
  pWelltoICC1_PAP = ifelse(age_cycle <age_start_pap, hpv_beforetest_transition[age_cycle, "pWelltoICC1"],
                           ifelse(age_cycle == age_start_pap, pap_decisiontree_transition[age_cycle, "pWelltoICC1"],
                                  pap_normal_transition[age_cycle, "pWelltoICC1"])),
  pWelltoICC2_PAP = ifelse(age_cycle <age_start_pap, hpv_beforetest_transition[age_cycle, "pWelltoICC2"],
                           ifelse(age_cycle == age_start_pap, pap_decisiontree_transition[age_cycle, "pWelltoICC2"],
                                  pap_normal_transition[age_cycle, "pWelltoICC2"])),
  # CIN1
  pCIN1toWell_PAP = ifelse(age_cycle <age_start_pap, hpv_beforetest_transition[age_cycle, "pCIN1toWell"], 
                           pap_normal_transition[age_cycle, "pCIN1toWell"]),
  pCIN1toCIN23_PAP = ifelse(age_cycle <age_start_pap, hpv_beforetest_transition[age_cycle, "pCIN1toCIN23"], 
                            pap_normal_transition[age_cycle, "pCIN1toCIN23"]),
  
  # CIN23
  pCIN23toWell_PAP = ifelse(age_cycle <age_start_pap, hpv_beforetest_transition[age_cycle, "pCIN23toWell"], 
                            pap_normal_transition[age_cycle, "pCIN23toWell"]),
  pCIN23toCIN1_PAP = ifelse(age_cycle <age_start_pap, hpv_beforetest_transition[age_cycle, "pCIN23toCIN1"], 
                            pap_normal_transition[age_cycle, "pCIN23toCIN1"]),
  pCIN23toICC1_PAP = ifelse(age_cycle <age_start_pap, hpv_beforetest_transition[age_cycle, "pCIN23toCIN1"], 
                            pap_normal_transition[age_cycle, "pCIN23toCIN1"]),
  
  # ICC1
  pICC1toICC2_PAP = ifelse(age_cycle <age_start_pap, hpv_beforetest_transition[age_cycle, "pICC1toICC2"], 
                           pap_normal_transition[age_cycle, "pICC1toICC2"]), 
  pICC1toRem_PAP = ifelse(age_cycle <age_start_pap, hpv_beforetest_transition[age_cycle, "pICC1toRem"], 
                          pap_normal_transition[age_cycle, "pICC1toICC2"]), 
  pICC1toDeath_PAP = ifelse(age_cycle <age_start_pap, hpv_beforetest_transition[age_cycle, "pICC1toDeath"], 
                            pap_normal_transition[age_cycle, "pICC1toDeath"]), 
  
  # ICC2
  pICC2toRem_PAP = ifelse(age_cycle <age_start_pap, hpv_beforetest_transition[age_cycle, "pICC2toRem"], 
                          pap_normal_transition[age_cycle, "pICC2toRem"]), 
  pICC2toDeath_PAP = ifelse(age_cycle <age_start_pap, hpv_beforetest_transition[age_cycle, "pICC2toDeath"], 
                            pap_normal_transition[age_cycle, "pICC2toDeath"]),
  
  # REM
  pRemtoICC2_PAP = ifelse(age_cycle <age_start_pap, hpv_beforetest_transition[age_cycle, "pRemtoICC2"], 
                          pap_normal_transition[age_cycle, "pRemtoICC2"]),
  
  
  ## HPV
  # Well
  pWelltoCIN1_HPV = ifelse(age_cycle <age_start_hpv, hpv_beforetest_transition[age_cycle, "pWelltoCIN1"],
                           ifelse(age_cycle == age_start_hpv, hpv_decisiontree_transition[age_cycle, "pWelltoCIN1"], 
                                  hpv_normal_transition[age_cycle, "pWelltoCIN1"])),
  pWelltoCIN23_HPV = ifelse(age_cycle <age_start_hpv, hpv_beforetest_transition[age_cycle, "pWelltoCIN23"],
                            ifelse(age_cycle == age_start_hpv, hpv_decisiontree_transition[age_cycle,"pWelltoCIN23"],
                                   hpv_normal_transition[age_cycle, "pWelltoCIN23"])),
  pWelltoICC1_HPV = ifelse(age_cycle <age_start_hpv, hpv_beforetest_transition[age_cycle, "pWelltoICC1"],
                           ifelse(age_cycle == age_start_hpv, hpv_decisiontree_transition[age_cycle, "pWelltoICC1"],
                                  hpv_normal_transition[age_cycle, "pWelltoICC1"])),
  pWelltoICC2_HPV = ifelse(age_cycle <age_start_hpv, hpv_beforetest_transition[age_cycle, "pWelltoICC2"],
                           ifelse(age_cycle == age_start_hpv, hpv_decisiontree_transition[age_cycle, "pWelltoICC2"],
                                  hpv_normal_transition[age_cycle, "pWelltoICC2"])),
    
  # CIN1  
  pCIN1toWell_HPV = ifelse(age_cycle <age_start_hpv, hpv_beforetest_transition[age_cycle, "pCIN1toWell"], 
                           hpv_normal_transition[age_cycle, "pCIN1toWell"]),
  pCIN1toCIN23_HPV = ifelse(age_cycle <age_start_hpv, hpv_beforetest_transition[age_cycle, "pCIN1toCIN23"], 
                            hpv_normal_transition[age_cycle, "pCIN1toCIN23"]),
  
  # CIN23
  pCIN23toWell_HPV = ifelse(age_cycle <age_start_hpv, hpv_beforetest_transition[age_cycle, "pCIN23toWell"], 
                            hpv_normal_transition[age_cycle, "pCIN23toWell"]),
  pCIN23toCIN1_HPV = ifelse(age_cycle <age_start_hpv, hpv_beforetest_transition[age_cycle, "pCIN23toCIN1"], 
                            hpv_normal_transition[age_cycle, "pCIN23toCIN1"]),
  pCIN23toICC1_HPV = ifelse(age_cycle <age_start_hpv, hpv_beforetest_transition[age_cycle, "pCIN23toCIN1"], 
                            hpv_normal_transition[age_cycle, "pCIN23toCIN1"]),
  
  # ICC1
  pICC1toICC2_HPV = ifelse(age_cycle <age_start_hpv, hpv_beforetest_transition[age_cycle, "pICC1toICC2"], 
                           hpv_normal_transition[age_cycle, "pICC1toICC2"]), 
  pICC1toRem_HPV = ifelse(age_cycle <age_start_hpv, hpv_beforetest_transition[age_cycle, "pICC1toRem"], 
                          hpv_normal_transition[age_cycle, "pICC1toICC2"]), 
  pICC1toDeath_HPV = ifelse(age_cycle <age_start_hpv, hpv_beforetest_transition[age_cycle, "pICC1toDeath"], 
                            hpv_normal_transition[age_cycle, "pICC1toDeath"]), 
  
  # ICC2
  pICC2toRem_HPV = ifelse(age_cycle <age_start_hpv, hpv_beforetest_transition[age_cycle, "pICC2toRem"], 
                          hpv_normal_transition[age_cycle, "pICC2toRem"]), 
  pICC2toDeath_HPV = ifelse(age_cycle <age_start_hpv, hpv_beforetest_transition[age_cycle, "pICC2toDeath"], 
                            hpv_normal_transition[age_cycle, "pICC2toDeath"]), 
  
  # REM
  pRemtoICC2_HPV = ifelse(age_cycle <age_start_hpv, hpv_beforetest_transition[age_cycle, "pRemtoICC2"], 
                          hpv_normal_transition[age_cycle, "pRemtoICC2"])
  

  
)

## Background death rate
param <- heemod::modify(param,
sex_indiv = "FMLE", # FMLE => female in the WHO database
p_death_all = get_who_mr(
age = age_cycle,
sex = sex_indiv,
country = "CAN",
year = "latest",
local = TRUE))

## Modifications for pap vs. hpv
param <- heemod::modify(
  param,
  cW_pap=ifelse(((age_cycle > age_start_pap & age_cycle <70) &model_time %% 3),127.84, 0),  ## cost of well state (including testing) - pap
  cW_hpv = ifelse(((age_cycle > age_start_hpv & age_cycle <70) &model_time %% 5),110, 0), ## cost of well state (including testing) - HPV)
  eW_pap=ifelse(((age_cycle > age_start_pap & age_cycle <70) &model_time %% 3),.9, 1),  ## cost of well state (including testing) - pap
  eW_hpv = ifelse(((age_cycle > age_start_hpv & age_cycle <70) &model_time %% 5),.99, 1) ## cost of well state (including testing) - HPV)
)



#### Create transition probability matrix for treatment and comparator ####

mat_PAP<-define_transition(
  C, pWelltoCIN1_PAP, pWelltoCIN23_PAP, pWelltoCIN23_PAP, pWelltoICC1_PAP, pWelltoICC2_PAP, p_death_all, 
  pCIN1toWell_PAP, C, pCIN1toCIN23_PAP, 0, 0, 0, p_death_all,
  pCIN23toWell_PAP, pCIN23toCIN1_PAP, C, pCIN23toICC1_PAP, 0, 0, p_death_all,
  0, 0, 0, C, pICC1toICC2_PAP,	pICC1toRem_PAP,	pICC1toDeath_PAP*p_death_all,
  0,	0,	0,	0,	C,	pICC2toRem_PAP,	pICC2toDeath_PAP*p_death_all,
  0,	0,	0,	0,	pRemtoICC2_PAP,	C,	p_death_all,
  0,	0,	0,	0,	0,	0,	1, 
  state_names=c("Well", "CIN1", "CIN23", "ICC1", "ICC2", "REM", "Death")
)

mat_HPV<-define_transition(
  C, pWelltoCIN1_HPV, pWelltoCIN23_HPV, pWelltoCIN23_HPV, pWelltoICC1_HPV, pWelltoICC2_HPV, p_death_all, 
  pCIN1toWell_HPV, C, pCIN1toCIN23_HPV, 0, 0, 0, p_death_all,
  pCIN23toWell_HPV, pCIN23toCIN1_HPV, C, pCIN23toICC1_HPV, 0, 0, p_death_all,
  0, 0, 0, C, pICC1toICC2_HPV,	pICC1toRem_HPV,	pICC1toDeath_HPV*p_death_all,
  0,	0,	0,	0,	C,	pICC2toRem_HPV,	pICC2toDeath_HPV*p_death_all,
  0,	0,	0,	0,	pRemtoICC2_HPV,	C,	p_death_all,
  0,	0,	0,	0,	0,	0,	1, 
  state_names=c("Well", "CIN1", "CIN23", "ICC1", "ICC2", "REM", "Death")
)


#### Outcomes associated with each health state ####
state_Well<-define_state(
  cost = discount(dispatch_strategy(
    PAP = cW_pap,
    HPV = cW_hpv
  ), disc),
  effect = discount(dispatch_strategy(
    PAP = eW_pap,
    HPV = eW_hpv
  ), disc)
)

state_CIN1<-define_state(
  cost = discount(dispatch_strategy(
    PAP = cCIN1,
    HPV = cCIN1
  ), disc),
  effect = discount(eCIN1,disc)
)

state_CIN23<-define_state(
  cost = discount(dispatch_strategy(
    PAP = cCIN23,
    HPV = cCIN23
  ), disc),
  effect = discount(eCIN23,disc)
)

state_ICC1<-define_state(
  cost = discount(dispatch_strategy(
    PAP = cICC1,
    HPV = cICC1
  ), disc),
  effect = discount(eICC1,disc)
)

state_ICC2<-define_state(
  cost = discount(dispatch_strategy(
    PAP = cICC2,
    HPV = cICC2
  ), disc),
  effect = discount(eICC2,disc)
)

state_Rem<-define_state(
  cost = discount(dispatch_strategy(
    PAP = cRem,
    HPV = cRem
  ), disc),
  effect = discount(eRem,disc)
)

state_Death<-define_state(
  cost = discount(dispatch_strategy(
    PAP = 0,
    HPV = 0
  ), disc),
  effect = discount(0,disc)
)
#### Determining the inputs to be calculated for each strategy option (what transition matrix, health state values)
##"Well", "CIN1", "CIN23", "ICC1", "ICC2", "REM", "Death"
strategy_PAP<-define_strategy(
  transition = mat_PAP,
  Well = state_Well,
  CIN1 = state_CIN1,
  CIN23 = state_CIN23,
  ICC1 = state_ICC1,
  ICC2 = state_ICC2,
  REM = state_Rem,
  Death = state_Death
)


strategy_HPV<-define_strategy(
  transition = mat_HPV,
  Well = state_Well,
  CIN1 = state_CIN1,
  CIN23 = state_CIN23,
  ICC1 = state_ICC1,
  ICC2 = state_ICC2,
  REM = state_Rem,
  Death = state_Death
)

#### Run MODEL(S) ####
model_v2<-run_model(
  PAP = strategy_PAP,
  HPV = strategy_HPV,
  parameters = param,
  cycles=80,
  init=c(495777, 0, 0,0,0,0,0),
  method="life-table",
  cost=cost,
  effect=effect
  
)

summary(model_v2)

## see resulting death
final_table <- data.frame(get_counts(model_v1))
final_table <- final_table %>% filter(markov_cycle==80) %>% filter(state_names == "Death") %>% mutate(diff = count - lag(count, default = 0))
final_table[2, "diff"]

## other checks
final_table_PAP <- data.frame(get_counts(model_v1)) %>% filter(state_names == "CIN23") %>% filter(`.strategy_names` == "PAP")
final_table_HPV <- data.frame(get_counts(model_v1)) %>% filter(state_names == "CIN23") %>% filter(`.strategy_names` == "HPV")


#### Present results ####
plot(model_v1, type = "ce")
plot(model_v1, type = "counts", panel = "by_state")
# plot(model_v1, type = "counts", panel = "by_strategy")
# plot(model_v1, type ="values", panel = "by_value")




# #### DSA #####

def_dsa <- define_dsa(
age_base, 18, 35,
eW_pap, .68, 1,
eW_hpv, .74, 1,
eCIN23, 0.40,1,
eCIN1, 0.37, 1,
eICC1, 0.73,0.99,
eICC2, 0.47,0.78,
cW_hpv, 82.5, 137.5,
cW_pap, 96.16, 159.51,
cCIN1, 151, 609,
cCIN23, 329.53,1694.67,
cICC1, 13894, 23157.49,
cICC2, 19256.81, 32094.68,
cRem, 0, 113
)

res_dsa <- run_dsa(model_v1, dsa = def_dsa)
plot(res_dsa, result = "cost", strategy = c('PAP', "HPV"))
plot(res_dsa, result = "effect", strategy = c('PAP', "HPV"))
plot(res_dsa, result = "icer", type = "difference", shorten_labels = TRUE, limits_by_bars = FALSE, remove_ns = TRUE) + theme_bw()

plot(res_dsa, result = "icer", type = "difference")


# 
#### PSA ####

estBetaParams <- function(mu, var) {
  alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
  beta <- alpha * (1 / mu - 1)
  return(params = list(alpha = alpha, beta = beta))
}

def_psa1 <- define_psa(
age_base ~ normal(mean = 20, sd = 5),
cW_pap ~ poisson(127.84),
cW_hpv ~ poisson(110),
cCIN1 ~ poisson(151),
cCIN23 ~ poisson(897.62),
cICC1~ poisson(18525.99),
cICC2 ~ poisson(44201.74),
# eCIN1 ~ beta(79170000,7830000),  ## effect of CIN1
# eCIN23~beta(70600500,10549500),  ## effect CIN23
# eICC1~beta(71644375,4980625),  ## effect of ICC1 - treated
# eICC2~beta(29637097,9879032), ## effect of ICC2 - treated
# eRem~beta(78852571,5033143),
#eW_pap~beta(73800000,8200000),
#eW_hpv ~beta(97039800,980200)
eW_pap ~beta(21,3),
eW_hpv ~ beta(25,2),
eCIN1~beta(7,3),
eCIN23 ~beta(7,3),
eICC1 ~beta(10, 1),
eICC2 ~beta(100, 50)
)


res_psa <- run_psa(model_v1, psa = def_psa1, N = 5000)
plot(res_psa, type = "ac", max_wtp = 50000, log_scale = FALSE) 
plot(res_psa, type = "ce")

## get PSA data to make CE cloud w ggplot
res_psa_data <- res_psa[["psa"]]
res_psa_data1 <- res_psa_data %>% select("cost", "effect", ".strategy_names")
res_psa_data1_pap <- res_psa_data1 %>% filter(`.strategy_names` == "PAP")
res_psa_data1_hpv <- res_psa_data1 %>% filter(`.strategy_names` == "HPV")

res_psa_data1_pap <- res_psa_data1_pap %>% rename(Cost_pap = "cost")
res_psa_data1_pap <- res_psa_data1_pap %>% rename(effect_pap = "effect")

res_psa_data1_hpv <- res_psa_data1_hpv %>% rename(Cost_hpv = "cost")
res_psa_data1_hpv <- res_psa_data1_hpv %>% rename(effect_hpv = "effect")

res_psa_data1_pap <- res_psa_data1_pap %>% rename(strat_pap = ".strategy_names")
res_psa_data1_hpv <- res_psa_data1_hpv %>% rename(strat_hpv = ".strategy_names")

full_res <- res_psa_data1_hpv %>% cbind(res_psa_data1_pap)
full_res <- full_res %>% mutate(icer = (Cost_hpv-Cost_pap)/(effect_hpv-effect_pap))
full_res <- full_res %>% mutate(incremental_cost = (Cost_pap-Cost_hpv)/495777, incremental_effect = (effect_pap-effect_hpv)/495777)
full_res1 <- full_res %>% mutate(incremental_effect1 = rnorm(5000, 0, 2))
baseline_point <- data.frame(incremental_effect = 1.310486, incremental_cost =  1477.25)

full_res$fitted <- full_res$incremental_effect*50000
full_res$difference <- full_res$fitted - full_res$incremental_cost
full_res$below <- ifelse(full_res$difference > 0, 1,0)


## plot CE cloud (2 views)
plot_1 <- full_res %>% ggplot(aes(x = incremental_effect, y = incremental_cost))+geom_point()+
  theme_bw()+xlab("Incremental Effect")+ylab("Incremental Cost")+ggtitle("Probabilistic Sensitivity Analysis With WTP Threshold")+
  labs(subtitle = "Reference = Pap, WTP = $50,000")+
  geom_abline(intercept = 0, slope = 50000, color = "red")+
  geom_point(data = baseline_point, aes(x = incremental_effect, y = incremental_cost),color = "red", size = 3)+
  scale_y_continuous(limits = c(-10000, 50000))
ggsave("C-e plot psa1.png", height = 9, width = 9)

plot_2 <- full_res %>% ggplot(aes(x = incremental_effect, y = incremental_cost))+geom_point()+
  theme_bw()+ylab("Incremental Cost")+xlab("Incremental Effect")+ggtitle("Probabilistic Sensitivity Analysis: C-E Scatterplot")+
  labs(subtitle = "Reference = Pap")+
  geom_point(data = baseline_point, aes(x = incremental_effect, y = incremental_cost),color = "red", size = 3)
  #geom_abline(intercept = 0, slope = 50000, color = "red")

ggsave("C-e plot psa2.png", height = 9, width = 9)

library(cowplot)
plot_grid(plot_1, plot_2, labels = "AUTO")

