
library(tidyverse)
## PAP testing

pAdhere_PAP<-.596
pNotAdhere_PAP<-1-pAdhere_PAP

pTestPos_PAP <- 0.055
pTestNeg_PAP <- 1- pTestPos_PAP

cAdhere_PAP<-100
cNoAdhere_PAP<-0

cTestPos_PAP <- 0
cTestNeg_PAP <-0

healthstate_transitions_testneg_pap <- data.frame("well"=0.7, "CIN1" = 0.019, "CIN23" = 0.00288, "ICC1" = 0.003003, "ICC2"= 0.001193, "death" = 0)
healthstate_transitions_testpos_pap <- data.frame("well"=.3, "CIN1" = 0.2698, "CIN23" = 0.04986, "ICC1" = 0.0426426, "ICC2"= 0.0169406, "death" = 0)
healthstate_transitions_notest_pap <- data.frame("well"=.7, "CIN1" = 0.184, "CIN23" = 0.078496, "ICC1" = 0.0426426, "ICC2"= 0.0169406, "death" = 0)

## HPV testing

pAdhere_HPV<-.596
pNotAdhere_HPV<-1-pAdhere_HPV

pTestPos_HPV <- 0.147
pTestNeg_HPV <- 1- pTestPos_HPV

cAdhere_HPV<-100
cNoAdhere_HPV<-0

cTestPos_HPV <- 0
cTestNeg_HPV <- 0

healthstate_transitions_testneg_hpv <- data.frame("well"=0.95, "CIN1" = 0.0608, "CIN23" = 0.00531, "ICC1" = 0.00354354, "ICC2"= 0.00140774, "death" = 0)
healthstate_transitions_testpos_hpv <- data.frame("well"=0.05, "CIN1" = 0.361, "CIN23" = 0.08514, "ICC1" = 0.057057, "ICC2"= 0.022667, "death" = 0)
healthstate_transitions_notest_hpv <- data.frame("well"=.7, "CIN1" = 0.184, "CIN23" = 0.078496, "ICC1" = 0.0426426, "ICC2"= 0.0169406, "death" = 0)




## Using functions


DecisionTree<-function(pAdhere,pNotAdhere,pTestPos,pTestNeg,cAdhere,cNoAdhere,cTestPos, cTestNeg, cohort_size, 
                       testpos_transitions, testneg_transitions, notest_transitions)
{
  cohort_adhere = cohort_size*pAdhere
  cohort_testpos = cohort_adhere*pTestPos
  cohort_testneg = cohort_adhere*pTestNeg
  cohort_notest = cohort_size*pNotAdhere
  
  cohort_pos_well = cohort_testpos*testpos_transitions[1,1]
  cohort_pos_CIN1 =cohort_testpos*testpos_transitions[1,2]
  cohort_pos_CIN23=cohort_testpos*testpos_transitions[1,3]
  cohort_pos_ICC1=cohort_testpos*testpos_transitions[1,4]
  cohort_pos_ICC2=cohort_testpos*testpos_transitions[1,5]
  cohort_pos_death=cohort_testpos*testpos_transitions[1,6]
  
  cohort_neg_well =cohort_testneg*notest_transitions[1,1]
  cohort_neg_CIN1 =cohort_testneg*notest_transitions[1,2]
  cohort_neg_CIN23=cohort_testneg*notest_transitions[1,3]
  cohort_neg_ICC1=cohort_testneg*notest_transitions[1,4]
  cohort_neg_ICC2=cohort_testneg*notest_transitions[1,5]
  cohort_neg_death=  cohort_testneg*notest_transitions[1,6]
  
  cohort_notest_well =cohort_notest*testneg_transitions[1,1]
  cohort_notest_CIN1 =cohort_notest*testneg_transitions[1,2]
  cohort_notest_CIN23=cohort_notest*testneg_transitions[1,3]
  cohort_notest_ICC1=cohort_notest*testneg_transitions[1,4]
  cohort_notest_ICC2=cohort_notest*testneg_transitions[1,5]
  cohort_notest_death=  cohort_notest*testneg_transitions[1,6]
  
  cohort_sizes = c(cohort_adhere,cohort_notest, cohort_testpos,cohort_testneg, cohort_pos_well,
                   cohort_pos_CIN1, cohort_pos_CIN23, cohort_pos_ICC1, cohort_pos_ICC2, cohort_pos_death,
                   cohort_neg_well, cohort_neg_CIN1, cohort_neg_CIN23, cohort_neg_ICC1, 
                   cohort_neg_ICC2, cohort_neg_death, cohort_notest_well, cohort_notest_CIN1, 
                   cohort_notest_CIN23, cohort_notest_ICC1, 
                   cohort_notest_ICC2, cohort_notest_death)
  
  cost_adhere = cAdhere
  cost_testpos = cAdhere+cTestPos
  cost_testneg = cAdhere+cTestNeg
  cost_notest = cNoAdhere
  cost_testneg_well=cAdhere+cTestNeg
  cost_testneg_cin1=cAdhere+cTestNeg
  cost_testneg_cin23=cAdhere+cTestNeg
  cost_testneg_icc1=cAdhere+cTestNeg
  cost_testneg_icc2= cAdhere+cTestNeg
  cost_testneg_death=cAdhere+cTestNeg
  
  cost_testpos_well=cAdhere+cTestPos
  cost_testpos_cin1=cAdhere+cTestPos
  cost_testpos_cin23= cAdhere+cTestPos
  cost_testpos_icc1=cAdhere+cTestPos
  cost_testpos_icc2=cAdhere+cTestPos
  cost_testpos_death=cAdhere+cTestPos
  
  cost_notest_well = cNoAdhere
  cost_notest_cin1 = cNoAdhere
  cost_notest_cin23 = cNoAdhere
  cost_notest_icc1 = cNoAdhere 
  cost_notest_icc2 = cNoAdhere
  cost_notest_death = cNoAdhere
  
  costs_indvidual = c(cost_adhere,cost_notest, cost_testpos, cost_testneg,
                      cost_testpos_well, cost_testpos_cin1, cost_testpos_cin23, 
                      cost_testpos_icc1, cost_testpos_icc2, cost_testpos_death, cost_testneg_well, 
                      cost_testneg_cin1, cost_testneg_cin23, cost_testneg_icc1, 
                      cost_testneg_icc2, cost_testneg_death, cost_notest_well, 
                      cost_notest_cin1, cost_notest_cin23, cost_notest_icc1, 
                      cost_notest_icc2, cost_notest_death)
  
  cost_adhere_pop = cost_adhere*cohort_adhere
  cost_testpos_pop = cost_testpos*cohort_testpos
  cost_testneg_pop = cost_testneg*cohort_testneg
  cost_notest_pop = cost_notest*cohort_notest
  
  cost_testneg_pop_well = cost_testneg*cohort_neg_well
  cost_testneg_pop_cin1 = cost_testneg*cohort_neg_CIN1
  cost_testneg_pop_cin23 = cost_testneg*cohort_neg_CIN23
  cost_testneg_pop_icc1 = cost_testneg*cohort_neg_ICC1
  cost_testneg_pop_icc2 = cost_testneg*cohort_neg_ICC2
  cost_testneg_pop_death = cost_testneg*cohort_neg_death
  
  cost_testpos_pop_well =cost_testpos*cohort_pos_well 
  cost_testpos_pop_cin1 = cost_testpos*cohort_pos_CIN1
  cost_testpos_pop_cin23 = cost_testpos*cohort_pos_CIN23
  cost_testpos_pop_icc1 = cost_testpos*cohort_pos_ICC1
  cost_testpos_pop_icc2 = cost_testpos*cohort_pos_ICC2
  cost_testpos_pop_death = cost_testpos*cohort_pos_death
  
  cost_notest_pop_well =cost_notest*cohort_notest_well 
  cost_notest_pop_cin1 = cost_notest*cohort_notest_CIN1
  cost_notest_pop_cin23 = cost_notest*cohort_notest_CIN23
  cost_notest_pop_icc1 = cost_notest*cohort_notest_ICC1
  cost_notest_pop_icc2 = cost_notest*cohort_notest_ICC2
  cost_notest_pop_death = cost_notest*cohort_notest_death
  
  costs_per_cohort = c(cost_adhere_pop, cost_notest_pop, cost_testpos_pop, cost_testneg_pop,
                       cost_testpos_pop_well, cost_testpos_pop_cin1, cost_testpos_pop_cin23, 
                       cost_testpos_pop_icc1, cost_testpos_pop_icc2, cost_testpos_pop_death, cost_testneg_pop_well,
                       cost_testneg_pop_cin1, cost_testneg_pop_cin23, cost_testneg_pop_icc1, cost_testneg_pop_icc2,
                       cost_testneg_pop_death, cost_notest_pop_well,
                       cost_notest_pop_cin1, cost_notest_pop_cin23, 
                       cost_notest_pop_icc1, cost_notest_pop_icc2,
                       cost_notest_pop_death)
  
  Results<-data.frame("Population" = c("Adhere","No Adherence", "Adhere","Adhere", 
                                       "Well", "CIN1", "CIN23", "ICC1", "ICC2", 
                                       "Death", "Well", "CIN1", "CIN23", "ICC1", "ICC2", 
                                       "Death", "Well", "CIN1", "CIN23", "ICC1", "ICC2", 
                                       "Death"), 
                      "Test Result" = c("Initial", "Initial", "Positive", "Negative", "Positive", "Positive", "Positive", 
                                        "Positive", "Positive", "Positive", "Negative", "Negative", "Negative",
                                        "Negative", "Negative", "Negative", "No Test","No Test","No Test",
                                        "No Test","No Test","No Test"),
                      "cohort_size"=cohort_sizes, 
                      "cost_indvidual"=costs_indvidual, "costs_per_cohort" = costs_per_cohort)
  
  return(Results)
}


Results_PAP<-DecisionTree(pAdhere_PAP,pNotAdhere_PAP,pTestPos_PAP,pTestNeg_PAP,cAdhere_PAP,cNoAdhere_PAP,cTestPos_PAP, cTestNeg_PAP, 
                          cohort_size = 495777, testpos_transitions = healthstate_transitions_testpos_pap, 
                          testneg_transitions = healthstate_transitions_testneg_pap,
                          notest_transitions= healthstate_transitions_notest_pap)

Results_HPV<-DecisionTree(pAdhere_HPV,pNotAdhere_HPV,pTestPos_HPV,pTestNeg_HPV,cAdhere_HPV,cNoAdhere_HPV,
                          cTestPos_HPV, cTestNeg_HPV, cohort_size = 495777,
                          testpos_transitions = healthstate_transitions_testpos_hpv, 
                          testneg_transitions = healthstate_transitions_testneg_hpv,
                          notest_transitions= healthstate_transitions_notest_hpv)

Results_PAP1 <- Results_PAP %>% group_by(Population) %>% summarize(by_state = sum(cohort_size), percent = by_state/495777) 

Results_HPV1 <- Results_HPV %>% group_by(Population) %>% summarize(by_state = sum(cohort_size), percent = by_state/495777)


Results_PAP2 <- Results_PAP %>% filter(Test.Result != "No Test") %>% filter(! Population %in% c("Adhere", 'No Adherence')) %>%group_by(Population) %>% summarize(by_state = sum(cohort_size), percent = by_state/495777) 
Results_HPV2 <- Results_HPV %>% filter(Test.Result != "No Test") %>% filter(! Population %in% c("Adhere", 'No Adherence')) %>%group_by(Population) %>% summarize(by_state = sum(cohort_size), percent = by_state/495777) 

library(openxlsx)

#dataset_names <- list('cohort_size_PAP' = Results_PAP, 'cohort_size_HPV' = Results_HPV, 'cohort_size_PAP_combined' = Results_PAP2, 'cohort_size_HPV_combined' = Results_HPV2)

#openxlsx::write.xlsx(dataset_names, file = 'DecisionTree_Results_60adherence.xlsx', overwrite = TRUE) 

## re-do based on decision tree
pap_decisiontree_transition <- readxl::read_xlsx("By age Transition Probabilities.xlsx", sheet = "PAP_decisiontree")
hpv_decisiontree_transition <- readxl::read_xlsx("By age Transition Probabilities.xlsx", sheet = "HPV_decisiontree")
pap_normal_transition <- readxl::read_xlsx("By age Transition Probabilities.xlsx", sheet = "PAP_normal")
hpv_normal_transition <- readxl::read_xlsx("By age Transition Probabilities.xlsx", sheet = "HPV_normal")
hpv_beforetest_transition <- readxl::read_xlsx("By age Transition Probabilities.xlsx", sheet = "HPV_beforetesting")


pap_decisiontree_transition[,"pWelltoCIN1"] <- Results_PAP2[1, "percent"]
pap_decisiontree_transition[,"pWelltoCIN23"] <- Results_PAP2[2, "percent"]
pap_decisiontree_transition[,"pWelltoICC1"] <- Results_PAP2[4, "percent"]
pap_decisiontree_transition[,"pWelltoICC2"] <- Results_PAP2[5, "percent"]


hpv_decisiontree_transition[,"pWelltoCIN1"] <- Results_HPV2[1, "percent"]
hpv_decisiontree_transition[,"pWelltoCIN23"] <- Results_HPV2[2, "percent"]
hpv_decisiontree_transition[,"pWelltoICC1"] <- Results_HPV2[4, "percent"]
hpv_decisiontree_transition[,"pWelltoICC2"] <- Results_HPV2[5, "percent"]

dataset_names1 <- list('PAP_normal' = pap_normal_transition, 'HPV_normal' = hpv_normal_transition, 
                      'HPV_beforetesting' = hpv_beforetest_transition, 'PAP_decisiontree' = pap_decisiontree_transition,
                      "HPV_decisiontree" = hpv_decisiontree_transition )

openxlsx::write.xlsx(dataset_names1, file = 'By age Transition Probabilities.xlsx', overwrite = TRUE)
