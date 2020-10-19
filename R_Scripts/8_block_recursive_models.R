#Comparing ces15 and ces19 Block Recursive Models
library(nnet)
library(broom)
library(purrr)

#First make a data frame 
ces15phone %>% 
  select(ndp, liberal, conservative, bloc, region3, working_class2, union_both, young, old, male, sector, catholic, no_religion, degree, foreign, low_income, high_income, language, 
         market_liberalism, moral_traditionalism, political_disaffection, continentalism, quebec_sovereignty, ndp_id, liberal_id, conservative_id, bloc_id, personal_retrospective, 
         national_retrospective, immigration_rate, environment, redistribution, defence, liberal_leader, conservative_leader, ndp_leader, bloc_leader)->out15
ces19phone %>% 
  select(ndp, liberal, conservative, bloc, region3, working_class2, union_both, young, old, male, sector, catholic, no_religion, degree, foreign, low_income, high_income, language, 
         market_liberalism, moral_traditionalism, political_disaffection, continentalism, quebec_sovereignty, ndp_id, liberal_id, conservative_id, bloc_id, personal_retrospective, 
         national_retrospective, immigration_rate, environment, redistribution, defence, liberal_leader, conservative_leader, ndp_leader, bloc_leader)->out19
out15$survey<-rep(0, nrow(out15))
out19$survey<-rep(1, nrow(out19))
out15 %>% 
  bind_rows(., out19)->out
out$survey
table(out$survey)

#Model 1
#### NDP ROC ####
out %>% 
  group_by(survey) %>% 
nest() %>% 
  mutate(
    block1=map(data, function(x) glm(ndp~region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income, family="binomial", data=x)), 
    block2=map(data, function(x) glm(ndp~region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism, family="binomial", data=x)),
    block3=map(data, function(x) glm(ndp~region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id, family="binomial", data=x)),
    block4=map(data, function(x) glm(ndp~region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective, family="binomial", data=x)),
    block5=map(data, function(x) glm(ndp~region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence, family="binomial", data=x)),
    block6=map(data, function(x) glm(ndp~region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence+liberal_leader+conservative_leader+ndp_leader, family="binomial", data=x))
    )->ndp_models_ROC
ndp_models_ROC %>% 
  mutate(block1=map(block1, tidy),
         block2=map(block2, tidy), 
         block3=map(block3, tidy),
         block4=map(block4, tidy),
         block5=map(block5, tidy),
         block6=map(block6, tidy)) ->ndp_models_ROC

#Get block 1
ndp_models_ROC %>% 
  unnest(block1) %>% 
  #Calculate the differences
  group_by(term) %>% 
  mutate(difference=estimate-lag(estimate)) %>% 
  filter(survey==1) %>% 
  ungroup() %>% 
  mutate(block=rep("block 1", nrow(.))) %>% 
  select(term, difference, block)->block1

#Get block 2
ndp_models_ROC %>% 
  #Be sure to get the right block here 
  unnest(block2) %>% 
  #Calculate the differences
  group_by(term) %>% 
  mutate(difference=estimate-lag(estimate)) %>% 
  filter(survey==1) %>% 
  ungroup() %>% 
  #Name the correct block here 
  mutate(block=rep("block 2", nrow(.))) %>% 
    select(term, difference, block)->block2

#Get block 3
ndp_models_ROC %>% 
  unnest(block3) %>% 
  #Calculate the differences
  group_by(term) %>% 
  mutate(difference=estimate-lag(estimate)) %>% 
  filter(survey==1) %>% 
  ungroup() %>% 
    #Name the correct block here 
  mutate(block=rep("block 3", nrow(.))) %>% 
  select(term, difference, block)->block3

#Get block 4
ndp_models_ROC %>% 
  unnest(block4) %>% 
  #Calculate the differences
  group_by(term) %>% 
  mutate(difference=estimate-lag(estimate)) %>% 
  filter(survey==1) %>% 
  ungroup() %>% 
  #Name the correct block here 
  mutate(block=rep("block 4", nrow(.))) %>% 
  select(term, difference, block)->block4

#Get block 5
ndp_models_ROC %>% 
  unnest(block5) %>% 
  #Calculate the differences
  group_by(term) %>% 
  mutate(difference=estimate-lag(estimate)) %>% 
  filter(survey==1) %>% 
  ungroup() %>% 
  #Name the correct block here 
  mutate(block=rep("block 5", nrow(.))) %>% 
  select(term, difference, block)->block5

#Get block 6
ndp_models_ROC %>% 
  unnest(block6) %>% 
  #Calculate the differences
  group_by(term) %>% 
  mutate(difference=estimate-lag(estimate)) %>% 
  filter(survey==1) %>% 
  ungroup() %>% 
  #Name the correct block here 
  mutate(block=rep("block 6", nrow(.))) %>% 
  select(term, difference, block)->block6

#Bind them together
library(knitr)
block1 %>% 
  #bind all the things together
  #the dot here just means what came before it (e.g. block 1)
  #then it binds all the following arguments to block 1 in successive order 
  bind_rows(., block2, block3, block4, block5, block6) %>% 
  #This next bit is to pick out the first occurrence of each term. 
  #We want to do this only to get the first time a variable is entered
  #Group by the term
  #So this creates a grouped data frame where each group is made up of all the variables with the same name (e.g. all the variables with the name 'young' ) are in one gtroup
  group_by(term)%>%
  #pick out the first occurrence of each term
  slice(1)%>%
  #Then we need to arrange these by block. 
  #I ran this without this and it seemed like the kable() caommand below was printing the table with the variables sorted alphabetically. We want them sorted by block
  #wE dumpt it out here into an object, because I think that was what was tripping you up.
  arrange(block)->NDP_ROC_block_difference_model #Name it whatever is most meanginful.

NDP_ROC_block_difference_model %>%
  kable()

#### NDP QC ####
out %>% 
  group_by(survey) %>% 
  nest() %>% 
  mutate(
    block1=map(data, function(x) glm(ndp~working_class2+union_both+young+old+male+degree+language+foreign, family="binomial", data=x)), 
    block2=map(data, function(x) glm(ndp~working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty, family="binomial", data=x)),
    block3=map(data, function(x) glm(ndp~working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id, family="binomial", data=x)),
    block4=map(data, function(x) glm(ndp~working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id+personal_retrospective+national_retrospective, family="binomial", data=x)),
    block5=map(data, function(x) glm(ndp~working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence, family="binomial", data=x)),
    block6=map(data, function(x) glm(ndp~working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence+liberal_leader+conservative_leader+ndp_leader+bloc_leader, family="binomial", data=x))
  )->ndp_models_QC
ndp_models_QC %>% 
  mutate(block1=map(block1, tidy),
         block2=map(block2, tidy), 
         block3=map(block3, tidy),
         block4=map(block4, tidy),
         block5=map(block5, tidy),
         block6=map(block6, tidy)) ->ndp_models_QC

#Get block 1
ndp_models_QC %>% 
  unnest(block1) %>% 
  #Calculate the differences
  group_by(term) %>% 
  mutate(difference=estimate-lag(estimate)) %>% 
  filter(survey==1) %>% 
  ungroup() %>% 
  mutate(block=rep("block 1", nrow(.))) %>% 
  select(term, difference, block)->block1

#Get block 2
ndp_models_QC %>% 
  #Be sure to get the right block here 
  unnest(block2) %>% 
  #Calculate the differences
  group_by(term) %>% 
  mutate(difference=estimate-lag(estimate)) %>% 
  filter(survey==1) %>% 
  ungroup() %>% 
  #Name the correct block here 
  mutate(block=rep("block 2", nrow(.))) %>% 
  select(term, difference, block)->block2

#Get block 3
ndp_models_QC %>% 
  unnest(block3) %>% 
  #Calculate the differences
  group_by(term) %>% 
  mutate(difference=estimate-lag(estimate)) %>% 
  filter(survey==1) %>% 
  ungroup() %>% 
  #Name the correct block here 
  mutate(block=rep("block 3", nrow(.))) %>% 
  select(term, difference, block)->block3

#Get block 4
ndp_models_QC %>% 
  unnest(block4) %>% 
  #Calculate the differences
  group_by(term) %>% 
  mutate(difference=estimate-lag(estimate)) %>% 
  filter(survey==1) %>% 
  ungroup() %>% 
  #Name the correct block here 
  mutate(block=rep("block 4", nrow(.))) %>% 
  select(term, difference, block)->block4

#Get block 5
ndp_models_QC %>% 
  unnest(block5) %>% 
  #Calculate the differences
  group_by(term) %>% 
  mutate(difference=estimate-lag(estimate)) %>% 
  filter(survey==1) %>% 
  ungroup() %>% 
  #Name the correct block here 
  mutate(block=rep("block 5", nrow(.))) %>% 
  select(term, difference, block)->block5

#Get block 6
ndp_models_QC %>% 
  unnest(block6) %>% 
  #Calculate the differences
  group_by(term) %>% 
  mutate(difference=estimate-lag(estimate)) %>% 
  filter(survey==1) %>% 
  ungroup() %>% 
  #Name the correct block here 
  mutate(block=rep("block 6", nrow(.))) %>% 
  select(term, difference, block)->block6

#Bind them together
block1 %>% 
  bind_rows(., block2, block3, block4, block5, block6) %>% 
  group_by(term)%>%
  slice(1)%>%
  arrange(block)->NDP_QC_block_difference_model

NDP_QC_block_difference_model %>%
  kable()

#Model 2
#### Liberal ROC ####
out %>% 
  group_by(survey) %>% 
  nest() %>% 
  mutate(
    block1=map(data, function(x) glm(liberal~region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income, family="binomial", data=x)), 
    block2=map(data, function(x) glm(liberal~region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism, family="binomial", data=x)),
    block3=map(data, function(x) glm(liberal~region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id, family="binomial", data=x)),
    block4=map(data, function(x) glm(liberal~region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective, family="binomial", data=x)),
    block5=map(data, function(x) glm(liberal~region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence, family="binomial", data=x)),
    block6=map(data, function(x) glm(liberal~region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence+liberal_leader+conservative_leader+ndp_leader, family="binomial", data=x))
  )->liberal_models_ROC
liberal_models_ROC %>% 
  mutate(block1=map(block1, tidy),
         block2=map(block2, tidy), 
         block3=map(block3, tidy),
         block4=map(block4, tidy),
         block5=map(block5, tidy),
         block6=map(block6, tidy)) ->liberal_models_ROC

#Get block 1
liberal_models_ROC %>% 
  unnest(block1) %>% 
  #Calculate the differences
  group_by(term) %>% 
  mutate(difference=estimate-lag(estimate)) %>% 
  filter(survey==1) %>% 
  ungroup() %>% 
  mutate(block=rep("block 1", nrow(.))) %>% 
  select(term, difference, block)->block1

#Get block 2
liberal_models_ROC %>% 
  #Be sure to get the right block here 
  unnest(block2) %>% 
  #Calculate the differences
  group_by(term) %>% 
  mutate(difference=estimate-lag(estimate)) %>% 
  filter(survey==1) %>% 
  ungroup() %>% 
  #Name the correct block here 
  mutate(block=rep("block 2", nrow(.))) %>% 
  select(term, difference, block)->block2

#Get block 3
liberal_models_ROC %>% 
  unnest(block3) %>% 
  #Calculate the differences
  group_by(term) %>% 
  mutate(difference=estimate-lag(estimate)) %>% 
  filter(survey==1) %>% 
  ungroup() %>% 
  #Name the correct block here 
  mutate(block=rep("block 3", nrow(.))) %>% 
  select(term, difference, block)->block3

#Get block 4
liberal_models_ROC %>% 
  unnest(block4) %>% 
  #Calculate the differences
  group_by(term) %>% 
  mutate(difference=estimate-lag(estimate)) %>% 
  filter(survey==1) %>% 
  ungroup() %>% 
  #Name the correct block here 
  mutate(block=rep("block 4", nrow(.))) %>% 
  select(term, difference, block)->block4

#Get block 5
liberal_models_ROC %>% 
  unnest(block5) %>% 
  #Calculate the differences
  group_by(term) %>% 
  mutate(difference=estimate-lag(estimate)) %>% 
  filter(survey==1) %>% 
  ungroup() %>% 
  #Name the correct block here 
  mutate(block=rep("block 5", nrow(.))) %>% 
  select(term, difference, block)->block5

#Get block 6
liberal_models_ROC %>% 
  unnest(block6) %>% 
  #Calculate the differences
  group_by(term) %>% 
  mutate(difference=estimate-lag(estimate)) %>% 
  filter(survey==1) %>% 
  ungroup() %>% 
  #Name the correct block here 
  mutate(block=rep("block 6", nrow(.))) %>% 
  select(term, difference, block)->block6

#Bind them together
block1 %>% 
  bind_rows(., block2, block3, block4, block5, block6) %>% 
  group_by(term)%>%
  slice(1)%>%
  arrange(block)->Liberal_ROC_block_difference_model

Liberal_ROC_block_difference_model %>%
  kable()

#### Liberal QC ####
out %>% 
  group_by(survey) %>% 
  nest() %>% 
  mutate(
    block1=map(data, function(x) glm(liberal~working_class2+union_both+young+old+male+degree+language+foreign, family="binomial", data=x)), 
    block2=map(data, function(x) glm(liberal~working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty, family="binomial", data=x)),
    block3=map(data, function(x) glm(liberal~working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id, family="binomial", data=x)),
    block4=map(data, function(x) glm(liberal~working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id+personal_retrospective+national_retrospective, family="binomial", data=x)),
    block5=map(data, function(x) glm(liberal~working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence, family="binomial", data=x)),
    block6=map(data, function(x) glm(liberal~working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence+liberal_leader+conservative_leader+ndp_leader+bloc_leader, family="binomial", data=x))
  )->liberal_models_QC
liberal_models_QC %>% 
  mutate(block1=map(block1, tidy),
         block2=map(block2, tidy), 
         block3=map(block3, tidy),
         block4=map(block4, tidy),
         block5=map(block5, tidy),
         block6=map(block6, tidy)) ->liberal_models_QC

#Get block 1
liberal_models_QC %>% 
  unnest(block1) %>% 
  #Calculate the differences
  group_by(term) %>% 
  mutate(difference=estimate-lag(estimate)) %>% 
  filter(survey==1) %>% 
  ungroup() %>% 
  mutate(block=rep("block 1", nrow(.))) %>% 
  select(term, difference, block)->block1

#Get block 2
liberal_models_QC %>% 
  #Be sure to get the right block here 
  unnest(block2) %>% 
  #Calculate the differences
  group_by(term) %>% 
  mutate(difference=estimate-lag(estimate)) %>% 
  filter(survey==1) %>% 
  ungroup() %>% 
  #Name the correct block here 
  mutate(block=rep("block 2", nrow(.))) %>% 
  select(term, difference, block)->block2

#Get block 3
liberal_models_QC %>% 
  unnest(block3) %>% 
  #Calculate the differences
  group_by(term) %>% 
  mutate(difference=estimate-lag(estimate)) %>% 
  filter(survey==1) %>% 
  ungroup() %>% 
  #Name the correct block here 
  mutate(block=rep("block 3", nrow(.))) %>% 
  select(term, difference, block)->block3

#Get block 4
liberal_models_QC %>% 
  unnest(block4) %>% 
  #Calculate the differences
  group_by(term) %>% 
  mutate(difference=estimate-lag(estimate)) %>% 
  filter(survey==1) %>% 
  ungroup() %>% 
  #Name the correct block here 
  mutate(block=rep("block 4", nrow(.))) %>% 
  select(term, difference, block)->block4

#Get block 5
liberal_models_QC %>% 
  unnest(block5) %>% 
  #Calculate the differences
  group_by(term) %>% 
  mutate(difference=estimate-lag(estimate)) %>% 
  filter(survey==1) %>% 
  ungroup() %>% 
  #Name the correct block here 
  mutate(block=rep("block 5", nrow(.))) %>% 
  select(term, difference, block)->block5

#Get block 6
liberal_models_QC %>% 
  unnest(block6) %>% 
  #Calculate the differences
  group_by(term) %>% 
  mutate(difference=estimate-lag(estimate)) %>% 
  filter(survey==1) %>% 
  ungroup() %>% 
  #Name the correct block here 
  mutate(block=rep("block 6", nrow(.))) %>% 
  select(term, difference, block)->block6

#Bind them together
block1 %>% 
  bind_rows(., block2, block3, block4, block5, block6) %>% 
  group_by(term)%>%
  slice(1)%>%
  arrange(block)->Liberal_QC_block_difference_model

Liberal_QC_block_difference_model %>%
  kable()

#Model 3
#### Conservative ROC ####
out %>% 
  group_by(survey) %>% 
  nest() %>% 
  mutate(
    block1=map(data, function(x) glm(conservative~region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income, family="binomial", data=x)), 
    block2=map(data, function(x) glm(conservative~region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism, family="binomial", data=x)),
    block3=map(data, function(x) glm(conservative~region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id, family="binomial", data=x)),
    block4=map(data, function(x) glm(conservative~region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective, family="binomial", data=x)),
    block5=map(data, function(x) glm(conservative~region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence, family="binomial", data=x)),
    block6=map(data, function(x) glm(conservative~region3+working_class2+union_both+young+old+male+sector+catholic+no_religion+degree+foreign+low_income+high_income+market_liberalism+moral_traditionalism+political_disaffection+continentalism+ndp_id+liberal_id+conservative_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence+liberal_leader+conservative_leader+ndp_leader, family="binomial", data=x))
  )->conservative_models_ROC
conservative_models_ROC %>% 
  mutate(block1=map(block1, tidy),
         block2=map(block2, tidy), 
         block3=map(block3, tidy),
         block4=map(block4, tidy),
         block5=map(block5, tidy),
         block6=map(block6, tidy)) ->conservative_models_ROC

#Get block 1
conservative_models_ROC %>% 
  unnest(block1) %>% 
  #Calculate the differences
  group_by(term) %>% 
  mutate(difference=estimate-lag(estimate)) %>% 
  filter(survey==1) %>% 
  ungroup() %>% 
  mutate(block=rep("block 1", nrow(.))) %>% 
  select(term, difference, block)->block1

#Get block 2
conservative_models_ROC %>% 
  #Be sure to get the right block here 
  unnest(block2) %>% 
  #Calculate the differences
  group_by(term) %>% 
  mutate(difference=estimate-lag(estimate)) %>% 
  filter(survey==1) %>% 
  ungroup() %>% 
  #Name the correct block here 
  mutate(block=rep("block 2", nrow(.))) %>% 
  select(term, difference, block)->block2

#Get block 3
conservative_models_ROC %>% 
  unnest(block3) %>% 
  #Calculate the differences
  group_by(term) %>% 
  mutate(difference=estimate-lag(estimate)) %>% 
  filter(survey==1) %>% 
  ungroup() %>% 
  #Name the correct block here 
  mutate(block=rep("block 3", nrow(.))) %>% 
  select(term, difference, block)->block3

#Get block 4
conservative_models_ROC %>% 
  unnest(block4) %>% 
  #Calculate the differences
  group_by(term) %>% 
  mutate(difference=estimate-lag(estimate)) %>% 
  filter(survey==1) %>% 
  ungroup() %>% 
  #Name the correct block here 
  mutate(block=rep("block 4", nrow(.))) %>% 
  select(term, difference, block)->block4

#Get block 5
conservative_models_ROC %>% 
  unnest(block5) %>% 
  #Calculate the differences
  group_by(term) %>% 
  mutate(difference=estimate-lag(estimate)) %>% 
  filter(survey==1) %>% 
  ungroup() %>% 
  #Name the correct block here 
  mutate(block=rep("block 5", nrow(.))) %>% 
  select(term, difference, block)->block5

#Get block 6
conservative_models_ROC %>% 
  unnest(block6) %>% 
  #Calculate the differences
  group_by(term) %>% 
  mutate(difference=estimate-lag(estimate)) %>% 
  filter(survey==1) %>% 
  ungroup() %>% 
  #Name the correct block here 
  mutate(block=rep("block 6", nrow(.))) %>% 
  select(term, difference, block)->block6

#Bind them together
block1 %>% 
  bind_rows(., block2, block3, block4, block5, block6) %>% 
  group_by(term)%>%
  slice(1)%>%
  arrange(block)->Conservative_ROC_block_difference_model

Conservative_ROC_block_difference_model %>%
  kable()

#### Conservative QC ####
out %>% 
  group_by(survey) %>% 
  nest() %>% 
  mutate(
    block1=map(data, function(x) glm(conservative~working_class2+union_both+young+old+male+degree+language+foreign, family="binomial", data=x)), 
    block2=map(data, function(x) glm(conservative~working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty, family="binomial", data=x)),
    block3=map(data, function(x) glm(conservative~working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id, family="binomial", data=x)),
    block4=map(data, function(x) glm(conservative~working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id+personal_retrospective+national_retrospective, family="binomial", data=x)),
    block5=map(data, function(x) glm(conservative~working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence, family="binomial", data=x)),
    block6=map(data, function(x) glm(conservative~working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence+liberal_leader+conservative_leader+ndp_leader+bloc_leader, family="binomial", data=x))
  )->conservative_models_QC
conservative_models_QC %>% 
  mutate(block1=map(block1, tidy),
         block2=map(block2, tidy), 
         block3=map(block3, tidy),
         block4=map(block4, tidy),
         block5=map(block5, tidy),
         block6=map(block6, tidy)) ->conservative_models_QC

#Get block 1
conservative_models_QC %>% 
  unnest(block1) %>% 
  #Calculate the differences
  group_by(term) %>% 
  mutate(difference=estimate-lag(estimate)) %>% 
  filter(survey==1) %>% 
  ungroup() %>% 
  mutate(block=rep("block 1", nrow(.))) %>% 
  select(term, difference, block)->block1

#Get block 2
conservative_models_QC %>% 
  #Be sure to get the right block here 
  unnest(block2) %>% 
  #Calculate the differences
  group_by(term) %>% 
  mutate(difference=estimate-lag(estimate)) %>% 
  filter(survey==1) %>% 
  ungroup() %>% 
  #Name the correct block here 
  mutate(block=rep("block 2", nrow(.))) %>% 
  select(term, difference, block)->block2

#Get block 3
conservative_models_QC %>% 
  unnest(block3) %>% 
  #Calculate the differences
  group_by(term) %>% 
  mutate(difference=estimate-lag(estimate)) %>% 
  filter(survey==1) %>% 
  ungroup() %>% 
  #Name the correct block here 
  mutate(block=rep("block 3", nrow(.))) %>% 
  select(term, difference, block)->block3

#Get block 4
conservative_models_QC %>% 
  unnest(block4) %>% 
  #Calculate the differences
  group_by(term) %>% 
  mutate(difference=estimate-lag(estimate)) %>% 
  filter(survey==1) %>% 
  ungroup() %>% 
  #Name the correct block here 
  mutate(block=rep("block 4", nrow(.))) %>% 
  select(term, difference, block)->block4

#Get block 5
conservative_models_QC %>% 
  unnest(block5) %>% 
  #Calculate the differences
  group_by(term) %>% 
  mutate(difference=estimate-lag(estimate)) %>% 
  filter(survey==1) %>% 
  ungroup() %>% 
  #Name the correct block here 
  mutate(block=rep("block 5", nrow(.))) %>% 
  select(term, difference, block)->block5

#Get block 6
conservative_models_QC %>% 
  unnest(block6) %>% 
  #Calculate the differences
  group_by(term) %>% 
  mutate(difference=estimate-lag(estimate)) %>% 
  filter(survey==1) %>% 
  ungroup() %>% 
  #Name the correct block here 
  mutate(block=rep("block 6", nrow(.))) %>% 
  select(term, difference, block)->block6

#Bind them together
block1 %>% 
  bind_rows(., block2, block3, block4, block5, block6) %>% 
  group_by(term)%>%
  slice(1)%>%
  arrange(block)->Conservative_QC_block_difference_model

Conservative_QC_block_difference_model %>%
  kable()

#### Bloc QC ####
out %>% 
  group_by(survey) %>% 
  nest() %>% 
  mutate(
    block1=map(data, function(x) glm(bloc~working_class2+union_both+young+old+male+degree+language+foreign, family="binomial", data=x)), 
    block2=map(data, function(x) glm(bloc~working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty, family="binomial", data=x)),
    block3=map(data, function(x) glm(bloc~working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id, family="binomial", data=x)),
    block4=map(data, function(x) glm(bloc~working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id+personal_retrospective+national_retrospective, family="binomial", data=x)),
    block5=map(data, function(x) glm(bloc~working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence, family="binomial", data=x)),
    block6=map(data, function(x) glm(bloc~working_class2+union_both+young+old+male+degree+language+foreign+market_liberalism+moral_traditionalism+political_disaffection+continentalism+quebec_sovereignty+ndp_id+liberal_id+conservative_id+bloc_id+personal_retrospective+national_retrospective+immigration_rate+environment+redistribution+defence+liberal_leader+conservative_leader+ndp_leader+bloc_leader, family="binomial", data=x))
  )->bloc_models_QC
bloc__models_QC %>% 
  mutate(block1=map(block1, tidy),
         block2=map(block2, tidy), 
         block3=map(block3, tidy),
         block4=map(block4, tidy),
         block5=map(block5, tidy),
         block6=map(block6, tidy)) ->bloc__models_QC

#Get block 1
bloc_models_QC %>% 
  unnest(block1) %>% 
  #Calculate the differences
  group_by(term) %>% 
  mutate(difference=estimate-lag(estimate)) %>% 
  filter(survey==1) %>% 
  ungroup() %>% 
  mutate(block=rep("block 1", nrow(.))) %>% 
  select(term, difference, block)->block1

#Get block 2
bloc_models_QC %>% 
  #Be sure to get the right block here 
  unnest(block2) %>% 
  #Calculate the differences
  group_by(term) %>% 
  mutate(difference=estimate-lag(estimate)) %>% 
  filter(survey==1) %>% 
  ungroup() %>% 
  #Name the correct block here 
  mutate(block=rep("block 2", nrow(.))) %>% 
  select(term, difference, block)->block2

#Get block 3
bloc_models_QC %>% 
  unnest(block3) %>% 
  #Calculate the differences
  group_by(term) %>% 
  mutate(difference=estimate-lag(estimate)) %>% 
  filter(survey==1) %>% 
  ungroup() %>% 
  #Name the correct block here 
  mutate(block=rep("block 3", nrow(.))) %>% 
  select(term, difference, block)->block3

#Get block 4
bloc_models_QC %>% 
  unnest(block4) %>% 
  #Calculate the differences
  group_by(term) %>% 
  mutate(difference=estimate-lag(estimate)) %>% 
  filter(survey==1) %>% 
  ungroup() %>% 
  #Name the correct block here 
  mutate(block=rep("block 4", nrow(.))) %>% 
  select(term, difference, block)->block4

#Get block 5
bloc_models_QC %>% 
  unnest(block5) %>% 
  #Calculate the differences
  group_by(term) %>% 
  mutate(difference=estimate-lag(estimate)) %>% 
  filter(survey==1) %>% 
  ungroup() %>% 
  #Name the correct block here 
  mutate(block=rep("block 5", nrow(.))) %>% 
  select(term, difference, block)->block5

#Get block 6
bloc_models_QC %>% 
  unnest(block6) %>% 
  #Calculate the differences
  group_by(term) %>% 
  mutate(difference=estimate-lag(estimate)) %>% 
  filter(survey==1) %>% 
  ungroup() %>% 
  #Name the correct block here 
  mutate(block=rep("block 6", nrow(.))) %>% 
  select(term, difference, block)->block6

#Bind them together
block1 %>% 
  bind_rows(., block2, block3, block4, block5, block6) %>% 
  group_by(term)%>%
  slice(1)%>%
  arrange(block)->Bloc_QC_block_difference_model

Bloc_QC_block_difference_model %>%
  kable()