rm(list=ls())  #Clean the environment
setwd("/Users/corgi/Desktop") 
library(dplyr) #Import 'dplyr' package
library(sampling)
library(showtext)
library(readr) #Import 'readr' package 
library(broom) #Import 'broom' package
library(ggplot2) #Import 'ggplot2' package
library(tidymodels) #Import 'tidymodels' package
tidymodels_prefer() #prefer 'tidymodels' first if conflict
set.seed(1234)  #Set random number
CFPD <- read_csv("/") #The input format is the training set and testing set of csv
CFPD4 <- read_csv("/")#The input format is the validation set of csv


repl3<-c(2,3,4,5,6,7,10,12,13,17,18,19,21,22,24,26,29,30,34,35,36,39,40)#Non-Cancerous,change it into the data you want
repl4<-c(1,8,9,11,14,15,16,20,23,25,27,28,31,32,33,37,38,41,42,43)#Cancerous,,change it into the data you want

CFPD2 <- CFPD #change CFPD2 into total population dataset
CFPD2 <- CFPD[repl3,]#change CFPD2 into cancerous dataset
CFPD2 <- CFPD[repl4,]#change CFPD2 into cancerous dataset
CFPD2 <- CFPD2[,colSums(is.na(CFPD))==0] 
head(CFPD2) #Return the first or last part

CFPD3 <-CFPD #change CFPD2 into all population dataset
CFPD3 <-  CFPD3[,colSums(is.na(CFPD))==0] 
head(CFPD3) 


data <- CFPD2 %>% mutate(joa = JOA1)%>%select(-c(JOA1,JOA2,Tomour_group,Age,gender,EMS))
data2 <- CFPD3 %>% mutate(joa = JOA1)%>%select(-c(JOA1,JOA2,Tomour_group,Age,gender,EMS))


(colnames(data))[1:10] 
rev(colnames(data))[1:10] 

full_rec <- recipe(joa ~ ., data = data) %>% 
  update_role(ID, new_role = 'ID') %>% 
  step_nzv(all_predictors()) %>% 
  step_normalize(all_numeric_predictors()) %>% # LASSO normalize 
  step_dummy(all_nominal_predictors())  

# Create CV folds 
set.seed(818) 
data_cv10 <- vfold_cv(data, v = 10)

# Lasso Model Spec with tune 
lm_lasso_spec_tune <- 
  linear_reg() %>%
  set_args(mixture = 1, penalty = tune()) %>% 
  set_engine(engine = 'glmnet') %>% 
  set_mode('regression') 

# Workflow (Recipe + Model) 
lasso_wf_tune <- workflow() %>% 
  add_recipe(full_rec) %>% 
  add_model(lm_lasso_spec_tune) 

# Tune Model
penalty_grid <- grid_regular(
  penalty(range = c(-3, 3)), 
  levels = 1000)

tune_output <- tune_grid( 
  lasso_wf_tune, 
  resamples = data_cv10, # cv folds
  metrics = metric_set(rmse, mae), 
  grid = penalty_grid 
) 


# Visualize Model Evaluation Metrics from Tuning
autoplot(tune_output) + theme_classic()

best_penalty <- select_best(tune_output, metric = 'mae', desc(penalty))
best_penalty   
best_penalty<- ??? #change it into the penalty you want

#another 
best_se_penalty <- select_by_one_std_err(tune_output, metric = 'mae', desc(penalty)) 
best_se_penalty

#Visualize again and mark out the penalty we choose
autoplot(tune_output) + theme_classic()+
  geom_vline(xintercept = best_penalty %>% pull(penalty), linetype = 'solid',col="red")

#RMSE mean and visualize
metrics_output <- collect_metrics(tune_output) %>%
  filter(.metric == 'rmse') 
metrics_output %>% ggplot(aes(x=penalty,y=mean))+geom_line()+
  xlim(0,1)+
  geom_vline(xintercept = best_penalty %>% pull(penalty), linetype = 'solid',col="red")

#MSE mean and visualize
metrics_output1 <- collect_metrics(tune_output) %>%
  filter(.metric == 'mae') 
metrics_output1 %>% ggplot(aes(x=penalty,y=mean))+geom_line()+
  xlim(0,1)+
  geom_vline(xintercept = best_penalty %>% pull(penalty), linetype = 'solid',col="red")




# Fit Final Model 

final_wf <- finalize_workflow(lasso_wf_tune, best_penalty) 
final_fit <- fit(final_wf, data = data)
 
tidy(final_fit) #penalty 
 
(v <- final_fit %>% tidy())
 
sum(v$estimate!=0) 
v[v$estimate!=0,]
v$term[v$estimate!=0]
 
glmnet_output <- final_fit %>% extract_fit_engine()


bool_predictor_exclude <- glmnet_output$beta==0



#Variables Importance score
var_imp <- sapply(seq_len(nrow(bool_predictor_exclude)), function(row) { 
  this_coeff_path <- bool_predictor_exclude[row,]
  if(sum(this_coeff_path) == ncol(bool_predictor_exclude)){ return(0)}else{
    return(ncol(bool_predictor_exclude) - which.min(this_coeff_path) + 1)}
})

var_imp_data <- tibble( 
  var_name = rownames(bool_predictor_exclude),
  var_imp = var_imp
)

(vimf <- var_imp_data %>% arrange(desc(var_imp)))

#show the best 10 variables
(top10 <- vimf[1:10,])

glmnet_output <- final_fit %>% extract_fit_parsnip() %>% pluck('fit') 


lambdas <- glmnet_output$lambda

coefs_lambdas <- 
  coefficients(glmnet_output, s = lambdas )  %>% 
  as.matrix() %>%  
  t() %>% 
  as.data.frame() %>% 
  mutate(lambda = lambdas ) %>% 
  select(lambda, everything(), -`(Intercept)`) 

###add
m<-list(v[v$estimate!=0,][,1])

coefs_lambdas <- coefs_lambdas %>% select(lambda,???)%>% 
  pivot_longer(cols = -lambda, 
               names_to = "term", 
               values_to = "coefficients") %>%
  mutate(var = purrr::map_chr(stringr::str_split(term,"_"),~.[1])) 

#gname <- paste0("./Coefficients_Path.eps") 
 #postscript(gname,width=6,height=6,horizontal = FALSE, onefile = FALSE, paper = "special") 
par(mfrow=c(1,1),oma=c(0.2,1.5,2,1.5),mar=c(3,2,0.2,2),cex.axis=1,las=1,mgp=c(1,0.5,0),adj=0.5) 
#Visualize
coefs_lambdas %>%
  ggplot(aes(x = lambda, y = coefficients, group = term, color = var)) + 
  geom_line() +
  geom_vline(xintercept = best_penalty %>% pull(penalty),col="black",linetype = 'dashed') +
  theme_classic() +
   xlim(0,0.5)+ 
  theme(legend.position = "bottom", legend.text=element_text(size=8)) 
#dev.off() 

#Sum

final_fit %>% tidy() %>% filter(estimate != 0) #estimate score through RNA-seq data;tidy back ??
lasso_mod_out <- final_fit%>% 
  predict(new_data = data) %>% #predict
  bind_cols(data) %>% 
  mutate(resid = joa - .pred) #create a new list


final_fit %>% tidy() %>% filter(estimate != 0)
lasso_mod_out2 <- final_fit%>% 
  predict(new_data = data2) %>% #predict
  bind_cols(data2) %>%
  mutate(resid = joa - .pred) #create a new list

sst <- sum((data$joa-mean(data$joa))^2) #sum((lasso_mod_out$joa-mean(lasso_mod_out$joa))^2)
sse <- sum(lasso_mod_out$resid^2)
(r <- 1- (sse/sst))#R^2
sum(v$estimate!=0)#nunber of variables+1(the one is lambdas)