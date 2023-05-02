###This part is a cyclic calculation to train the model with 1000 different penalties

#1000 different penaltis
penalty_grid <- grid_regular(
  penalty(range = c(-3, 3)), 
  levels = 1000)

list<-list()
list2<-list()

n=0

repeat{
  n<- n+1
  n1<- n/10
  print(n1)
  best_penalty <- penalty_grid[n,1]
  #autoplot(tune_output) + theme_classic()+
    #geom_vline(xintercept = best_penalty %>% pull(penalty), linetype = 'solid',col="red")
  final_wf <- finalize_workflow(lasso_wf_tune, best_penalty) 
  final_fit <- fit(final_wf, data = data)
  tidy(final_fit) #penalty 
  (v <- final_fit %>% tidy())
  sum(v$estimate!=0) 
  v[v$estimate!=0,]
  v$term[v$estimate!=0]
  
  glmnet_output <- final_fit %>% extract_fit_engine()
 
  
  bool_predictor_exclude <- glmnet_output$beta==0
  
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
  glmnet_output <- final_fit %>% extract_fit_parsnip() %>% pluck('fit') 
  lambdas <- glmnet_output$lambda
  coefs_lambdas <- 
    coefficients(glmnet_output, s = lambdas )  %>% 
    as.matrix() %>%  
    t() %>% 
    as.data.frame() %>% 
    mutate(lambda = lambdas ) %>% 
    select(lambda, everything(), -`(Intercept)`) 
  
  #Sum
  final_fit %>% tidy() %>% filter(estimate != 0) #estimate score through RNA-seq data;tidy back ??
  lasso_mod_out <- final_fit%>% 
    predict(new_data = data) %>% #predict
    bind_cols(data) %>% 
    mutate(resid = joa - .pred) #create a new list
  
  
  sst <- sum((data$joa-mean(data$joa))^2) #sum((lasso_mod_out$joa-mean(lasso_mod_out$joa))^2)
  sse <- sum(lasso_mod_out$resid^2)
  (r <- 1- (sse/sst))
  sum(v$estimate!=0)
  x<-sum(v$estimate!=0)
  
  list[n]<-r #R^2
  list2[n]<-x #number of protein variables +1
  
  if (n > 999) {
    break
  }  
}

##Save the data to Excel

library(openxlsx)
## Create a new workbook
wb <- createWorkbook()
## Add 3 worksheets
addWorksheet(wb, sheetName = "r")
addWorksheet(wb, sheetName = "x") 
addWorksheet(wb, sheetName = "penalty") 
## create the label of the data in order to save them next

## Write data 
writeData(wb, sheet = 1, list)
writeData(wb, sheet = 2, list2)
writeData(wb, sheet = 3, penalty_grid )

## Save the data in excel
saveWorkbook(wb, "q.xlsx", overwrite = FALSE)

