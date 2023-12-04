########################### Uni-Cox ####################################
library(glmnet)
library(survival)

perform_univariate_cox <- function(data, vars = NULL, name) {
  uni_result = data.frame()
  for(i in vars){
    fit <- try(cox <- coxph(Surv(YEAR_AFTER_LT, NODLT) ~ get(i), data = data), silent = FALSE)
    if("try-error"%in%class(fit)) {
      print("No observation for data analysis")
    }else{
      coxSummary = summary(cox)
      uni_result = rbind(uni_result,
                         cbind(id = i,
                               HR = coxSummary$conf.int[,"exp(coef)"],
                               LCI = coxSummary$conf.int[,"lower .95"],
                               UCI = coxSummary$conf.int[,"upper .95"],
                               P = coxSummary$coefficients[,"Pr(>|z|)"],
                               C =  coxSummary$concordance[1],
                               `C(se)` = coxSummary$concordance[2]
                         )
      )
    }
  }
  uni_result[,2:ncol(uni_result)] <- apply(uni_result[,2:ncol(uni_result)], 2, as.numeric)
  uni_result$BONF <- uni_result$P*dim(uni_result)[1]
  write.csv(uni_result, paste0("./COX/uni_result_",name,".csv"))
  return(uni_result)
}
# the whole cohort
vars_overall <- myVars[which(!myVars%in%Del_overall)]
uni_overall <- perform_univariate_cox(CLTC_OPTN, vars_overall, "overall")
# Ethnicity stratification
vars_White <- myVars[which(!myVars%in%Del_White)]
uni_White <- perform_univariate_cox(CLTC_OPTN[which(CLTC_OPTN$White_R == 1), ], vars_White, "White")
vars_Asian <- myVars[which(!myVars%in%Del_Asian)]
uni_Asian <- perform_univariate_cox(CLTC_OPTN[which(CLTC_OPTN$Asian_R == 1), ], vars_Asian, "Asian")
vars_Hispanic <- myVars[which(!myVars%in%Del_Hispanic)]
uni_Hispanic <- perform_univariate_cox(CLTC_OPTN[which(CLTC_OPTN$Hispanic_R == 1), ], vars_Hispanic, "Hispanic")
vars_African <- myVars[which(!myVars%in%Del_African)]
uni_African <- perform_univariate_cox(CLTC_OPTN[which(CLTC_OPTN$African_R == 1), ], vars_African, "African")
# Gender stratification
vars_Male <- myVars[which(!myVars%in%Del_Male)]
uni_Male <- perform_univariate_cox(CLTC_OPTN[which(CLTC_OPTN$Gender_R == 1),], vars_Male, "Male")
vars_Female <- myVars[which(!myVars%in%Del_Female)]
uni_Female <- perform_univariate_cox(CLTC_OPTN[which(CLTC_OPTN$Gender_R == 0),], vars_Female, "Female")
############################ Multi-Cox ####################################
perform_multi_cox <- function(orig_data, name, uni_result){
  uni_indicators <- uni_result$id[uni_result$BONF<0.05& !is.na(uni_result$BONF)]
  myfit1 <- coxph(as.formula(paste0("Surv(YEAR_AFTER_LT, NODLT) ~ ", paste(uni_indicators, collapse = "+"))), 
                  data = na.omit(orig_data[, c("YEAR_AFTER_LT", "NODLT", uni_indicators)]))
  mystep1 <- summary(MASS::stepAIC(myfit1, direction = "both"))
  myfit2 <- coxph(as.formula(paste0("Surv(YEAR_AFTER_LT, NODLT) ~ ", paste(rownames(mystep1$conf.int), collapse = "+"))), 
                  data = na.omit(orig_data[, c("YEAR_AFTER_LT", "NODLT", rownames(mystep1$conf.int))]))
  mystep2 <- summary(MASS::stepAIC(myfit2, direction = "both"))
  
  mystep2_res <- cbind(mystep2$conf.int, mystep2$coefficients[,c(4,5)]) 
  myfit3 <- summary(glm(as.formula(paste0("NODLT ~ ", paste(rownames(mystep2$conf.int), collapse = "+"))), 
                        data = na.omit(orig_data[, c("YEAR_AFTER_LT", "NODLT", rownames(mystep2$conf.int))]),
                        family = "binomial")
  ) 
  multi_result <- cbind(mystep2_res, myfit3$coefficients[-1, ])
  multi_result <- multi_result[,c(1, 3, 4, 6, 7, 8, 10)]
  colnames(multi_result) <- c("HR", "HR_LCI", "HR_UCI", "HR_P", "BETA_logit", "SE_logit", "P_logit")
  write.csv(multi_result, paste0("./COX/multi_result_",name,".csv"))
}
perform_multi_cox(CLTC_OPTN, "overall", uni_overall)
perform_multi_cox(CLTC_OPTN[which(CLTC_OPTN$White_R == 1), ], "White", uni_White)
perform_multi_cox(CLTC_OPTN[which(CLTC_OPTN$Asian_R == 1), ], "Asian", uni_Asian)
perform_multi_cox(CLTC_OPTN[which(CLTC_OPTN$Hispanic_R == 1), ], "Hispanic", uni_Hispanic)
perform_multi_cox(CLTC_OPTN[which(CLTC_OPTN$African_R == 1), ], "African", uni_African)
perform_multi_cox(CLTC_OPTN[which(CLTC_OPTN$Gender_R==1),], "Male", uni_Male)
perform_multi_cox(CLTC_OPTN[which(CLTC_OPTN$Gender_R==0),], "Female", uni_Female)
########################### Cox results #######################################
uni_var_overall <- uni_overall$id[uni_overall$BONF<0.05&!is.na(uni_overall$BONF)]
uni_var_White <- uni_White$id[uni_White$BONF<0.05&!is.na(uni_White$BONF)]
uni_var_Asian <- uni_Asian$id[uni_Asian$BONF<0.05&!is.na(uni_Asian$BONF)]
uni_var_Hispanic <- uni_Hispanic$id[uni_Hispanic$BONF<0.05&!is.na(uni_Hispanic$BONF)]
uni_var_African <- uni_African$id[uni_African$BONF<0.05&!is.na(uni_African$BONF)]
uni_var_Male <- uni_Male$id[uni_Male$BONF<0.05&!is.na(uni_Male$BONF)]
uni_var_Female <- uni_Female$id[uni_Female$BONF<0.05&!is.na(uni_Female$BONF)]

######################### Forest plot (example) ########################################
library(forestplot)
library(forestploter)
library(grid)
Data_str <- read.csv("White.csv")
Data_str$`HR (95%CI)` <- ifelse(is.na(Data_str$HR), "",
                                sprintf("%.3f (%.3f - %.3f)",
                                        Data_str$HR, Data_str$lower, Data_str$upper))

Data_str$` ` <- paste(rep(" ", 25), collapse = " ")

Data_str$P <- ifelse(is.na(Data_str$P), "", Data_str$P)

tm <- forest_theme(base_size = 9, 
                   ci_pch = 16, 
                   ci_col = "#4575b4",
                   ci_lty = 1, 
                   ci_lwd = 1.5,
                   ci_Theight = 0.2, 
                   refline_lwd = 1,
                   refline_lty = "dashed",
                   refline_col = "grey20",
                   arrow_length = 0.05,
                   arrow_cex = 0.7)

p1 <- forest(Data_str[, c(1,7,6,5)],
             est = Data_str$HR,
             lower = Data_str$lower,
             upper = Data_str$upper,
             ci_column = 2,
             ref_line = 1, 
             xlab = "Hazard Ratio",
             xlim = c(0.5,1.5),
             ticks_at = c(0.5,0.75,1,1.25,1.5),
             theme = tm)
p1 <- edit_plot(p1,
                row = c(1,17),    # subheading lines
                gp = gpar(fontface = "bold"))
p1 <- add_border(p1, part = "header", row = 2, where = "top")
p1