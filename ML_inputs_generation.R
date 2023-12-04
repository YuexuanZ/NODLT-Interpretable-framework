colname <- colnames(CLTC_OPTN)
for (i in c(1:length(colname))){
  CLTC_OPTN[which(CLTC_OPTN[,colname[i]]==""),i]<-NA   
}

Generate_data_Tree <- function(data, var, cox_type, name){
  selected_data <- data[,c(var,"NODLT")]
  data_na <- selected_data
  na_index<-numeric(0)
  for (i in c(1:nrow(data_na))){                                                       
    if (length(which(is.na(data_na[i,])))>0)
      na_index<-append(na_index,i)}
  data_na <- selected_data[-na_index,]
  fwrite(data_na, paste0("./Data_for_ML/Input_",cox_type,"_",name,".csv"))
  return (data_na)
}

Input_uni_overall <- Generate_data_Tree(CLTC_OPTN, uni_var_overall, "uni", "overall")
Input_uni_White <- Generate_data_Tree(CLTC_OPTN[which(CLTC_OPTN$White_R == 1), ], uni_var_White, "uni", "White")
Input_uni_Hispanic <- Generate_data_Tree(CLTC_OPTN[which(CLTC_OPTN$Hispanic_R == 1), ], uni_var_Hispanic, "uni", "Hispanic")
Input_uni_Asian <- Generate_data_Tree(CLTC_OPTN[which(CLTC_OPTN$Asian_R == 1), ], uni_var_Asian, "uni", "Asian")
Input_uni_African <- Generate_data_Tree(CLTC_OPTN[which(CLTC_OPTN$African_R == 1), ], uni_var_African, "uni", "African")
Input_uni_Male <- Generate_data_Tree(CLTC_OPTN[which(CLTC_OPTN$Gender_R == 1),], uni_var_Male, "uni_P", "Male")
Input_uni_Female <- Generate_data_Tree(CLTC_OPTN[which(CLTC_OPTN$Gender_R == 0),], uni_var_Female, "uni_P", "Female")