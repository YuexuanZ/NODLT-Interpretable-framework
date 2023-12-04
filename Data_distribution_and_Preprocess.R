library(readxl)
library(data.table)
library(dplyr)
setwd("D:/NODLT")
CLTC_OPTN <- read.csv("origdata.csv",header=T)
######################### Missing values & Major classes #############################
## Major classes
library(tableone)
perform_tableone <- function(AllVar, CatVar, data, strata=NULL, name=NULL){ 
  if (is.null(strata)){   
    tab0 <- CreateTableOne(vars = AllVar, data = data, factorVars = CatVar)
    tab0Mat <- print(tab0, showAllLevels = TRUE,quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
    write.csv(tab0Mat, file = paste0("./1_Major_class/Tableone_",name,".csv"))
  }
  else{
    tab0 <- CreateTableOne(vars = AllVar, strata = strata , data = data, factorVars = CatVar)
    tab0Mat <- print(tab0, showAllLevels = TRUE,quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
    write.csv(tab0Mat, file = paste0("./1_Major_class/Tableone_",strata,"_",name,".csv"))
  }
}
myVars <- c("Age_R","Age_D","Gender_R","Gender_D","BMI_R","BMI_D","ALB_R","TBILI_R","TBILI_D",
            "Crea_R","Crea_D","Na_R","INR_R","MELD_R","HT_D","HBV_R","HBV_D","HCV_R",
            "HCV_D","IMMUNO","Age_diff","Age_diff_ABS",
            "Diab_D","African_R","Hispanic_R","Asian_R","White_R","African_D","Hispanic_D","Asian_D","White_D",
            "Ascites_R","HE_R","ALF_R","HCVcirr_R","ALD_R","Choles_R","HCC_R","SameRace_D",
            "Urea_D","CIT","HBVcirr_R"
)
catVars <- c("Gender_R","Gender_D","HT_D","HBV_R","HBV_D","HCV_R","HCV_D","IMMUNO","Diab_D","African_R","Hispanic_R","Asian_R","White_R",
             "African_D","Hispanic_D","Asian_D","White_D",
             "Ascites_R","HE_R","ALF_R","HCVcirr_R","ALD_R","Choles_R","HCC_R","SameRace_D","HBVcirr_R")

perform_tableone(myVars, catVars, CLTC_OPTN, strata="NODLT", name="overall")
perform_tableone(myVars, catVars, CLTC_OPTN, name="overall")

perform_tableone(myVars, catVars, CLTC_OPTN[which(CLTC_OPTN$White_R == 1), ], name="White")
perform_tableone(myVars, catVars, CLTC_OPTN[which(CLTC_OPTN$Hispanic_R == 1), ], name="Hispanic")
perform_tableone(myVars, catVars, CLTC_OPTN[which(CLTC_OPTN$Asian_R == 1), ], name="Asian")
perform_tableone(myVars, catVars, CLTC_OPTN[which(CLTC_OPTN$African_R == 1), ], name="African")

perform_tableone(myVars, catVars, CLTC_OPTN[which(CLTC_OPTN$Gender_R == 1), ], name="Male")
perform_tableone(myVars, catVars, CLTC_OPTN[which(CLTC_OPTN$Gender_R == 0), ], name="Female")

perform_tableone(myVars, catVars, CLTC_OPTN[which(CLTC_OPTN$White_R == 1), ], strata="NODLT", name="White")
perform_tableone(myVars, catVars, CLTC_OPTN[which(CLTC_OPTN$Hispanic_R == 1), ], strata="NODLT",name="Hispanic")
perform_tableone(myVars, catVars, CLTC_OPTN[which(CLTC_OPTN$Asian_R == 1), ], strata="NODLT",name="Asian")
perform_tableone(myVars, catVars, CLTC_OPTN[which(CLTC_OPTN$African_R == 1), ], strata="NODLT",name="African")

perform_tableone(myVars, catVars, CLTC_OPTN[which(CLTC_OPTN$Gender_R == 1), ], strata="NODLT",name="Male")
perform_tableone(myVars, catVars, CLTC_OPTN[which(CLTC_OPTN$Gender_R == 0), ], strata="NODLT",name="Female")

## Missing values
colname <- colnames(CLTC_OPTN)
for (i in c(1:length(colname))){
  CLTC_OPTN[which(CLTC_OPTN[,colname[i]]==""),i]<-NA  
}
perform_NA_nums <- function(input_data, name) {
  col_na <- data.frame(col_name=0,na_num=0)    
  for (j in 1:ncol(input_data)){
    na <- sum(is.na(input_data[,j]))
    col_na[j,1] <- colnames(input_data)[j]
    col_na[j,2] <- na
  }
  col_na$threshold_20 <- nrow(input_data)*0.2
  col_na$threshold_30 <- nrow(input_data)*0.3
  col_na$delete_20 <- 0
  col_na$delete_20[which(col_na$na_num >= nrow(input_data)*0.2)] <- 1
  col_na$delete_30 <- 0
  col_na$delete_30[which(col_na$na_num >= nrow(input_data)*0.3)] <- 1
  fwrite(col_na, paste0("./2_NA/NA_",name,".csv"))
  return (col_na)
}
NA_overall <- perform_NA_nums(CLTC_OPTN,"Overall")
NA_WHITE <- perform_NA_nums(CLTC_OPTN[which(CLTC_OPTN$White_R == 1), ],"White")
NA_LATINO <- perform_NA_nums(CLTC_OPTN[which(CLTC_OPTN$Hispanic_R == 1), ],"Hispanic")
NA_ASIAN <- perform_NA_nums(CLTC_OPTN[which(CLTC_OPTN$Asian_R == 1), ],"Asian")
NA_BLACK <- perform_NA_nums(CLTC_OPTN[which(CLTC_OPTN$African_R == 1), ],"African")

NA_MALE <- perform_NA_nums(CLTC_OPTN[which(CLTC_OPTN$Gender_R == 1), ],"Male")
NA_MALE <- perform_NA_nums(CLTC_OPTN[which(CLTC_OPTN$Gender_R == 0), ],"Female")

## The characteristics to be deleted
Del_overall <- c("HBV_R","HBV_D","HCV_D","IMMUNO","Diab_D","African_R","Asian_R",
                 "Asian_D","ALF_R","HBVcirr_R")
Del_White <- c("Na_R","HBV_R","HBV_D","HCV_D","IMMUNO","Diab_D","Asian_D","ALF_R","HBVcirr_R")
Del_Asian <- c("HBV_D","HCV_D","IMMUNO","Diab_D","ALD_R","Choles_R")
Del_Hispanic <- c("HBV_R","HBV_D","HCV_D","IMMUNO","Asian_D","ALF_R","Choles_R","HBVcirr_R")
Del_African <- c("HBV_R","HBV_D","HCV_D","IMMUNO","Diab_D","Asian_D","HBVcirr_R")
Del_Male <- c("HCV_D","African_R","Asian_R","HBV_R","ALF_R","Choles_R","HBVcirr_R","IMMUNO","Asian_D",
              "HBV_D")
Del_Female <- c("Na_R","HCV_D","Asian_R","HBV_R","ALF_R","HBVcirr_R","IMMUNO","Asian_D","Diab_D","HBV_D")