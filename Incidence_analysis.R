##The incidence of NODLT during 10 years of follow-up for four stratification
# Donor type; Age group; Gender; Ethnicity
library(readxl)
library(data.table)
library(dplyr)
CLTC_OPTN$IS_1YEAR <- 0
CLTC_OPTN$IS_1YEAR[which(CLTC_OPTN$NODLT == 1 & CLTC_OPTN$YEAR_AFTER_LT > 0.1 & CLTC_OPTN$YEAR_AFTER_LT <=1 )] <- 1
CLTC_OPTN$IS_2YEAR <- 0
CLTC_OPTN$IS_2YEAR[which(CLTC_OPTN$NODLT == 1 & CLTC_OPTN$YEAR_AFTER_LT > 0.1 & CLTC_OPTN$YEAR_AFTER_LT <=2 )] <- 1
CLTC_OPTN$IS_3YEAR <- 0
CLTC_OPTN$IS_3YEAR[which(CLTC_OPTN$NODLT == 1 & CLTC_OPTN$YEAR_AFTER_LT > 0.1 & CLTC_OPTN$YEAR_AFTER_LT <=3 )] <- 1
CLTC_OPTN$IS_4YEAR <- 0
CLTC_OPTN$IS_4YEAR[which(CLTC_OPTN$NODLT == 1 & CLTC_OPTN$YEAR_AFTER_LT > 0.1 & CLTC_OPTN$YEAR_AFTER_LT <=4 )] <- 1
CLTC_OPTN$IS_5YEAR <- 0
CLTC_OPTN$IS_5YEAR[which(CLTC_OPTN$NODLT == 1 & CLTC_OPTN$YEAR_AFTER_LT > 0.1 & CLTC_OPTN$YEAR_AFTER_LT <=5 )] <- 1
CLTC_OPTN$IS_6YEAR <- 0
CLTC_OPTN$IS_6YEAR[which(CLTC_OPTN$NODLT == 1 & CLTC_OPTN$YEAR_AFTER_LT > 0.1 & CLTC_OPTN$YEAR_AFTER_LT <=6 )] <- 1
CLTC_OPTN$IS_7YEAR <- 0
CLTC_OPTN$IS_7YEAR[which(CLTC_OPTN$NODLT == 1 & CLTC_OPTN$YEAR_AFTER_LT > 0.1 & CLTC_OPTN$YEAR_AFTER_LT <=7 )] <- 1
CLTC_OPTN$IS_8YEAR <- 0
CLTC_OPTN$IS_8YEAR[which(CLTC_OPTN$NODLT == 1 & CLTC_OPTN$YEAR_AFTER_LT > 0.1 & CLTC_OPTN$YEAR_AFTER_LT <=8 )] <- 1
CLTC_OPTN$IS_9YEAR <- 0
CLTC_OPTN$IS_9YEAR[which(CLTC_OPTN$NODLT == 1 & CLTC_OPTN$YEAR_AFTER_LT > 0.1 & CLTC_OPTN$YEAR_AFTER_LT <=9 )] <- 1
CLTC_OPTN$IS_10YEAR <- 0
CLTC_OPTN$IS_10YEAR[which(CLTC_OPTN$NODLT == 1 & CLTC_OPTN$YEAR_AFTER_LT > 0.1 & CLTC_OPTN$YEAR_AFTER_LT <=10 )] <- 1

Fig2_data <- data.frame(matrix(ncol = 8, nrow = 1))
colnames(Fig2_data) <- c("Year","name","ncase","ncontrol","ratio","se","LCI","UCI")

calculate_ratio <- function(ncase, ncontrol) {
  ratio <- ncase/(ncase+ncontrol)
  se <- sqrt((ratio * (1-ratio))/(ncase+ncontrol))
  LCI <- ratio - 1.96*se
  UCI <- ratio + 1.96*se
  return(c(ratio, se, LCI, UCI))
}

ratio_difference <- function(p1, n1, p2, n2) {
  RD <- p2 - p1
  SE <- sqrt(((p1*(1-p1))/n1) + ((p2*(1-p2))/n2))
  LCI <- RD - 1.96*SE
  UCI <- RD + 1.96*SE
  return(c(RD, SE, LCI, UCI))
}

Incidence_table <- as.data.frame(matrix(nrow=1,ncol=5))
colnames(Incidence_table) <- c("name","1Y","4Y","7Y","10Y")
## Age
CLTC_OPTN$IS_ADULT <- NA
CLTC_OPTN$IS_ADULT[which(CLTC_OPTN$Age_R >= 18)] <- 1
CLTC_OPTN$IS_ADULT[which(CLTC_OPTN$Age_R < 18)] <- 0 
IS_ADULT_1YEAR <- table(CLTC_OPTN$IS_ADULT, CLTC_OPTN$IS_1YEAR)
CHILD_1YEAR_ratio <- calculate_ratio(IS_ADULT_1YEAR[3], IS_ADULT_1YEAR[1])
ADULT_1YEAR_ratio <- calculate_ratio(IS_ADULT_1YEAR[4], IS_ADULT_1YEAR[2])

IS_ADULT_4YEAR <- table(CLTC_OPTN$IS_ADULT, CLTC_OPTN$IS_4YEAR)
CHILD_4YEAR_ratio <- calculate_ratio(IS_ADULT_4YEAR[3], IS_ADULT_4YEAR[1])
ADULT_4YEAR_ratio <- calculate_ratio(IS_ADULT_4YEAR[4], IS_ADULT_4YEAR[2])

IS_ADULT_7YEAR <- table(CLTC_OPTN$IS_ADULT, CLTC_OPTN$IS_7YEAR)
CHILD_7YEAR_ratio <- calculate_ratio(IS_ADULT_7YEAR[3], IS_ADULT_7YEAR[1])
ADULT_7YEAR_ratio <- calculate_ratio(IS_ADULT_7YEAR[4], IS_ADULT_7YEAR[2])

IS_ADULT_10YEAR <- table(CLTC_OPTN$IS_ADULT, CLTC_OPTN$IS_10YEAR)
CHILD_10YEAR_ratio <- calculate_ratio(IS_ADULT_10YEAR[3], IS_ADULT_10YEAR[1])
ADULT_10YEAR_ratio <- calculate_ratio(IS_ADULT_10YEAR[4], IS_ADULT_10YEAR[2])

ratio_childAdult <- data.frame(ChildAdult = c(rep("Pediatric recepient", 4), rep("Adult recepient", 4)),
                               YEARS = c(c(1,4,7,10), c(1,4,7,10)),
                               Ratio = c(CHILD_1YEAR_ratio[1], CHILD_4YEAR_ratio[1],
                                         CHILD_7YEAR_ratio[1],CHILD_10YEAR_ratio[1],  
                                         ADULT_1YEAR_ratio[1],ADULT_4YEAR_ratio[1], 
                                         ADULT_7YEAR_ratio[1], ADULT_10YEAR_ratio[1]),
                               Ratio_se = c(CHILD_1YEAR_ratio[2],CHILD_4YEAR_ratio[2], 
                                            CHILD_7YEAR_ratio[2],CHILD_10YEAR_ratio[2],
                                            ADULT_1YEAR_ratio[2], ADULT_4YEAR_ratio[2], 
                                            ADULT_7YEAR_ratio[2], ADULT_10YEAR_ratio[2])
)
write.csv(ratio_childAdult, "ratio_ChildAdult_plot.csv", row.names = F)

## Donor type
CLTC_OPTN$IS_LIVING <- NA
CLTC_OPTN$IS_LIVING[which(CLTC_OPTN$DonorType == 1)] <- 1
CLTC_OPTN$IS_LIVING[which(CLTC_OPTN$DonorType == 0)] <- 0 
IS_LIVING_1YEAR <- table(CLTC_OPTN$IS_LIVING, CLTC_OPTN$IS_1YEAR)
Deceased_1YEAR_ratio <- calculate_ratio(IS_LIVING_1YEAR[3], IS_LIVING_1YEAR[1])
LIVING_1YEAR_ratio <- calculate_ratio(IS_LIVING_1YEAR[4], IS_LIVING_1YEAR[2])

IS_LIVING_4YEAR <- table(CLTC_OPTN$IS_LIVING, CLTC_OPTN$IS_4YEAR)
Deceased_4YEAR_ratio <- calculate_ratio(IS_LIVING_4YEAR[3], IS_LIVING_4YEAR[1])
LIVING_4YEAR_ratio <- calculate_ratio(IS_LIVING_4YEAR[4], IS_LIVING_4YEAR[2])

IS_LIVING_7YEAR <- table(CLTC_OPTN$IS_LIVING, CLTC_OPTN$IS_7YEAR)
Deceased_7YEAR_ratio <- calculate_ratio(IS_LIVING_7YEAR[3], IS_LIVING_7YEAR[1])
LIVING_7YEAR_ratio <- calculate_ratio(IS_LIVING_7YEAR[4], IS_LIVING_7YEAR[2])

IS_LIVING_10YEAR <- table(CLTC_OPTN$IS_LIVING, CLTC_OPTN$IS_10YEAR)
Deceased_10YEAR_ratio <- calculate_ratio(IS_LIVING_10YEAR[3], IS_LIVING_10YEAR[1])
LIVING_10YEAR_ratio <- calculate_ratio(IS_LIVING_10YEAR[4], IS_LIVING_10YEAR[2])

ratio_DeceasedLIVING <- data.frame(DeceasedLIVING = c(rep("Deceased donor", 4), rep("Living donor", 4)),
                                   YEARS = c(c(1,4,7,10), c(1,4,7,10)),
                                   Ratio = c(Deceased_1YEAR_ratio[1], Deceased_4YEAR_ratio[1],
                                             Deceased_7YEAR_ratio[1],Deceased_10YEAR_ratio[1],  
                                             LIVING_1YEAR_ratio[1],LIVING_4YEAR_ratio[1], 
                                             LIVING_7YEAR_ratio[1], LIVING_10YEAR_ratio[1]),
                                   Ratio_se = c(Deceased_1YEAR_ratio[2],Deceased_4YEAR_ratio[2], 
                                                Deceased_7YEAR_ratio[2],Deceased_10YEAR_ratio[2],
                                                LIVING_1YEAR_ratio[2], LIVING_4YEAR_ratio[2], 
                                                LIVING_7YEAR_ratio[2], LIVING_10YEAR_ratio[2])
)
write.csv(ratio_DeceasedLIVING, "ratio_DeceasedLIVING_plot.csv", row.names = F)

## Gender
CLTC_OPTN$IS_Male <- NA
CLTC_OPTN$IS_Male[which(CLTC_OPTN$Gender_R == 1)] <- 1
CLTC_OPTN$IS_Male[which(CLTC_OPTN$Gender_R == 0)] <- 0 
IS_Male_1YEAR <- table(CLTC_OPTN$IS_Male, CLTC_OPTN$IS_1YEAR)
Female_1YEAR_ratio <- calculate_ratio(IS_Male_1YEAR[3], IS_Male_1YEAR[1])
Male_1YEAR_ratio <- calculate_ratio(IS_Male_1YEAR[4], IS_Male_1YEAR[2])

IS_Male_4YEAR <- table(CLTC_OPTN$IS_Male, CLTC_OPTN$IS_4YEAR)
Female_4YEAR_ratio <- calculate_ratio(IS_Male_4YEAR[3], IS_Male_4YEAR[1])
Male_4YEAR_ratio <- calculate_ratio(IS_Male_4YEAR[4], IS_Male_4YEAR[2])

IS_Male_7YEAR <- table(CLTC_OPTN$IS_Male, CLTC_OPTN$IS_7YEAR)
Female_7YEAR_ratio <- calculate_ratio(IS_Male_7YEAR[3], IS_Male_7YEAR[1])
Male_7YEAR_ratio <- calculate_ratio(IS_Male_7YEAR[4], IS_Male_7YEAR[2])

IS_Male_10YEAR <- table(CLTC_OPTN$IS_Male, CLTC_OPTN$IS_10YEAR)
Female_10YEAR_ratio <- calculate_ratio(IS_Male_10YEAR[3], IS_Male_10YEAR[1])
Male_10YEAR_ratio <- calculate_ratio(IS_Male_10YEAR[4], IS_Male_10YEAR[2])

ratio_FemaleMale <- data.frame(FemaleMale = c(rep("Female recepient", 4), rep("Male recepient", 4)),
                               YEARS = c(c(1,4,7,10), c(1,4,7,10)),
                               Ratio = c(Female_1YEAR_ratio[1], Female_4YEAR_ratio[1],
                                         Female_7YEAR_ratio[1],Female_10YEAR_ratio[1],  
                                         Male_1YEAR_ratio[1],Male_4YEAR_ratio[1], 
                                         Male_7YEAR_ratio[1], Male_10YEAR_ratio[1]),
                               Ratio_se = c(Female_1YEAR_ratio[2],Female_4YEAR_ratio[2], 
                                            Female_7YEAR_ratio[2],Female_10YEAR_ratio[2],
                                            Male_1YEAR_ratio[2], Male_4YEAR_ratio[2], 
                                            Male_7YEAR_ratio[2], Male_10YEAR_ratio[2])
)
write.csv(ratio_FemaleMale, "ratio_FemaleMale_plot.csv", row.names = F)

## Ethnicity
CLTC_OPTN$Ethnicity <- 0
CLTC_OPTN$Ethnicity[which(CLTC_OPTN$White_R==1)] <- 1
CLTC_OPTN$Ethnicity[which(CLTC_OPTN$Hispanic_R==1)] <- 2
CLTC_OPTN$Ethnicity[which(CLTC_OPTN$Asian_R==1)] <- 3
CLTC_OPTN$Ethnicity[which(CLTC_OPTN$African_R==1)] <- 4

Ethnicity_1YEAR <- table(CLTC_OPTN$Ethnicity, CLTC_OPTN$IS_1YEAR)
White_1YEAR_ratio <- calculate_ratio(Ethnicity_1YEAR[7], Ethnicity_1YEAR[2])
Hispanic_1YEAR_ratio <- calculate_ratio(Ethnicity_1YEAR[8], Ethnicity_1YEAR[3])
Asian_1YEAR_ratio <- calculate_ratio(Ethnicity_1YEAR[9], Ethnicity_1YEAR[4])
African_1YEAR_ratio <- calculate_ratio(Ethnicity_1YEAR[10], Ethnicity_1YEAR[5])

Ethnicity_4YEAR <- table(CLTC_OPTN$Ethnicity, CLTC_OPTN$IS_4YEAR)
White_4YEAR_ratio <- calculate_ratio(Ethnicity_4YEAR[7], Ethnicity_4YEAR[2])
Hispanic_4YEAR_ratio <- calculate_ratio(Ethnicity_4YEAR[8], Ethnicity_4YEAR[3])
Asian_4YEAR_ratio <- calculate_ratio(Ethnicity_4YEAR[9], Ethnicity_4YEAR[4])
African_4YEAR_ratio <- calculate_ratio(Ethnicity_4YEAR[10], Ethnicity_4YEAR[5])

Ethnicity_7YEAR <- table(CLTC_OPTN$Ethnicity, CLTC_OPTN$IS_7YEAR)
White_7YEAR_ratio <- calculate_ratio(Ethnicity_7YEAR[7], Ethnicity_7YEAR[2])
Hispanic_7YEAR_ratio <- calculate_ratio(Ethnicity_7YEAR[8], Ethnicity_7YEAR[3])
Asian_7YEAR_ratio <- calculate_ratio(Ethnicity_7YEAR[9], Ethnicity_7YEAR[4])
African_7YEAR_ratio <- calculate_ratio(Ethnicity_7YEAR[10], Ethnicity_7YEAR[5])

Ethnicity_10YEAR <- table(CLTC_OPTN$Ethnicity, CLTC_OPTN$IS_10YEAR)
White_10YEAR_ratio <- calculate_ratio(Ethnicity_10YEAR[7], Ethnicity_10YEAR[2])
Hispanic_10YEAR_ratio <- calculate_ratio(Ethnicity_10YEAR[8], Ethnicity_10YEAR[3])
Asian_10YEAR_ratio <- calculate_ratio(Ethnicity_10YEAR[9], Ethnicity_10YEAR[4])
African_10YEAR_ratio <- calculate_ratio(Ethnicity_10YEAR[10], Ethnicity_10YEAR[5])

ratio_Ethnicity <- data.frame(Ethnicity = c(rep("White", 4), rep("Hispanic", 4),
                                            rep("Asian", 4), rep("African", 4)),
                              YEARS = c(c(1,4,7,10), c(1,4,7,10), c(1,4,7,10), c(1,4,7,10)),
                              Ratio = c(White_1YEAR_ratio[1],White_4YEAR_ratio[1],
                                        White_7YEAR_ratio[1],White_10YEAR_ratio[1],
                                        Hispanic_1YEAR_ratio[1],Hispanic_4YEAR_ratio[1], 
                                        Hispanic_7YEAR_ratio[1], Hispanic_10YEAR_ratio[1],
                                        Asian_1YEAR_ratio[1], Asian_4YEAR_ratio[1], 
                                        Asian_7YEAR_ratio[1], Asian_10YEAR_ratio[1], 
                                        African_1YEAR_ratio[1],African_4YEAR_ratio[1], 
                                        African_7YEAR_ratio[1], African_10YEAR_ratio[1]),
                              Ratio_se = c(White_1YEAR_ratio[2], White_4YEAR_ratio[2], 
                                           White_7YEAR_ratio[2], White_10YEAR_ratio[2], 
                                           Hispanic_1YEAR_ratio[2],Hispanic_4YEAR_ratio[2], 
                                           Hispanic_7YEAR_ratio[2],Hispanic_10YEAR_ratio[2],
                                           Asian_1YEAR_ratio[2],Asian_4YEAR_ratio[2], 
                                           Asian_7YEAR_ratio[2], Asian_10YEAR_ratio[2], 
                                           African_1YEAR_ratio[2], African_4YEAR_ratio[2], 
                                           African_7YEAR_ratio[2], African_10YEAR_ratio[2])
)
write.csv(ratio_Ethnicity, "ratio_Ethnicity_plot.csv", row.names = F)

#ratio_plot1 <- read.csv("ratio_ChildAdult_plot.csv", header = T)
ratio_plot1 <- ratio_childAdult
colnames(ratio_plot1)[1] <- "Age"
ratio_plot1$Ratio_100 <- 100*ratio_plot1$Ratio
ratio_plot1$Ratio_se_100 <- 100*ratio_plot1$Ratio_se
library(ggplot2)
p1 <- ggplot(ratio_plot1, aes(x=YEARS, y=Ratio_100, color = Age, shape = Age))+
  theme_bw() +   
  geom_line(size=1.2)+
  geom_point(size = 3)+
  scale_color_brewer(palette="Paired")+ 
  scale_fill_brewer(palette="Paired")+ 
  scale_y_continuous(breaks = c(0, 3, 6, 9, 12, 15)) +
  geom_errorbar(aes(ymin=Ratio_100 - 1.96*Ratio_se_100,
                    ymax=Ratio_100 + 1.96*Ratio_se_100),
                width=0.1)+
  theme(plot.title = element_text(size=12,hjust=0.5),
        panel.background = element_blank(),
        panel.grid=element_blank(),
        axis.line.x = element_line(),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        axis.line.y = element_line(),
        legend.position = "bottom")+
  scale_x_continuous(breaks = c(1,4,7,10),labels=c(1,4,7,10))+
  
  labs(x="Years after transplantation", y="NODLT incidence rates (%)",
       title = "Adult / Pediatric recepients")
ggsave("Age.pdf",device = cairo_pdf,width =6, height =6)
ggsave("Age.png",width = 6, height = 6, dpi = 300)
##############2
#ratio_plot2 <- read.csv("ratio_DeceasedLIVING_plot.csv", header = T)
ratio_plot2 <- ratio_DeceasedLIVING
colnames(ratio_plot2)[1] <- "DonorType"
ratio_plot2$Ratio_100 <- 100*ratio_plot2$Ratio
ratio_plot2$Ratio_se_100 <- 100*ratio_plot2$Ratio_se
library(patchwork)
p2 <- ggplot(ratio_plot2, aes(x=YEARS, y=Ratio_100, color = DonorType, shape = DonorType))+
  theme_bw() +  
  geom_line(size=1.2)+
  geom_point(size = 3)+
  scale_color_brewer(palette="Paired")+  
  scale_fill_brewer(palette="Paired")+ 
  scale_y_continuous(breaks = c(0, 3, 6, 9, 12, 15)) +
  geom_errorbar(aes(ymin=Ratio_100 - 1.96*Ratio_se_100,
                    ymax=Ratio_100 + 1.96*Ratio_se_100),
                width=0.1)+
  theme(plot.title = element_text(size=12,hjust=0.5),
        panel.background = element_blank(),
        panel.grid=element_blank(),
        axis.line.x = element_line(),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        axis.line.y = element_line(),
        legend.position = "bottom")+
  scale_x_continuous(breaks = c(1,4,7,10),labels=c(1,4,7,10))+
  
  labs(x="Years after transplantation", y="NODLT incidence rates (%)",
       title = "Deceased / Living donors")
ggsave("DonorType.pdf",device = cairo_pdf,width =6, height =6)
ggsave("DonorType.png",width = 6, height = 6, dpi = 300)
##########3
 #ratio_plot3 <- read.csv("ratio_FemaleMale_plot.csv", header = T)
ratio_plot3 <- ratio_FemaleMale
colnames(ratio_plot3)[1] <- "Gender"
ratio_plot3$Ratio_100 <- 100*ratio_plot3$Ratio
ratio_plot3$Ratio_se_100 <- 100*ratio_plot3$Ratio_se
library(ggplot2);library(patchwork)
p3 <- ggplot(ratio_plot3, aes(x=YEARS, y=Ratio_100, color = Gender, shape = Gender))+
  theme_bw() +   
  geom_line(size=1.2)+
  geom_point(size = 3)+
  scale_color_brewer(palette="Paired")+  
  scale_fill_brewer(palette="Paired")+ 
  scale_y_continuous(breaks = c(0, 3, 6, 9, 12, 15)) +
  geom_errorbar(aes(ymin=Ratio_100 - 1.96*Ratio_se_100,
                    ymax=Ratio_100 + 1.96*Ratio_se_100),
                width=0.1)+
  theme(plot.title = element_text(size=12,hjust=0.5),
        panel.background = element_blank(),
        panel.grid=element_blank(),
        axis.line.x = element_line(),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        axis.line.y = element_line(),
        legend.position = "bottom")+
  scale_x_continuous(breaks = c(1,4,7,10),labels=c(1,4,7,10))+
  
  labs(x="Years after transplantation", y="NODLT incidence rates (%)",
       title = "Male / Female recepients")
ggsave("Gender.pdf",device = cairo_pdf,width =6, height =6)
ggsave("Gender.png",width = 6, height = 6, dpi = 300)
#########4
#ratio_plot4 <- read.csv("ratio_Ethnicity_plot.csv", header = T)
ratio_plot4 <- ratio_Ethnicity
ratio_plot4$Ethnicity <- as.factor(ratio_plot4$Ethnicity)
ratio_plot4$Ratio_100 <- 100*ratio_plot4$Ratio
ratio_plot4$Ratio_se_100 <- 100*ratio_plot4$Ratio_se
library(ggplot2);library(patchwork)
p4 <- ggplot(ratio_plot4, aes(x=YEARS, y=Ratio_100, color = Ethnicity, shape = Ethnicity))+
  theme_bw() +   
  geom_line(size=1.2)+
  geom_point(size = 3)+
  scale_color_brewer(palette="Blues")+ 
  scale_fill_brewer(palette="Blues")+ 
  scale_y_continuous(breaks = c(0, 3, 6, 9, 12, 15)) +
  geom_errorbar(aes(ymin=Ratio_100 - 1.96*Ratio_se_100,
                    ymax=Ratio_100 + 1.96*Ratio_se_100),
                width=0.1)+
  theme(plot.title = element_text(size=12,hjust=0.5),
        panel.background = element_blank(),
        panel.grid=element_blank(),
        axis.line.x = element_line(),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        axis.line.y = element_line(),
        legend.position = "bottom")+
  scale_x_continuous(breaks = c(1,4,7,10),labels=c(1,4,7,10))+
  
  labs(x="Years after transplantation", y="NODLT incidence rates (%)",
       title = "Different ethnicities")
ggsave("Ethnicity.pdf",device = cairo_pdf,width =6, height =6)
ggsave("Ethnicity.png",width = 6, height = 6, dpi = 300)
