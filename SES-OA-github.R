library(tidyverse)
library(ggrepel);library(ggExtra); library(viridis); library(gridExtra); library(cowplot)
library(ggpubr)
library(zoo)
library(lubridate); library(gsubfn)
library(haven)
library(pspearman); library(dunn.test); library(rstatix); library(DescTools)
library(gvlma); library(forestmangr);library(forestmodel)


####Data loading####
results<- read.csv("Paper/Resultados_analisis.csv")
results1<- results %>% select(1:150)

results2<- results1 %>%
  mutate(A2_Sexo = factor(A2_Sexo, 1:2,c("Female", "Male"))) %>%
  mutate(IMC = Peso/(Talla^2)) %>%
  mutate(across(c("Fuma_Hoy","Ha_Fumado","OA","Rodilla","Cadera","Otras","A7_Diabetes","A8_Hipertension","A9_Art_Reum","A10_Otra_Reum"),
                ~factor(.,1:2,c("Yes","No")))) %>%
  mutate(across(c("haq_BASTON","HAQ_ANDADOR", "HAQ_MULETAS", "HAQ_SILLARUED","HAQ_APARVEST","HAQ_UTEN","HAQ_SILLAESP","HAQ_OTRO1","HAQ_AY_VEST","HAQ_AY_LEVANT","HAQ_AY_COMER","HAQ_AY_CAMINAR","HAQ_INOD","HAQ_ASIEN_TINA","HAQ_AGARR_TINA","HAQ_ABRIDOR","HAQ_EXTEN_BANO","HAQ_EXTEN_ALCAN","HAQ_OTRO2","HAQ_AY_HIGIENE","HAQ_AY_ALCANZAR","HAQ_AY_AGARRAR","HAQ_AY_COMPRAS"),
                ~factor(.,1:2,c("Yes","No")))) %>%
  mutate(across(c("B1_Escol","B3_Escol_Padre","B4_Escol_Madre"),
                 ~factor(.,1:6,c("Less than elementary","Elementary school","Middle-school","High-school","College level","College level"), ordered = TRUE))) %>%
  mutate(across(c("B2_Ocup","B5_Ocup_Padre","B6_Ocup_Madre"),
                ~factor(.,1:4,c("Manual labor","Non-managerial office work", "Managerial","Home")))) %>%
  mutate(across(c("M1_DVD","M2_Micro","M3_Tel","M4_Casa","M5_Cable","M6_Compu","M7_Auto","M8_Internet"),
                ~ifelse(. == 2,0,.))) %>%
  mutate(MSNSE = 0.25*M1_DVD + 0.22*M2_Micro + 0.17*M3_Tel + 0.13*M4_Casa + 
           0.1*M5_Cable + 0.07*M6_Compu + 0.03*M7_Auto + 0.01*M8_Internet) %>%
  mutate(MSNSE_Quint = cut(MSNSE, c(0,0.2,0.4,0.6,0.8,1), right = FALSE, include.lowest = TRUE ,labels = c("I","II","III","IV","V"), ordered_result = TRUE)) %>%
  mutate(across(starts_with("W"),as.numeric)) %>%
  mutate(WOMAC = rowSums(across(starts_with("W")))) %>%
  mutate(WOMAC_std = WOMAC/96*100) %>%
  mutate(across(c("Q11","Q12","Q22","Q23"), ~ifelse(.=="X",0,.))) %>%
  mutate(across(starts_with("Q"),as.numeric)) %>%
  mutate(AMICAL = rowSums(across(c(Q1:Q29,Q33:Q38,Q41))) + (11-Q30) + (11-Q31) + (11-Q32) + (11-Q39) + (11-Q40) + (11-Q42) + (11-Q43)) %>%
  mutate(AMICAL_std = AMICAL/430*100) %>%
  mutate(across(c(HAQ1:HAQ9,HAQ10:HAQ20), as.numeric)) %>%
  mutate(HAQ_Calc_A = pmax(HAQ1,HAQ2, ifelse(HAQ_AY_VEST == 1 | HAQ_APARVEST == 1,3,0), na.rm = TRUE) - 1) %>%
  mutate(HAQ_Calc_B = pmax(HAQ3,HAQ4, ifelse(HAQ_AY_LEVANT == 1 | HAQ_SILLAESP == 1,3,0), na.rm = TRUE) - 1) %>%
  mutate(HAQ_Calc_C = pmax(HAQ5,HAQ6, HAQ7, ifelse(HAQ_AY_COMER == 1 | HAQ_UTEN == 1 ,3,0), na.rm = TRUE) - 1) %>%
  mutate(HAQ_Calc_D = pmax(HAQ8,HAQ9, ifelse(HAQ_AY_CAMINAR == 1 | haq_BASTON == 1 | HAQ_ANDADOR == 1| HAQ_MULETAS == 1| HAQ_SILLARUED == 1,3,0), na.rm = TRUE) - 1) %>%
  mutate(HAQ_Calc_E = pmax(HAQ10,HAQ11, HAQ12, ifelse(HAQ_AY_HIGIENE == 1 | HAQ_INOD == 1| HAQ_EXTEN_BANO == 1| HAQ_ASIEN_TINA == 1| HAQ_AGARR_TINA == 1,3,0), na.rm = TRUE) - 1) %>%
  mutate(HAQ_Calc_F = pmax(HAQ13,HAQ14, ifelse(HAQ_AY_ALCANZAR == 1 | HAQ_EXTEN_ALCAN == 1,3,0), na.rm = TRUE) - 1) %>%
  mutate(HAQ_Calc_G = pmax(HAQ15,HAQ16, HAQ17, ifelse(HAQ_AY_AGARRAR == 1 | HAQ_ABRIDOR == 1,3,0), na.rm = TRUE) - 1) %>%
  mutate(HAQ_Calc_H = pmax(HAQ18,HAQ19, HAQ20, ifelse(HAQ_AY_COMPRAS == 1,3,0), na.rm = TRUE) - 1) %>%
  mutate(HAQ_Tot = rowSums(across(starts_with("HAQ_Calc")))/8) %>%
  mutate(HAQ_std = HAQ_Tot/3*100)

#### Análisis ####
results3 <- results2 %>%
  mutate(MSNSE_Quint = fct_collapse(MSNSE_Quint,
                                    "I or II" = c("I","II"),
                                    III = c("III"),
                                    IV = c("IV"),
                                    V = c("V"))) %>%
  select(A1_Edad,A2_Sexo, IMC, A7_Diabetes, A8_Hipertension,B1_Escol,B2_Ocup,B3_Escol_Padre,B4_Escol_Madre,B5_Ocup_Padre,B6_Ocup_Madre,MSNSE_Quint,WOMAC,AMICAL, HAQ_Tot, WOMAC_std,AMICAL_std,HAQ_std)

Spear_Escol_AMICAL<- spearman.test(results3$B1_Escol, results3$AMICAL)
Spear_MSNSE_AMICAL<- spearman.test(results3$MSNSE_Quint, results3$AMICAL)
Spear_EscoPad_AMICAL<- spearman.test(results3$B3_Escol_Padre, results3$AMICAL)
Spear_EscoMad_AMICAL<- spearman.test(results3$B4_Escol_Madre, results3$AMICAL)

Spear_Escol_WOMAC<- spearman.test(results3$B1_Escol, results3$WOMAC)
Spear_MSNSE_WOMAC<- spearman.test(results3$MSNSE_Quint, results3$WOMAC)
Spear_EscoPad_WOMAC<- spearman.test(results3$B3_Escol_Padre, results3$WOMAC)
Spear_EscoMad_WOMAC<- spearman.test(results3$B4_Escol_Madre, results3$WOMAC)

Spear_Escol_HAQ<- spearman.test(results3$B1_Escol, results3$HAQ_Tot)
Spear_MSNSE_HAQ<- spearman.test(results3$MSNSE_Quint, results3$HAQ_Tot)
Spear_EscoPad_HAQ<- spearman.test(results3$B3_Escol_Padre, results3$HAQ_Tot)
Spear_EscoMad_HAQ<- spearman.test(results3$B4_Escol_Madre, results3$HAQ_Tot)


Spear_Escol_AMICAL2<- SpearmanRho(results3$B1_Escol, results3$AMICAL, use = "complete.obs", conf.level = 0.95)
Spear_MSNSE_AMICAL2<- SpearmanRho(results3$MSNSE_Quint, results3$AMICAL, use = "complete.obs", conf.level = 0.95)
Spear_EscoPad_AMICAL2<- SpearmanRho(results3$B3_Escol_Padre, results3$AMICAL, use = "complete.obs", conf.level = 0.95)
Spear_EscoMad_AMICAL2<- SpearmanRho(results3$B4_Escol_Madre, results3$AMICAL, use = "complete.obs", conf.level = 0.95)

Spear_Escol_WOMAC2<- SpearmanRho(results3$B1_Escol, results3$WOMAC, use = "complete.obs", conf.level = 0.95)
Spear_MSNSE_WOMAC2<- SpearmanRho(results3$MSNSE_Quint, results3$WOMAC, use = "complete.obs", conf.level = 0.95)
Spear_EscoPad_WOMAC2<- SpearmanRho(results3$B3_Escol_Padre, results3$WOMAC, use = "complete.obs", conf.level = 0.95)
Spear_EscoMad_WOMAC2<- SpearmanRho(results3$B4_Escol_Madre, results3$WOMAC, use = "complete.obs", conf.level = 0.95)

Spear_Escol_HAQ2<- SpearmanRho(results3$B1_Escol, results3$HAQ_Tot, use = "complete.obs", conf.level = 0.95)
Spear_MSNSE_HAQ2<- SpearmanRho(results3$MSNSE_Quint, results3$HAQ_Tot, use = "complete.obs", conf.level = 0.95)
Spear_EscoPad_HAQ2<- SpearmanRho(results3$B3_Escol_Padre, results3$HAQ_Tot, use = "complete.obs", conf.level = 0.95)
Spear_EscoMad_HAQ2<- SpearmanRho(results3$B4_Escol_Madre, results3$HAQ_Tot, use = "complete.obs", conf.level = 0.95)


Kruskal_Ocup_WOMAC<- kruskal.test(WOMAC ~ B2_Ocup, data = results3)
Kruskal_Ocup_AMICAL<- kruskal.test(AMICAL ~ B2_Ocup, data = results3)
Kruskal_Ocup_HAQ<- kruskal.test(HAQ_Tot ~ B2_Ocup, data = results3)

Kruskal_OcuMad_WOMAC<- kruskal.test(WOMAC ~ B6_Ocup_Madre, data = results3)
Kruskal_OcuMad_AMICAL<- kruskal.test(AMICAL ~ B6_Ocup_Madre, data = results3)
Kruskal_OcuMad_HAQ<- kruskal.test(HAQ_Tot ~ B6_Ocup_Madre, data = results3)

Kruskal_OcuPad_WOMAC<- kruskal.test(WOMAC ~ B5_Ocup_Padre, data = results3)
Kruskal_OcuPad_AMICAL<- kruskal.test(AMICAL ~ B5_Ocup_Padre, data = results3)
Kruskal_OcuPad_HAQ<- kruskal.test(HAQ_Tot ~ B5_Ocup_Padre, data = results3)


pwc_Ocup_WOMAC<-  dunn_test(WOMAC ~ B2_Ocup, data = results3, p.adjust.method = "none") %>% add_xy_position(x= "B2_Ocup")
pwc_Ocup_WOMAC_adj<- pwc_Ocup_WOMAC %>%
  filter(group1 == "Managerial" | group2 == "Managerial") %>%
  mutate(p.adj = p.adjust(p.adj, method = "bonferroni")) %>%
  mutate(p.adj.signif = ifelse(p.adj<0.01,"**",ifelse(p.adj<0.05,"*","ns")))

pwc_Ocup_AMICAL<- dunn_test(AMICAL ~ B2_Ocup, data = results3, p.adjust.method = "none") %>% add_xy_position(x= "B2_Ocup")
pwc_Ocup_AMICAL_adj<- pwc_Ocup_AMICAL %>%
  filter(group1 == "Managerial" | group2 == "Managerial") %>%
  mutate(p.adj = p.adjust(p.adj, method = "bonferroni")) %>%
  mutate(p.adj.signif = ifelse(p.adj<0.01,"**",ifelse(p.adj<0.05,"*","ns")))

pwc_Ocup_HAQ<- dunn_test(HAQ_Tot ~ B2_Ocup, data = results3, p.adjust.method = "none") %>% add_xy_position(x= "B2_Ocup")
pwc_Ocup_HAQ_adj<- pwc_Ocup_HAQ %>%
  filter(group1 == "Managerial" | group2 == "Managerial") %>%
  mutate(p.adj = p.adjust(p.adj, method = "bonferroni")) %>%
  mutate(p.adj.signif = ifelse(p.adj<0.01,"**",ifelse(p.adj<0.05,"*","ns")))

pwc_OcuPad_WOMAC<-  dunn_test(WOMAC ~ B5_Ocup_Padre, data = results3, p.adjust.method = "none") %>% add_xy_position(x= "B5_Ocup_Padre")
pwc_OcuPad_WOMAC_adj<- pwc_OcuPad_WOMAC %>%
  filter(group1 == "Managerial" | group2 == "Managerial") %>%
  mutate(p.adj = p.adjust(p.adj, method = "bonferroni")) %>%
  mutate(p.adj.signif = ifelse(p.adj<0.01,"**",ifelse(p.adj<0.05,"*","ns")))

pwc_OcuPad_AMICAL<-  dunn_test(AMICAL ~ B5_Ocup_Padre, data = results3, p.adjust.method = "none") %>% add_xy_position(x= "B5_Ocup_Padre")
pwc_OcuPad_AMICAL_adj<- pwc_OcuPad_AMICAL %>%
  filter(group1 == "Managerial" | group2 == "Managerial") %>%
  mutate(p.adj = p.adjust(p.adj, method = "bonferroni")) %>%
  mutate(p.adj.signif = ifelse(p.adj<0.01,"**",ifelse(p.adj<0.05,"*","ns")))

pwc_OcuPad_HAQ<-  dunn_test(HAQ_Tot ~ B5_Ocup_Padre, data = results3, p.adjust.method = "none") %>% add_xy_position(x= "B5_Ocup_Padre")
pwc_OcuPad_HAQ_adj<- pwc_OcuPad_HAQ %>%
  filter(group1 == "Managerial" | group2 == "Managerial") %>%
  mutate(p.adj = p.adjust(p.adj, method = "bonferroni")) %>%
  mutate(p.adj.signif = ifelse(p.adj<0.01,"**",ifelse(p.adj<0.05,"*","ns")))

pwc_OcuMad_WOMAC<-  dunn_test(WOMAC ~ B6_Ocup_Madre, data = results3, p.adjust.method = "none") %>% add_xy_position(x= "B6_Ocup_Madre")
pwc_OcuMad_WOMAC_adj<- pwc_OcuMad_WOMAC %>%
  filter(group1 == "Managerial" | group2 == "Managerial") %>%
  mutate(p.adj = p.adjust(p.adj, method = "bonferroni")) %>%
  mutate(p.adj.signif = ifelse(p.adj<0.01,"**",ifelse(p.adj<0.05,"*","ns")))

pwc_OcuMad_AMICAL<-  dunn_test(AMICAL ~ B6_Ocup_Madre, data = results3, p.adjust.method = "none") %>% add_xy_position(x= "B6_Ocup_Madre")
pwc_OcuMad_AMICAL_adj<- pwc_OcuMad_AMICAL %>%
  filter(group1 == "Managerial" | group2 == "Managerial") %>%
  mutate(p.adj = p.adjust(p.adj, method = "bonferroni")) %>%
  mutate(p.adj.signif = ifelse(p.adj<0.01,"**",ifelse(p.adj<0.05,"*","ns")))

pwc_OcuMad_HAQ<-  dunn_test(HAQ_Tot ~ B6_Ocup_Madre, data = results3, p.adjust.method = "none") %>% add_xy_position(x= "B6_Ocup_Madre")
pwc_OcuMad_HAQ_adj<- pwc_OcuMad_HAQ %>%
  filter(group1 == "Managerial" | group2 == "Managerial") %>%
  mutate(p.adj = p.adjust(p.adj, method = "bonferroni")) %>%
  mutate(p.adj.signif = ifelse(p.adj<0.01,"**",ifelse(p.adj<0.05,"*","ns")))



results_dicot<- results3 %>%
  mutate(MSNSE_Quint2 = fct_collapse( MSNSE_Quint, "Low" = c("I or II", "III"), "High" = c("IV", "V"))) %>%
  mutate(MSNSE_Quint2 = factor(MSNSE_Quint2, levels = c("Low", "High"), ordered = F)) %>%
  mutate(MSNSE_Quint2 = relevel(MSNSE_Quint2, ref = "High")) %>%
  mutate(B4_Escol_Madre2 = fct_collapse(B4_Escol_Madre, "Low" = c("Less than elementary"), "High" = c("Elementary school","Middle-school", "High-school","College level"))) %>%
  mutate(B4_Escol_Madre2 = factor(B4_Escol_Madre2, levels = c("Low", "High"), ordered = F)) %>%
  mutate(B4_Escol_Madre2 = relevel(B4_Escol_Madre2, ref = "High")) %>%
  mutate(Trayectoria = ifelse(B4_Escol_Madre2 == "Low" & MSNSE_Quint2 == "Low", "LL",
                              ifelse(B4_Escol_Madre2 == "Low" & MSNSE_Quint2 == "High", "LH",
                                     ifelse(B4_Escol_Madre2 == "High" & MSNSE_Quint2 == "Low", "HL",
                                            ifelse(B4_Escol_Madre2 == "High" & MSNSE_Quint2 == "High", "HH",
                                                   NA))))) %>%
 
  mutate(Trayectoria = factor(Trayectoria, levels = c("LL","LH","HL","HH"), ordered = F)) %>%
  mutate(`SES trajectory` = relevel(Trayectoria, ref = "HH")) %>%  mutate(Age = A1_Edad,
         Sex = relevel(factor(A2_Sexo, labels = c("Female", "Male")), ref = "Male"),
         `Income level`= MSNSE_Quint2,
         `Maternal education`= B4_Escol_Madre2,
         BMI=IMC, Diabetes=relevel(A7_Diabetes, ref = "No"), Hypertension=relevel(A8_Hipertension, ref= "No"))




MSNSE_WOMAC_lm<- lm(sqrt(WOMAC) ~ `Income level` + Age + Sex + BMI + Diabetes + Hypertension,
                    data = results_dicot)
gvlma(MSNSE_WOMAC_lm)
summary(MSNSE_WOMAC_lm)


MSNSE_AMICAL_lm<- lm(AMICAL ~ `Income level` + Age + Sex + BMI + Diabetes + Hypertension,
                     data = results_dicot)
gvlma(MSNSE_AMICAL_lm)
summary(MSNSE_AMICAL_lm)


MSNSE_HAQ_lm<- lm(HAQ_Tot ~ `Income level` + Age + Sex + BMI + Diabetes + Hypertension,
                  data = results_dicot)
gvlma(MSNSE_HAQ_lm)
summary(MSNSE_HAQ_lm)



EduMad_WOMAC_lm<- lm(sqrt(WOMAC) ~ `Maternal education` + Age + Sex + BMI + Diabetes + Hypertension,
                     data = results_dicot)
gvlma(EduMad_WOMAC_lm)
summary(EduMad_WOMAC_lm)



EduMad_AMICAL_lm<- lm(AMICAL ~ `Maternal education` + Age + Sex + BMI + Diabetes + Hypertension,
                      data = results_dicot)
gvlma(EduMad_AMICAL_lm)
summary(EduMad_AMICAL_lm)


EduMad_HAQ_lm<- lm(HAQ_Tot ~ `Maternal education` + Age + Sex + BMI + Diabetes + Hypertension,
                   data = results_dicot)
gvlma(EduMad_HAQ_lm)
summary(EduMad_HAQ_lm)


Mutual_WOMAC_lm<- lm(sqrt(WOMAC) ~ `Income level` + `Maternal education` + Age + Sex + BMI + Diabetes + Hypertension,
                     data = results_dicot)
gvlma(Mutual_WOMAC_lm)
summary(Mutual_WOMAC_lm)


Mutual_AMICAL_lm<- lm(AMICAL ~ `Income level` + `Maternal education` + Age + Sex + BMI + Diabetes + Hypertension,
                     data = results_dicot)
gvlma(Mutual_AMICAL_lm)
summary(Mutual_AMICAL_lm)


Mutual_HAQ_lm<- lm(HAQ_Tot ~ `Income level` + `Maternal education` + Age + Sex + BMI + Diabetes + Hypertension,
                     data = results_dicot)
gvlma(Mutual_HAQ_lm)
summary(Mutual_HAQ_lm)


Trayectoria_WOMAC_lm<- lm(sqrt(WOMAC) ~ `SES trajectory` + Age + Sex + BMI + Diabetes + Hypertension,
                     data = results_dicot)
gvlma(Trayectoria_WOMAC_lm)
summary(Trayectoria_WOMAC_lm)


Trayectoria_AMICAL_lm<- lm(AMICAL ~ `SES trajectory` + Age + Sex + BMI + Diabetes + Hypertension,
                          data = results_dicot)
gvlma(Trayectoria_AMICAL_lm)
summary(Trayectoria_AMICAL_lm)


Trayectoria_HAQ_lm<- lm(HAQ_Tot ~ `SES trajectory` + Age + Sex + BMI + Diabetes + Hypertension,
                           data = results_dicot)
gvlma(Trayectoria_HAQ_lm)
summary(Trayectoria_HAQ_lm)


####Trayectorias ####
Kruskal_Trayectoria_WOMAC<- kruskal.test(WOMAC ~ Trayectoria, data = results_dicot)
Kruskal_Trayectoria_AMICAL<- kruskal.test(AMICAL ~ Trayectoria, data = results_dicot)
Kruskal_Trayectoria_HAQ<- kruskal.test(HAQ_Tot ~ Trayectoria, data = results_dicot)


pwc_Trayectoria_WOMAC<-  dunn_test(WOMAC ~ Trayectoria, data = results_dicot, p.adjust.method = "none") %>% add_xy_position(x= "Trayectoria")
pwc_Trayectoria_WOMAC_adj<- pwc_Trayectoria_WOMAC %>%
  filter(group1 == "HH" | group2 == "HH") %>%
  mutate(p.adj = p.adjust(p.adj, method = "bonferroni")) %>%
  mutate(p.adj.signif = ifelse(p.adj<0.01,"**",ifelse(p.adj<0.05,"*","ns")))

pwc_Trayectoria_AMICAL<-  dunn_test(AMICAL ~ Trayectoria, data = results_dicot, p.adjust.method = "none") %>% add_xy_position(x= "Trayectoria")
pwc_Trayectoria_AMICAL_adj<- pwc_Trayectoria_AMICAL %>%
  filter(group1 == "HH" | group2 == "HH") %>%
  mutate(p.adj = p.adjust(p.adj, method = "bonferroni")) %>%
  mutate(p.adj.signif = ifelse(p.adj<0.01,"**",ifelse(p.adj<0.05,"*","ns")))

pwc_Trayectoria_HAQ<-  dunn_test(HAQ_Tot ~ Trayectoria, data = results_dicot, p.adjust.method = "none") %>% add_xy_position(x= "Trayectoria")
pwc_Trayectoria_HAQ_adj<- pwc_Trayectoria_HAQ %>%
  filter(group1 == "HH" | group2 == "HH") %>%
  mutate(p.adj = p.adjust(p.adj, method = "bonferroni")) %>%
  mutate(p.adj.signif = ifelse(p.adj<0.01,"**",ifelse(p.adj<0.05,"*","ns")))




#### Gráficas ####

titlesize<- 40
subsize<- 36
xsize1<- 36
xsize2<- 36
ysize<- 36
outsize<- 3
sigsize = 7.5
margs1<- margin(0,120,40,0,"pt")
margs2<- margin(0,20,40,0,"pt")


####Figure1####

MSNSE_WOMAC<- results3 %>%
  ggplot(aes(x=MSNSE_Quint, y = WOMAC)) +
  stat_boxplot(geom= "errorbar", width = 0.2) +
  geom_boxplot(fill = "darkorange1", width= 0.5,
               outlier.size = outsize, outlier.shape = 15) + 
  theme_classic() + 
  labs(x = "", y = "", title = "WOMAC Score",
       subtitle = paste0("rho = ", round(Spear_MSNSE_WOMAC$estimate,3),
                         " [", round(Spear_MSNSE_WOMAC2[2],3)," - ",round(Spear_MSNSE_WOMAC2[3],3), "]",
                         "\n ",
                         ifelse(Spear_MSNSE_WOMAC$p.value<0.001, "p<0.001", paste0("p = ",round(Spear_MSNSE_WOMAC$p.value,3))) 
                         )) + 
  theme(plot.title = element_text(hjust=0.5, size = titlesize),
        plot.subtitle = element_text(hjust = 0.5, size = subsize),
        axis.text.x = element_text(size = xsize1),
        axis.text.y = element_text(size = ysize),
        plot.margin = margs1)

MSNSE_AMICAL<- results3 %>%
  ggplot(aes(x=MSNSE_Quint, y = AMICAL)) +
  stat_boxplot(geom= "errorbar", width = 0.2) +
  geom_boxplot(fill = "forestgreen", width= 0.5,
               outlier.size = outsize, outlier.shape = 15) + 
  theme_classic() + 
  labs(x = "", y = "", title = "AMICAL Score",
       subtitle = paste0("rho = ", round(Spear_MSNSE_AMICAL$estimate,3), 
                         " [", round(Spear_MSNSE_AMICAL2[2],3)," - ",round(Spear_MSNSE_AMICAL2[3],3), "]",
                         "\n ",
                         ifelse(Spear_MSNSE_AMICAL$p.value<0.001, "p<0.001", paste0("p = ",round(Spear_MSNSE_AMICAL$p.value,3))) 
       )) + 
  theme(plot.title = element_text(hjust=0.5, size = titlesize),
        plot.subtitle = element_text(hjust = 0.5, size = subsize),
        axis.text.x = element_text(size = xsize1),
        axis.text.y = element_text(size = ysize),
        plot.margin = margs1)

MSNSE_HAQ<- results3 %>%
  ggplot(aes(x=MSNSE_Quint, y = HAQ_Tot)) +
  stat_boxplot(geom= "errorbar", width = 0.2) +
  geom_boxplot(fill = "deepskyblue", width= 0.5,
               outlier.size = outsize, outlier.shape = 15) + 
  theme_classic() + 
  labs(x = "", y = "", title = "HAQ-DI Score",
       subtitle = paste0("rho = ", round(Spear_MSNSE_HAQ$estimate,3), 
                         " [", round(Spear_MSNSE_HAQ2[2],3)," - ",round(Spear_MSNSE_HAQ2[3],3), "]",
                         "\n ",
                         ifelse(Spear_MSNSE_HAQ$p.value<0.001, "p<0.001", paste0("p = ",round(Spear_MSNSE_HAQ$p.value,3))) 
       )) + 
  theme(plot.title = element_text(hjust=0.5, size = titlesize),
        plot.subtitle = element_text(hjust = 0.5, size = subsize),
        axis.text.x = element_text(size = xsize1),
        axis.text.y = element_text(size = ysize),
        plot.margin = margs2)

Education_WOMAC<- results3 %>%
  ggplot(aes(x= B1_Escol, y = WOMAC)) +
  stat_boxplot(geom= "errorbar", width = 0.2) +
  geom_boxplot(fill = "darkorange1", width= 0.5,
               outlier.size = outsize, outlier.shape = 15) + 
  theme_classic() + 
  scale_x_discrete(labels = c("LE","E","M","H","C")) +
  labs(x = "", y = "", title = "WOMAC Score",
       subtitle = paste0("rho = ", round(Spear_Escol_WOMAC$estimate,3), 
                         " [", round(Spear_Escol_WOMAC2[2],3)," - ",round(Spear_Escol_WOMAC2[3],3), "]",
                         "\n ",
                         ifelse(Spear_Escol_WOMAC$p.value<0.001, "p<0.001", paste0("p = ",round(Spear_Escol_WOMAC$p.value,3))) 
       )) + 
  theme(plot.title = element_text(hjust=0.5, size = titlesize),
        plot.subtitle = element_text(hjust = 0.5, size = subsize),
        axis.text.x = element_text(size = xsize2),
        axis.text.y = element_text(size = ysize),
        plot.margin = margs1)

Education_AMICAL<- results3 %>%
  ggplot(aes(x=B1_Escol, y = AMICAL)) +
  stat_boxplot(geom= "errorbar", width = 0.2) +
  geom_boxplot(fill = "forestgreen", width= 0.5, na.rm = T,
               outlier.size = outsize, outlier.shape = 15) + 
  theme_classic() + 
  scale_x_discrete(labels = c("LE","E","M","H","C")) +
  labs(x = "", y = "", title = "AMICAL Score",
       subtitle = paste0("rho = ", round(Spear_Escol_AMICAL$estimate,3), 
                         " [", round(Spear_Escol_AMICAL2[2],3)," - ",round(Spear_Escol_AMICAL2[3],3), "]",
                         "\n ",
                         ifelse(Spear_Escol_AMICAL$p.value<0.001, "p<0.001", paste0("p = ",round(Spear_Escol_AMICAL$p.value,3))) 
       )) + 
  theme(plot.title = element_text(hjust=0.5, size = titlesize),
        plot.subtitle = element_text(hjust = 0.5, size = subsize),
        axis.text.x = element_text(size = xsize2),
        axis.text.y = element_text(size = ysize),
        plot.margin = margs1)

Education_HAQ<- results3 %>%
  ggplot(aes(x=B1_Escol, y = HAQ_Tot)) +
  stat_boxplot(geom= "errorbar", width = 0.2) +
  geom_boxplot(fill = "deepskyblue", width= 0.5,
               outlier.size = outsize, outlier.shape = 15) + 
  theme_classic() + 
  scale_x_discrete(labels = c("LE","E","M","H","C")) +
  labs(x = "", y = "", title = "HAQ-DI Score",
       subtitle = paste0("rho = ", round(Spear_Escol_HAQ$estimate,3), 
                         " [", round(Spear_Escol_HAQ2[2],3)," - ",round(Spear_Escol_HAQ2[3],3), "]",
                         "\n ",
                         ifelse(Spear_Escol_HAQ$p.value<0.001, "p<0.001", paste0("p = ",round(Spear_Escol_HAQ$p.value,3))) 
       )) + 
  theme(plot.title = element_text(hjust=0.5, size = titlesize),
        plot.subtitle = element_text(hjust = 0.5, size = subsize),
        axis.text.x = element_text(size = xsize2),
        axis.text.y = element_text(size = ysize),
        plot.margin = margs2)

Ocup_WOMAC<- results3 %>%
  filter(!is.na(B2_Ocup)) %>%
  ggplot(aes(x=B2_Ocup, y = WOMAC)) +
  stat_boxplot(geom= "errorbar", width = 0.2) +
  geom_boxplot(fill = "darkorange1", width= 0.5,
               outlier.size = outsize, outlier.shape = 15) + 
  stat_pvalue_manual(pwc_Ocup_WOMAC_adj, hide.ns = TRUE,
                     y.position = 85,
                     size = sigsize) +
  theme_classic() + 
  scale_x_discrete(labels = c("ML","NMO","MO","H")) +
  labs(x = "", y = "", title = "WOMAC Score",
       subtitle = paste0("Kruskal-Wallis, p = ",round(Kruskal_Ocup_WOMAC$p.value,3))) + 
  theme(plot.title = element_text(hjust=0.5, size = titlesize),
        plot.subtitle = element_text(hjust = 0.5, size = subsize),
        axis.text.x = element_text(size = xsize2),
        axis.text.y = element_text(size = ysize),
        plot.margin = margs1)

Ocup_AMICAL<- results3 %>%
  filter(!is.na(B2_Ocup)) %>%
  ggplot(aes(x=B2_Ocup, y = AMICAL)) +
  stat_boxplot(geom= "errorbar", width = 0.2) +
  geom_boxplot(fill = "forestgreen", width= 0.5,
               outlier.size = outsize, outlier.shape = 15) + 
  stat_pvalue_manual(pwc_Ocup_AMICAL_adj, hide.ns = TRUE,
                     y.position = c(400,375),
                     size = sigsize) +
  theme_classic() + 
  scale_x_discrete(labels = c("ML","NMO","MO","H")) +
  labs(x = "", y = "", title = "AMICAL Score",
       subtitle = paste0("Kruskal-Wallis, p = ", round(Kruskal_Ocup_AMICAL$p.value,3))) + 
  theme(plot.title = element_text(hjust=0.5, size = titlesize),
        plot.subtitle = element_text(hjust = 0.5, size = subsize),
        axis.text.x = element_text(size = xsize2),
        axis.text.y = element_text(size = ysize),
        plot.margin = margs1)

Ocup_HAQ<- results3 %>%
  filter(!is.na(B2_Ocup)) %>%
  ggplot(aes(x=B2_Ocup, y = HAQ_Tot)) +
  stat_boxplot(geom= "errorbar", width = 0.2) +
  geom_boxplot(fill = "deepskyblue", width= 0.5,
               outlier.size = outsize, outlier.shape = 15) + 
  stat_pvalue_manual(pwc_Ocup_HAQ_adj, hide.ns = TRUE,
                     y.position = 2.7,
                     size = sigsize) +
  theme_classic() + 
  scale_x_discrete(labels = c("ML","NMO","MO","H")) +
  labs(x = "", y = "", title = "HAQ-DI Score",
       subtitle = paste0("Kruskal-Wallis, p = ", round(Kruskal_Ocup_HAQ$p.value,3))) + 
  theme(plot.title = element_text(hjust=0.5, size = titlesize),
        plot.subtitle = element_text(hjust = 0.5, size = subsize),
        axis.text.x = element_text(size = xsize2),
        axis.text.y = element_text(size = ysize),
        plot.margin = margs2)

MSNSE_graphs<- plot_grid(MSNSE_WOMAC, NULL,MSNSE_AMICAL, NULL,MSNSE_HAQ,
                         nrow=1,
                         rel_widths = c(1,-0.08,1,-0.08,1)
                         )
Education_graphs<- plot_grid(Education_WOMAC, NULL,Education_AMICAL, NULL,Education_HAQ,
                         nrow=1,
                         rel_widths = c(1,-0.08,1,-0.08,1)
                         )
Ocup_graphs<- plot_grid(Ocup_WOMAC, NULL,Ocup_AMICAL, NULL,Ocup_HAQ,
                             nrow=1,
                             rel_widths = c(1,-0.08,1,-0.08,1)
                             )

figure1<- plot_grid(MSNSE_graphs,Education_graphs, Ocup_graphs,
                    ncol = 1,
                    labels = "AUTO",
                    label_fontface = "plain",
                    label_size = 40
                    )
figure1

ggsave(plot = figure1, device = "png", filename = "Figure1-R.png", width = 620, height = 700, units = "mm")

#### Figure 2 ####
forestformat<- forest_model_format_options(
  text_size = 12
)


MSNSE_WOMAC_forest<- forestmodel::forest_model(MSNSE_WOMAC_lm,
                                               format_options = forestformat
)

MSNSE_AMICAL_forest<- forestmodel::forest_model(MSNSE_AMICAL_lm,
                                                format_options = forestformat
)

MSNSE_HAQ_forest<- forestmodel::forest_model(MSNSE_HAQ_lm,
                                             format_options = forestformat
)

EduMad_WOMAC_forest<- forestmodel::forest_model(EduMad_WOMAC_lm,
                                                format_options = forestformat
)
EduMad_AMICAL_forest<- forestmodel::forest_model(EduMad_AMICAL_lm,
                                                 format_options = forestformat
)

EduMad_HAQ_forest<- forestmodel::forest_model(EduMad_HAQ_lm,
                                              format_options = forestformat
)
Mutual_WOMAC_forest<- forestmodel::forest_model(Mutual_WOMAC_lm,
                                                format_options = forestformat
)

Mutual_AMICAL_forest<- forestmodel::forest_model(Mutual_AMICAL_lm,
                                                 format_options = forestformat
)

Mutual_HAQ_forest<- forestmodel::forest_model(Mutual_HAQ_lm,
                                              format_options = forestformat
)

Trayectoria_WOMAC_forest<- forestmodel::forest_model(Trayectoria_WOMAC_lm,
                                                     format_options = forestformat
)

Trayectoria_AMICAL_forest<- forestmodel::forest_model(Trayectoria_AMICAL_lm,
                                                      format_options = forestformat
)

Trayectoria_HAQ_forest<- forestmodel::forest_model(Trayectoria_HAQ_lm,
                                                   format_options = forestformat
)




fplabely<- 0.8
fplabelx<- 0.5
fplabelsize<- 24

MSNSE_forests<- plot_grid(NULL,NULL,NULL,MSNSE_WOMAC_forest,MSNSE_AMICAL_forest, MSNSE_HAQ_forest,
                          nrow=2,
                          rel_heights = c(0.15,1),
                          labels = c(paste0("WOMAC Score^(1/2) \n Model adj. R2 = ", round(summary(MSNSE_WOMAC_lm)$adj.r.squared,3)),
                                     paste0("AMICAL Score \n Model adj. R2 = ", round(summary(MSNSE_AMICAL_lm)$adj.r.squared,3)),
                                     paste0("HAQ-DI Score \n Model adj. R2 = ", round(summary(MSNSE_WOMAC_lm)$adj.r.squared,3)),
                                     "","",""),
                          label_y = fplabely,
                          label_x = fplabelx,
                          hjust = 0.5,
                          vjust = 1,
                          label_fontface = "plain",
                          label_size = fplabelsize
                          )
EduMad_forests<- plot_grid(NULL,NULL,NULL,EduMad_WOMAC_forest,EduMad_AMICAL_forest, EduMad_HAQ_forest,
                           nrow=2,
                           rel_heights = c(0.15,1),
                          labels = c(paste0("WOMAC Score^(1/2) \n Model adj. R2 = ", round(summary(EduMad_WOMAC_lm)$adj.r.squared,3)),
                                     paste0("AMICAL Score \n Model adj. R2 = ", round(summary(EduMad_AMICAL_lm)$adj.r.squared,3)),
                                     paste0("HAQ-DI Score \n Model adj. R2 = ", round(summary(EduMad_HAQ_lm)$adj.r.squared,3)),
                                     "","",""),
                          label_y = fplabely,
                          label_x = fplabelx,
                          hjust = 0.5,
                          vjust = 1,
                          label_fontface = "plain",
                          label_size = fplabelsize
)
Mutual_forests<- plot_grid(NULL,NULL,NULL,Mutual_WOMAC_forest, Mutual_AMICAL_forest, Mutual_HAQ_forest,
                           nrow=2,
                           rel_heights = c(0.15,1),
                           labels = c(paste0("WOMAC Score^(1/2) \n Model adj. R2 = ", round(summary(Mutual_WOMAC_lm)$adj.r.squared,3)),
                                      paste0("AMICAL Score \n Model adj. R2 = ", round(summary(Mutual_AMICAL_lm)$adj.r.squared,3)),
                                      paste0("HAQ-DI Score \n Model adj. R2 = ", round(summary(Mutual_HAQ_lm)$adj.r.squared,3)),
                                      "","",""),
                           label_y = fplabely,
                           label_x = fplabelx,
                           hjust = 0.5,
                           vjust = 1,
                           label_fontface = "plain",
                           label_size = fplabelsize
)
Trayectoria_forests<- plot_grid(NULL,NULL,NULL,Trayectoria_WOMAC_forest,Trayectoria_AMICAL_forest, Trayectoria_HAQ_forest,
                                nrow=2,
                                rel_heights = c(0.15,1),
                           labels = c(paste0("WOMAC Score^(1/2) \n Model adj. R2 = ", round(summary(Trayectoria_WOMAC_lm)$adj.r.squared,3)),
                                      paste0("AMICAL Score \n Model adj. R2 = ", round(summary(Trayectoria_AMICAL_lm)$adj.r.squared,3)),
                                      paste0("HAQ-DI Score \n Model adj. R2 = ", round(summary(Trayectoria_HAQ_lm)$adj.r.squared,3)),
                                      "","",""),
                           label_y = fplabely,
                           label_x = fplabelx,
                           hjust = 0.5,
                           vjust = 1,
                           label_fontface = "plain",
                           label_size = fplabelsize
)


figure2<- plot_grid(MSNSE_forests, Trayectoria_forests,
                    ncol = 1,
                    labels = "AUTO",
                    label_fontface = "plain",
                    label_size = 40
)
ggsave(plot = figure2, device = "png", filename = "Figure2-R.png", width = 620, height = 700/3*2, units = "mm")


sup_figure2<- plot_grid(EduMad_forests, Mutual_forests,
                    ncol = 1,
                    labels = "AUTO",
                    label_fontface = "plain",
                    label_size = 40
)
ggsave(plot = sup_figure2, device = "png", filename = "Sup-Figure2-R.png", width = 620, height = 700/3*2, units = "mm")



#### Trayectorias ####

Trayectoria_WOMAC_graph<- results_dicot %>%
  filter(!is.na(Trayectoria)) %>%
  ggplot(aes(x=Trayectoria, y = WOMAC)) +
  stat_boxplot(geom= "errorbar", width = 0.2) +
  geom_boxplot(fill = "darkorange1", width= 0.5,
               outlier.size = outsize, outlier.shape = 15) + 
  stat_pvalue_manual(pwc_Trayectoria_WOMAC_adj, hide.ns = TRUE,
                     
                     size = sigsize) +
  theme_classic() + 
  labs(x = "", y = "", title = "WOMAC Score",
       subtitle = paste0("Kruskal-Wallis, p = ",round(Kruskal_Trayectoria_WOMAC$p.value,3))) + 
  theme(plot.title = element_text(hjust=0.5, size = titlesize),
        plot.subtitle = element_text(hjust = 0.5, size = subsize),
        axis.text.x = element_text(size = xsize1),
        axis.text.y = element_text(size = ysize),
        plot.margin = margs1)

Trayectoria_AMICAL_graph<- results_dicot %>%
  filter(!is.na(Trayectoria)) %>%
  ggplot(aes(x=Trayectoria, y = AMICAL)) +
  stat_boxplot(geom= "errorbar", width = 0.2) +
  geom_boxplot(fill = "forestgreen", width= 0.5,
               outlier.size = outsize, outlier.shape = 15) + 
  stat_pvalue_manual(pwc_Trayectoria_AMICAL_adj, hide.ns = TRUE,
                     
                     size = sigsize) +
  theme_classic() + 
  labs(x = "", y = "", title = "AMICAL Score",
       subtitle = paste0("Kruskal-Wallis, p = ",round(Kruskal_Trayectoria_AMICAL$p.value,3))) + 
  theme(plot.title = element_text(hjust=0.5, size = titlesize),
        plot.subtitle = element_text(hjust = 0.5, size = subsize),
        axis.text.x = element_text(size = xsize1),
        axis.text.y = element_text(size = ysize),
        plot.margin = margs1)

Trayectoria_HAQ_graph<- results_dicot %>%
  filter(!is.na(Trayectoria)) %>%
  ggplot(aes(x=Trayectoria, y = HAQ_Tot)) +
  stat_boxplot(geom= "errorbar", width = 0.2) +
  geom_boxplot(fill = "deepskyblue", width= 0.5,
               outlier.size = outsize, outlier.shape = 15) + 
  stat_pvalue_manual(pwc_Trayectoria_HAQ_adj, hide.ns = TRUE,
                     
                     size = sigsize) +
  theme_classic() + 
  labs(x = "", y = "", title = "HAQ-DI Score",
       subtitle = paste0("Kruskal-Wallis, p = ",round(Kruskal_Trayectoria_HAQ$p.value,3))) + 
  theme(plot.title = element_text(hjust=0.5, size = titlesize),
        plot.subtitle = element_text(hjust = 0.5, size = subsize),
        axis.text.x = element_text(size = xsize1),
        axis.text.y = element_text(size = ysize),
        plot.margin = margs1)

Trayectoria_graphs<- plot_grid(Trayectoria_WOMAC_graph, NULL ,Trayectoria_AMICAL_graph, NULL,Trayectoria_HAQ_graph,
                        nrow=1,
                        rel_widths = c(1,-0.08,1,-0.08,1)
)
ggsave(plot = Trayectoria_graphs, device = "png", filename = "Trayectorias.png", width = 620, height = 700/3, units = "mm")

####Supplementary figure 1 ####

EduMad_WOMAC<- results3 %>%
  filter(!is.na(B4_Escol_Madre)) %>%
  ggplot(aes(x= B4_Escol_Madre, y = WOMAC)) +
  stat_boxplot(geom= "errorbar", width = 0.2) +
  geom_boxplot(fill = "darkorange1", width= 0.5,
               outlier.size = outsize, outlier.shape = 15) + 
  theme_classic() + 
  scale_x_discrete(labels = c("LE","E","M","H","C")) +
  labs(x = "", y = "", title = "WOMAC Score",
       subtitle = paste0("rho = ", round(Spear_EscoMad_WOMAC$estimate,3), 
                         " [", round(Spear_EscoMad_WOMAC2[2],3)," - ",round(Spear_EscoMad_WOMAC2[3],3), "]",
                         "\n ",
                         ifelse(Spear_EscoMad_WOMAC$p.value<0.001, "p<0.001", paste0("p = ",round(Spear_EscoMad_WOMAC$p.value,3))) 
       )) + 
  theme(plot.title = element_text(hjust=0.5, size = titlesize),
        plot.subtitle = element_text(hjust = 0.5, size = subsize),
        axis.text.x = element_text(size = xsize2),
        axis.text.y = element_text(size = ysize),
        plot.margin = margs1)

EduMad_AMICAL<- results3 %>%
  filter(!is.na(B4_Escol_Madre)) %>%
  ggplot(aes(x= B4_Escol_Madre, y = AMICAL)) +
  stat_boxplot(geom= "errorbar", width = 0.2) +
  geom_boxplot(fill = "forestgreen", width= 0.5, na.rm = T,
               outlier.size = outsize, outlier.shape = 15) + 
  theme_classic() + 
  scale_x_discrete(labels = c("LE","E","M","H","C")) +
  labs(x = "", y = "", title = "AMICAL Score",
       subtitle = paste0("rho = ", round(Spear_EscoMad_AMICAL$estimate,3), 
                         " [", round(Spear_EscoMad_AMICAL2[2],3)," - ",round(Spear_EscoMad_AMICAL2[3],3), "]",
                         "\n ",
                         ifelse(Spear_EscoMad_AMICAL$p.value<0.001, "p<0.001", paste0("p = ",round(Spear_EscoMad_AMICAL$p.value,3))) 
       )) + 
  theme(plot.title = element_text(hjust=0.5, size = titlesize),
        plot.subtitle = element_text(hjust = 0.5, size = subsize),
        axis.text.x = element_text(size = xsize2),
        axis.text.y = element_text(size = ysize),
        plot.margin = margs1)

EduMad_HAQ<- results3 %>%
  filter(!is.na(B4_Escol_Madre)) %>%
  ggplot(aes(x= B4_Escol_Madre, y = HAQ_Tot)) +
  stat_boxplot(geom= "errorbar", width = 0.2) +
  geom_boxplot(fill = "deepskyblue", width= 0.5,
               outlier.size = outsize, outlier.shape = 15) + 
  theme_classic() + 
  scale_x_discrete(labels = c("LE","E","M","H","C")) +
  labs(x = "", y = "", title = "HAQ-DI Score",
       subtitle = paste0("rho = ", round(Spear_EscoMad_HAQ$estimate,3), 
                         " [", round(Spear_EscoMad_HAQ2[2],3)," - ",round(Spear_EscoMad_HAQ2[3],3), "]",
                         "\n ",
                         ifelse(Spear_EscoMad_HAQ$p.value<0.001, "p<0.001", paste0("p = ",round(Spear_EscoMad_HAQ$p.value,3))) 
       )) + 
  theme(plot.title = element_text(hjust=0.5, size = titlesize),
        plot.subtitle = element_text(hjust = 0.5, size = subsize),
        axis.text.x = element_text(size = xsize2),
        axis.text.y = element_text(size = ysize),
        plot.margin = margs2)

OcuMad_WOMAC<- results3 %>%
  filter(!is.na(B6_Ocup_Madre)) %>%
  ggplot(aes(x=B6_Ocup_Madre, y = WOMAC)) +
  stat_boxplot(geom= "errorbar", width = 0.2) +
  geom_boxplot(fill = "darkorange1", width= 0.5,
               outlier.size = outsize, outlier.shape = 15) + 
  stat_pvalue_manual(pwc_OcuMad_WOMAC_adj, hide.ns = TRUE,
                     y.position = 85,
                     size = sigsize) +
  theme_classic() + 
  scale_x_discrete(labels = c("ML","NMO","MO","H")) +
  labs(x = "", y = "", title = "WOMAC Score",
       subtitle = paste0("Kruskal-Wallis, p = ",round(Kruskal_OcuMad_WOMAC$p.value,3))) + 
  theme(plot.title = element_text(hjust=0.5, size = titlesize),
        plot.subtitle = element_text(hjust = 0.5, size = subsize),
        axis.text.x = element_text(size = xsize2),
        axis.text.y = element_text(size = ysize),
        plot.margin = margs1)

OcuMad_AMICAL<- results3 %>%
  filter(!is.na(B6_Ocup_Madre)) %>%
  ggplot(aes(x=B6_Ocup_Madre, y = AMICAL)) +
  stat_boxplot(geom= "errorbar", width = 0.2) +
  geom_boxplot(fill = "forestgreen", width= 0.5,
               outlier.size = outsize, outlier.shape = 15) + 
  stat_pvalue_manual(pwc_OcuMad_AMICAL_adj, hide.ns = TRUE,
                     y.position = c(400,375),
                     size = sigsize) +
  theme_classic() + 
  scale_x_discrete(labels = c("ML","NMO","MO","H")) +
  labs(x = "", y = "", title = "AMICAL Score",
       subtitle = paste0("Kruskal-Wallis, p = ", round(Kruskal_OcuMad_AMICAL$p.value,3))) + 
  theme(plot.title = element_text(hjust=0.5, size = titlesize),
        plot.subtitle = element_text(hjust = 0.5, size = subsize),
        axis.text.x = element_text(size = xsize2),
        axis.text.y = element_text(size = ysize),
        plot.margin = margs1)

OcuMad_HAQ<- results3 %>%
  filter(!is.na(B6_Ocup_Madre)) %>%
  ggplot(aes(x=B6_Ocup_Madre, y = HAQ_Tot)) +
  stat_boxplot(geom= "errorbar", width = 0.2) +
  geom_boxplot(fill = "deepskyblue", width= 0.5,
               outlier.size = outsize, outlier.shape = 15) + 
  stat_pvalue_manual(pwc_OcuMad_HAQ_adj, hide.ns = TRUE,
                     y.position = 2.7,
                     size = sigsize) +
  theme_classic() + 
  scale_x_discrete(labels = c("ML","NMO","MO","H")) +
  labs(x = "", y = "", title = "HAQ-DI Score",
       subtitle = paste0("Kruskal-Wallis, p = ", round(Kruskal_OcuMad_HAQ$p.value,3))) + 
  theme(plot.title = element_text(hjust=0.5, size = titlesize),
        plot.subtitle = element_text(hjust = 0.5, size = subsize),
        axis.text.x = element_text(size = xsize2),
        axis.text.y = element_text(size = ysize),
        plot.margin = margs2)




EduPad_WOMAC<- results3 %>%
  filter(!is.na(B3_Escol_Padre)) %>%
  ggplot(aes(x= B3_Escol_Padre, y = WOMAC)) +
  stat_boxplot(geom= "errorbar", width = 0.2) +
  geom_boxplot(fill = "darkorange1", width= 0.5,
               outlier.size = outsize, outlier.shape = 15) + 
  theme_classic() + 
  scale_x_discrete(labels = c("LE","E","M","H","C")) +
  labs(x = "", y = "", title = "WOMAC Score",
       subtitle = paste0("rho = ", round(Spear_EscoPad_WOMAC$estimate,3), 
                         " [", round(Spear_EscoPad_WOMAC2[2],3)," - ",round(Spear_EscoPad_WOMAC2[3],3), "]",
                         "\n ",
                         ifelse(Spear_EscoPad_WOMAC$p.value<0.001, "p<0.001", paste0("p = ",round(Spear_EscoPad_WOMAC$p.value,3))) 
       )) + 
  theme(plot.title = element_text(hjust=0.5, size = titlesize),
        plot.subtitle = element_text(hjust = 0.5, size = subsize),
        axis.text.x = element_text(size = xsize2),
        axis.text.y = element_text(size = ysize),
        plot.margin = margs1)

EduPad_AMICAL<- results3 %>%
  filter(!is.na(B3_Escol_Padre)) %>%
  ggplot(aes(x= B3_Escol_Padre, y = AMICAL)) +
  stat_boxplot(geom= "errorbar", width = 0.2) +
  geom_boxplot(fill = "forestgreen", width= 0.5, na.rm = T,
               outlier.size = outsize, outlier.shape = 15) + 
  theme_classic() + 
  scale_x_discrete(labels = c("LE","E","M","H","C")) +
  labs(x = "", y = "", title = "AMICAL Score",
       subtitle = paste0("rho = ", round(Spear_EscoPad_AMICAL$estimate,3), 
                         " [", round(Spear_EscoPad_AMICAL2[2],3)," - ",round(Spear_EscoPad_AMICAL2[3],3), "]",
                         "\n ",
                         ifelse(Spear_EscoPad_AMICAL$p.value<0.001, "p<0.001", paste0("p = ",round(Spear_EscoPad_AMICAL$p.value,3))) 
       )) + 
  theme(plot.title = element_text(hjust=0.5, size = titlesize),
        plot.subtitle = element_text(hjust = 0.5, size = subsize),
        axis.text.x = element_text(size = xsize2),
        axis.text.y = element_text(size = ysize),
        plot.margin = margs1)

EduPad_HAQ<- results3 %>%
  filter(!is.na(B3_Escol_Padre)) %>%
  ggplot(aes(x= B3_Escol_Padre, y = HAQ_Tot)) +
  stat_boxplot(geom= "errorbar", width = 0.2) +
  geom_boxplot(fill = "deepskyblue", width= 0.5,
               outlier.size = outsize, outlier.shape = 15) + 
  theme_classic() + 
  scale_x_discrete(labels = c("LE","E","M","H","C")) +
  labs(x = "", y = "", title = "HAQ-DI Score",
       subtitle = paste0("rho = ", round(Spear_EscoPad_HAQ$estimate,3), 
                         " [", round(Spear_EscoPad_HAQ2[2],3)," - ",round(Spear_EscoPad_HAQ2[3],3), "]",
                         "\n ",
                         ifelse(Spear_EscoPad_HAQ$p.value<0.001, "p<0.001", paste0("p = ",round(Spear_EscoPad_HAQ$p.value,3))) 
       )) + 
  theme(plot.title = element_text(hjust=0.5, size = titlesize),
        plot.subtitle = element_text(hjust = 0.5, size = subsize),
        axis.text.x = element_text(size = xsize2),
        axis.text.y = element_text(size = ysize),
        plot.margin = margs2)

OcuPad_WOMAC<- results3 %>%
  filter(!is.na(B5_Ocup_Padre)) %>%
  ggplot(aes(x=B5_Ocup_Padre, y = WOMAC)) +
  stat_boxplot(geom= "errorbar", width = 0.2) +
  geom_boxplot(fill = "darkorange1", width= 0.5,
               outlier.size = outsize, outlier.shape = 15) + 
  stat_pvalue_manual(pwc_OcuPad_WOMAC_adj, hide.ns = TRUE,
                     y.position = 85,
                     size = sigsize) +
  theme_classic() + 
  scale_x_discrete(labels = c("ML","NMO","MO","H")) +
  labs(x = "", y = "", title = "WOMAC Score",
       subtitle = paste0("Kruskal-Wallis, p = ",round(Kruskal_OcuPad_WOMAC$p.value,3))) + 
  theme(plot.title = element_text(hjust=0.5, size = titlesize),
        plot.subtitle = element_text(hjust = 0.5, size = subsize),
        axis.text.x = element_text(size = xsize2),
        axis.text.y = element_text(size = ysize),
        plot.margin = margs1)

OcuPad_AMICAL<- results3 %>%
  filter(!is.na(B5_Ocup_Padre)) %>%
  ggplot(aes(x=B5_Ocup_Padre, y = AMICAL)) +
  stat_boxplot(geom= "errorbar", width = 0.2) +
  geom_boxplot(fill = "forestgreen", width= 0.5,
               outlier.size = outsize, outlier.shape = 15) + 
  stat_pvalue_manual(pwc_OcuPad_AMICAL_adj, hide.ns = TRUE,
                     size = sigsize) +
  theme_classic() + 
  scale_x_discrete(labels = c("ML","NMO","MO","H")) +
  labs(x = "", y = "", title = "AMICAL Score",
       subtitle = paste0("Kruskal-Wallis, p = ", round(Kruskal_OcuPad_AMICAL$p.value,3))) + 
  theme(plot.title = element_text(hjust=0.5, size = titlesize),
        plot.subtitle = element_text(hjust = 0.5, size = subsize),
        axis.text.x = element_text(size = xsize2),
        axis.text.y = element_text(size = ysize),
        plot.margin = margs1)


OcuPad_HAQ<- results3 %>%
  filter(!is.na(B5_Ocup_Padre)) %>%
  ggplot(aes(x=B5_Ocup_Padre, y = HAQ_Tot)) +
  stat_boxplot(geom= "errorbar", width = 0.2) +
  geom_boxplot(fill = "deepskyblue", width= 0.5,
               outlier.size = outsize, outlier.shape = 15) + 
  stat_pvalue_manual(pwc_OcuPad_HAQ_adj, hide.ns = TRUE,
                     y.position = 2.7,
                     size = sigsize) +
  theme_classic() + 
  scale_x_discrete(labels = c("ML","NMO","MO","H")) +
  labs(x = "", y = "", title = "HAQ-DI Score",
       subtitle = paste0("Kruskal-Wallis, p = ", round(Kruskal_OcuPad_HAQ$p.value,3))) + 
  theme(plot.title = element_text(hjust=0.5, size = titlesize),
        plot.subtitle = element_text(hjust = 0.5, size = subsize),
        axis.text.x = element_text(size = xsize2),
        axis.text.y = element_text(size = ysize),
        plot.margin = margs2)

EduMad_graphs<- plot_grid(EduMad_WOMAC, NULL,EduMad_AMICAL, NULL,EduMad_HAQ,
                             nrow=1,
                             rel_widths = c(1,-0.08,1,-0.08,1)
)
OcuMad_graphs<- plot_grid(OcuMad_WOMAC, NULL,OcuMad_AMICAL, NULL,OcuMad_HAQ,
                        nrow=1,
                        rel_widths = c(1,-0.08,1,-0.08,1)
)
EduPad_graphs<- plot_grid(EduPad_WOMAC, NULL,EduPad_AMICAL, NULL,EduPad_HAQ,
                          nrow=1,
                          rel_widths = c(1,-0.08,1,-0.08,1)
)
OcuPad_graphs<- plot_grid(OcuPad_WOMAC, NULL,OcuPad_AMICAL, NULL,OcuPad_HAQ,
                          nrow=1,
                          rel_widths = c(1,-0.08,1,-0.08,1)
)


sup_figure1<- plot_grid(NULL,EduMad_graphs,NULL,OcuMad_graphs, NULL, EduPad_graphs, NULL, OcuPad_graphs,
                    ncol = 1,
                    rel_heights = c(0.15,1,0.15,1,0.15,1,0.15,1),
                    labels = c("Maternal Education","A","Maternal Occupation","B","Paternal Education","C","Paternal Occupation","D"),
                    label_x = c(0.3,0,0.3,0,0.3,0,0.3,0),
                    label_fontface = "plain",
                    label_size = 40
)
sup_figure1

ggsave(plot = sup_figure1, device = "png", filename = "Sup-Figure1-R.png", width = 620, height = 700/3*4, units = "mm")
