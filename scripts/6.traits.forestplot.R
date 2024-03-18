#####FOREST PLOT FOR PLANT TRAITS (COMPATIBILITIES)
rm(list = ls(all.names = TRUE)) 
pacman::p_unload(pacman::p_loaded(), character.only = TRUE) 
pacman::p_load(tidyverse,rtry,rJava, dplyr,metafor,cowplot,orchaRd,ggbeeswarm,tidyr,ggthemes,sp,broom,lemon,MuMIn,glmulti,PerformanceAnalytics,GGally,gt,geodata,
               ggmap,mapproj,glmulti,MuMIn, RColorBrewer,ggsci)
#######
tbl.com<- read.csv("data\\4.complete_table.csv")
##delete data without effect size (there is one without country, this is removed from the map)
###remove also observations on abundance and visitation rates
tbl.com<-tbl.com[!is.na(tbl.com$yi), ]
unique(tbl.com$Pollinator.variable.measure)
tbl.com<-tbl.com%>%
  filter(!str_detect(Pollinator.variable.measure, "abundance")) %>%
  filter(!str_detect(Pollinator.variable.measure, "visitation"))
tbl.com<-tbl.com[!is.na(effect.size.total$vi), ]

###FILTER FOR INFORMATION OF COMPATIBILITIES
comp<- tbl.com %>%
  filter(!is.na(IMPUTED_Compatibility) & IMPUTED_Compatibility != "" )

c <-comp%>%
  group_by(IMPUTED_Compatibility,Plant.species.family)%>%
  summarise(n=n())

###MODELS
m0<- rma.mv(yi,vi,random=list(~ 1 | Year, ~ 1 | Title), data=comp, method="ML")
m0.comp<- rma.mv(yi,vi,mods=~IMPUTED_Compatibility-1, random=list(~ 1 | Year, ~ 1 | Title), data=comp, method="ML")
anova(m0,m0.comp)
##reml for estimates
m0.comp.reml<- rma.mv(yi,vi,mods=~IMPUTED_Compatibility-1,random=list(~ 1 | Year, ~ 1 | Title), data=comp, method="REML")
r2_ml(m0.comp.reml)
res.comp <- orchaRd::mod_results(m0.comp.reml, mod = "IMPUTED_Compatibility", group = "Title")

library(latex2exp)
pred.m0.ps<- comp%>%
  filter(IMPUTED_Compatibility=="partially_self_compatible")
k.1 <- dim(pred.m0.ps)[1]
stdy.1 <- pred.m0.ps %>% summarise(stdy = length(unique(Title)))
anno.1 <- annotate("text", x = 1.2, y = 2.5, size=5.5, label = TeX(paste0("\\textit{k} = ", k.1, " (", stdy.1, ")")))

pred.m0.sf<- comp%>%
  filter(IMPUTED_Compatibility=="self_compatible")
k.2<- dim(pred.m0.sf)[1]
stdy.2 <- pred.m0.sf %>% summarise(stdy = length(unique(Title)))
anno.2 <- annotate("text", x = 2.2, y = 2.5, size=5.5, label = TeX(paste0("\\textit{k} = ", k.2, " (", stdy.2, ")")))

pred.m0.sic<- comp%>%
  filter(IMPUTED_Compatibility=="self_incompatible")
k.3 <- dim(pred.m0.sic)[1]
stdy.3 <- pred.m0.sic %>% summarise(stdy = length(unique(Title)))
anno.3 <- annotate("text", x = 3.2, y = 2.5, size=5.5, label = TeX(paste0("\\textit{k} = ", k.3, " (", stdy.3, ")")))

plot.comp<-orchaRd::orchard_plot(res.comp, xlab = "Hedges´g", angle = 45, g = FALSE, k=F, alpha=0.7,branch.size=10,trunk.size = 11,legend.pos = "none",
                      condition.lab = "IMPUTED_Compatibility") + theme(legend.direction = "vertical")+
  theme(legend.title = element_text(size =15),
        legend.text = element_text(size = 15),
        text=element_text(size=16),
        axis.text.y = element_text(size =16),
        plot.title = element_text(lineheight=.8),
        axis.text.x = element_text(size =18),
        axis.title= element_text(size=18))+
 scale_fill_manual("IMPUTED_Compatibilities", values = c("#E64B35B2","#4DBBD5B2","#00A087B2"))+
  scale_color_manual("IMPUTED_Compatibilities", values = c("#E64B35B2","#4DBBD5B2","#00A087B2"))+
  scale_y_continuous(limits = c(-8.8,5))+
  scale_x_discrete(labels=c("Partilly self-compatible", "Self-compatible", "Self-incompatible"))+
anno.1+anno.2+anno.3+
  annotate(geom = "text", x =2.7, y = -6.25, 
           color = "black", size = 5, label=TeX(paste0("95% CI = ", round(res.comp$mod_table[3,3], 3), " to ", round(res.comp$mod_table[3,4], 3))))+
  annotate(geom = "text", x = 1.7, y = -6.25, 
           color = "black", size = 5, label=TeX(paste0("95% CI = ", round(res.comp$mod_table[2,3], 3), " to ", round(res.comp$mod_table[2,4], 3))))+
  annotate(geom = "text", x = 0.7, y = -6.25, 
           color = "black", size = 5, label=TeX(paste0("95% CI = ", round(res.comp$mod_table[1,3], 3), " to ", round(res.comp$mod_table[1,4], 3))))
  
  

plot.comp
ggsave(plot.comp, filename="RData/figures/comp.png",width = 9, height = 6)    

###ORCHARD PLOT FOR SELF COMPATIBLE PLANT SPECIES FOR SUPPL MATERIAL:
###seed set and fruit weight are more affected in compatible plants than fruit set.                                                                                           

fwe<-tbl.com%>%
  subset(IMPUTED_Compatibility=="self_compatible")

fwe.fruitset<- fwe%>%
  filter(Reproductive.succes.measure=="fruit set")
k.1 <- dim(fwe.fruitset)[1]
stdy.1 <- fwe.fruitset %>% summarise(stdy = length(unique(Title)))
anno.1 <- annotate("text", x = 1.2, y = 2.5, size=5.5, label = TeX(paste0("\\textit{k} = ", k.1, " (", stdy.1, ")")))


fwe.seedset<- fwe%>%
  filter(Reproductive.succes.measure=="seed set")
k.2 <- dim(fwe.seedset)[1]
stdy.2 <- fwe.seedset %>% summarise(stdy = length(unique(Title)))
anno.2 <- annotate("text", x = 2.2, y = 2.5, size=5.5, label = TeX(paste0("\\textit{k} = ", k.2, " (", stdy.2, ")")))

fwe.fruitwe<- fwe%>%
  filter(Reproductive.succes.measure=="fruit weight")
k.3 <- dim(fwe.fruitwe)[1]
stdy.3 <- fwe.fruitwe %>% summarise(stdy = length(unique(Title)))
anno.3 <- annotate("text", x = 3.2, y = 2.5, size=5.5, label = TeX(paste0("\\textit{k} = ", k.3, " (", stdy.3, ")")))


fwe.ml<- rma.mv(yi,vi,random=list(~ 1 | Year, ~ 1 | Title), data=fwe, method="ML")
m0.fwe.ml<- rma.mv(yi,vi,mods=~Reproductive.succes.measure-1,random=list(~ 1 | Year, ~ 1 | Title), data=fwe, method="ML")
anova(fwe.ml,m0.fwe.ml)

m0.fwe.reml<- rma.mv(yi,vi,mods=~Reproductive.succes.measure-1,random=list(~ 1 | Year, ~ 1 | Title), data=fwe, method="REML")
r2_ml(m0.fwe.reml)

res.fwe <- orchaRd::mod_results(m0.fwe.reml, mod = "Reproductive.succes.measure", group = "Title",
                                at = list(Reproductive.succes.measure = c("fruit set", "seed set", "fruit weight")), subset = TRUE)
plot.fwe<-orchaRd::orchard_plot(res.fwe, xlab = "Hedges´g", angle = 45, g = FALSE, k=F, alpha=0.7,branch.size=3,trunk.size = 11,legend.pos = "bottom.right",
                                 condition.lab = "Reproductive.succes.measure") + theme(legend.direction = "horizontal")+
  theme(legend.title = element_text(size =13),
        legend.text = element_text(size = 13),
        text=element_text(size=15),
        axis.text.y = element_text(size =15),
        plot.title = element_text(lineheight=.8))+
  scale_fill_manual("Reproductive succes measure", values = c("#E64B35B2","#4DBBD5B2","#00A087B2"))+
  scale_color_manual("Reproductive succes measure", values = c("#E64B35B2","#4DBBD5B2","#00A087B2"))+
  anno.1+anno.2+anno.3+
  scale_y_continuous(limits = c(-8.8,5))+
  annotate(geom = "text", x =3.2, y = -5.25, 
           color = "black", size = 5, label=TeX(paste0("95% CI = ", round(res.fwe$mod_table[3,3], 3), " to ", round(res.fwe$mod_table[3,4], 3))))+
  annotate(geom = "text", x = 2.2, y = -5.25, 
           color = "black", size = 5, label=TeX(paste0("95% CI = ", round(res.fwe$mod_table[2,3], 3), " to ", round(res.fwe$mod_table[2,4], 3))))+
  annotate(geom = "text", x = 1.2, y = -5.25, 
           color = "black", size = 5, label=TeX(paste0("95% CI = ", round(res.fwe$mod_table[1,3], 3), " to ", round(res.fwe$mod_table[1,4], 3))))

ggsave(plot.fwe, filename="RData/figures/compatiblesp.png",width = 9, height = 6)    

