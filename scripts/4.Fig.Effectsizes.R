######### FIGURES ORCHARD PLOT FOR EFFECT SIZES#####

rm(list = ls(all.names = TRUE)) 
pacman::p_unload(pacman::p_loaded(), character.only = TRUE) 
pacman::p_load(tidyverse,rJava, dplyr,metafor,cowplot,orchaRd,ggbeeswarm,tidyr,ggthemes,sp,broom,lemon,MuMIn,glmulti,PerformanceAnalytics,GGally,gt,geodata,
               ggmap,mapproj,glmulti,MuMIn, devtools)
devtools::install_github("daniel1noble/orchaRd", force = TRUE)
library(orchaRd)
repr.success <- read.csv("data/3.effectsizes_clean_nw.csv")

##delete data without effect size (there is one without country, this is removed from the map)
###remove also observations on abundance and visitation rates
effect.size.total<-repr.success[!is.na(repr.success$yi), ]
unique(effect.size.total$Pollinator.variable.measure)
effect.size.total<-effect.size.total%>%
  filter(!str_detect(Pollinator.variable.measure, "abundance")) %>%
  filter(!str_detect(Pollinator.variable.measure, "visitation"))
effect.size.total<-effect.size.total[!is.na(effect.size.total$vi), ]
excel_data<-effect.size.total%>%
  group_by(Author,Year,Title)%>%
  summarise(N_observations=n())
writexl::write_xlsx(excel_data, "data/papers.xlsx")
#effect.size.total <-effect.size.total%>%
# filter(!is.na(Country))

effect.size.total$Plant.species<- recode(effect.size.total$Plant.species,  
                               "Brassica oleracea var.capita"="Brassica oleracea var.capitata")
unique(effect.size.total$Plant.species)##224 sp, removing three about community effect and same sp red and white.
unique(effect.size.total$Plant.species.family)#66 fam
unique(effect.size.total$Title)#207 estudios,780 obs

##INFO THAT WE HAVE
effect.size.total$Country <- recode(effect.size.total$Country, "Perú"="Peru",
                                    
                                    "Spain, Canary Islands"="Canary Islands",
                                    "England"="UK")

studies_country <- effect.size.total%>%
  separate_rows(Country,sep=", ")%>%
  separate_rows(Country,sep=" & ")%>%
  filter(!is.na(Country))%>%
  group_by(Country)%>%
  summarise(obs.country=n(),
            studies_country =n_distinct(Title))


studies_country<- studies_country%>%
  mutate(total.studies=sum(studies_country),
             total.obs=sum(obs.country),
           percentage.est=(studies_country/total.studies)*100,
           percentage.obs=(obs.country/total.obs)*100)
##


##lollipop chard
studies<- studies_country%>%
  mutate(percentage.est=scales::number(percentage.est, accuracy=0.1))%>%
  mutate(percentage.obs=scales::number(percentage.obs, accuracy=0.1))

studies$percentage.est<-as.numeric(studies$percentage.est)
studies$percentage.obs<-as.numeric(studies$percentage.obs)

lolli<- studies%>% 
  arrange(percentage.est) %>% 
  mutate(Country=forcats::fct_inorder(Country))%>%
  ggplot()+
  geom_segment( aes(x=Country, xend=Country, y=percentage.est, yend=percentage.obs), color="grey") +
  geom_point(aes(x=Country, y=percentage.est,color="percentage.est"), size=2 ) +
  geom_point(aes(x=Country, y=percentage.obs,color="percentage.obs"), size=2 ) +
  coord_flip()+
  theme_classic()+
  theme(legend.title = element_blank(),
        legend.position = c(0.65,0.2),
        plot.title =element_text(hjust=0.5, size=13),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size=7))+
  labs(y="Percentage", x="")+
  ggtitle("Countries")+
  scale_color_manual(values = c("#358DB9", "#E64B35B2"),
                    labels=c("% Number of studies", "% Number of observations"))+
  guides(fill=guide_legend(nrow =2, override.aes = list(size=4.5)),
         color=guide_legend(override.aes = list(size=4.5)))
  
 
lolli

##barplot
studies<-studies %>%
  pivot_longer(cols = starts_with("percen"), names_to = "percentage", values_to = "count")


studies<-studies%>%
  mutate(Country=factor(Country, levels=unique(Country)))

p.c<- studies %>% 
  arrange(percentage, (count)) %>% 
  mutate(Country=forcats::fct_inorder(Country))%>%
  ggplot(aes(x=count,y=Country,fill=percentage))+
  geom_bar(aes(x=count),stat="identity", position="dodge")+
  theme_classic()+
  xlim(0,20)+
  scale_x_continuous(expand = c(0,0)) +
  xlab("Percentage")+
  theme(legend.title = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size=8),
        legend.position = c(0.65,0.2),
        plot.title = element_text(hjust=0.5, size=13))+
  ggtitle("Countries")+
  guides(fill=guide_legend(nrow =2, override.aes = list(size=2)),
         color=guide_legend(override.aes = list(size=2)))+
  scale_color_manual(values = c("#E64B35B2","#358DB9"))+
  scale_fill_manual(values = c("#E64B35B2","#358DB9"),labels = c("% Number of studies", "% Number of observations"))
p.c 

##most studied families
fams.studied<- effect.size.total%>%
  group_by(Plant.species.family)%>%
  summarise(n.family=n())

fams.studied<-fams.studied%>%
  mutate(total.obs=sum(n.family),
         percentage.fam=(n.family/total.obs)*100)
        
fams.studied<- fams.studied%>%
  mutate(Plant.species.family = case_when(
    percentage.fam < 1.5 ~ "Others",
    TRUE~Plant.species.family))

fams.studied<-fams.studied%>%
  group_by(Plant.species.family)%>%
  summarise(cum.percen=sum(percentage.fam))%>%
  mutate(percentage.fam=scales::number(cum.percen, accuracy=0.1))

fams.studied$percentage.fam<- as.numeric(fams.studied$percentage.fam)

fams.studied<-fams.studied%>%
  arrange(percentage.fam)
  
fams.studied<-fams.studied%>%
  mutate(percentage=paste(fams.studied$percentage.fam, "%",sep=""))


library(ggsci)
library(ggthemes)
library(grDevices)
library(colorspace)
library(RColorBrewer)
library(ggpubr)

colors<-get_palette("npg",16)
format.donus<-list(theme(legend.text = element_text(size=9),
                           legend.position = "bottom",
                           legend.title = element_blank(),
                           legend.key.size = unit(0.2, "cm"),
                           plot.title = element_text(hjust=0.5)),
                   scale_fill_manual(values=colors),
                   theme_void(),
                   xlim(c(.2,2.5)))
                     

fams.studied$Plant.species.family<- factor(fams.studied$Plant.species.family, levels=rev(as.character(fams.studied$Plant.species.family)))

donus<- ggplot (fams.studied, aes(y=percentage.fam,x=2, fill=Plant.species.family))+
  geom_bar(stat="identity", width=1)+
  coord_polar(theta="y", start=200)+
  geom_text(aes(label=percentage),position=position_stack(vjust=0.5),
            col="white",size=2.7,fontface="bold")+
  ggtitle("Plant families (% obs)")+
  guides(fill=guide_legend(nrow =4, override.aes = list(size=5)),
         color=guide_legend(override.aes = list(size=4)))+
  format.donus
   
donus

ggsave(donus, filename="RData/figures/donus.png",
       width = 9, height = 6)


fig.3<-plot_grid(fig1, labels=c("A"), label_size = 14,  hjust = -5)
bottom.3<-plot_grid(p, NULL, donus, labels=c("A","B"), nrow=1,rel_widths =c(1,0.3,1),label_size = 14, hjust = -2)
figure.3<-plot_grid(fig.3, bottom.3, ncol=1,
                    scale = c(0.7,1,1))
ggsave(bottom.3, filename="RData/figures/multipanel.png",
       width = 9, height = 6)

###most studied biomes
unique(effect.size.total$Biome)
biomes <- effect.size.total%>%
  filter(!is.na(Biome))%>%
  group_by(Biome)%>%
  summarise(n.biome=n())

biomes.est<-biomes%>%
  mutate(total.obs=sum(n.biome),
         percentage.b=(n.biome/total.obs)*100,
         percentage.b=scales::number(percentage.b, accuracy=0.1))

biomes.est$percentage.b<- as.numeric(biomes.est$percentage.b)
biomes.est<-biomes.est%>%
  mutate(Biome=factor(Biome, levels=unique(Biome)))

format.bar<-list(scale_color_manual(values=c("#358DB9")),
             scale_fill_manual(values=c("#358DB9")),
             theme_classic(),
             theme(legend.title = element_blank(),
                   axis.title.y = element_blank(),
                   legend.text = element_blank(),
                   legend.position = "none",
                   plot.title = element_text(hjust=0.5)),
             scale_x_continuous(expand = c(0,0)),
             xlab("Percentage (obs)"))

p.b<- biomes.est %>% 
  arrange(c(percentage.b)) %>% 
  mutate(Biome=forcats::fct_inorder(Biome))%>%
  ggplot(aes(x=percentage.b,y=Biome, fill="#358DB9"))+
  geom_bar(aes(x=percentage.b),stat="identity")+
  xlim(c(0,30)) +
  ggtitle("Biomes")+
  format.bar
p.b

##most studied habitats
land <- effect.size.total%>%
  filter(!is.na(Landscape)& !Landscape %in% c("Experimental"))%>%
  group_by(Landscape)%>%
  summarise(n.landscape=n())

land.est<-land%>%
  mutate(total.obs=sum(n.landscape),
         percentage.l=(n.landscape/total.obs)*100,
         percentage.l=scales::number(percentage.l, accuracy=0.1))
 
land.est$percentage.l<- as.numeric(land.est$percentage.l)
land.est<-land.est%>%
  mutate(Landscape=factor(Landscape, levels=unique(Landscape)))

p.l<- land.est %>% 
  arrange(c(percentage.l)) %>% 
  mutate(Landscape=forcats::fct_inorder(Landscape))%>%
  ggplot(aes(x=percentage.l,y=Landscape, fill="#358DB9"))+
  geom_bar(stat="identity")+
  xlim(c(0,40))+
  ggtitle("Landscapes")+
  format.bar
  p.l

##STUDY TYPE
mesoc<- effect.size.total%>%
  mutate(Obs.Exp = case_when(
    Landscape== "Experimental" ~ "Mesocosms",
    TRUE~ Obs.Exp))

mesoc$Obs.Exp<- recode(mesoc$Obs.Exp, 
                       "Exp"="Exclusions",
                       "Obs"="Observational")

study.type<- mesoc%>%
  filter(!is.na(Obs.Exp))%>%
  group_by(Obs.Exp)%>%
  summarise(n.type=n())

study.type<-study.type%>%
  mutate(total.obs=sum(n.type),
         percentage.type=(n.type/total.obs)*100,
         percentage.type=scales::number(percentage.type, accuracy=0.1))

study.type$percentage.type<- as.numeric(study.type$percentage.type)
study.type<-study.type%>%
  arrange(percentage.type)%>%
  mutate(percentage=paste(percentage.type, "%",sep=""))

colors<-get_palette("Dark2",3)
format.donus<-list(theme(legend.text = element_text(size=9),
                         legend.position = "bottom",
                         legend.title = element_blank(),
                         legend.key.size = unit(0.2, "cm"),
                         plot.title = element_text(hjust=0.5)),
                   scale_fill_manual(values=colors),
                   theme_void(),
                   xlim(c(.2,2.5)))

study.type$Obs.Exp<- factor(study.type$Obs.Exp, levels=rev(as.character(study.type$Obs.Exp)))

donus.type<- ggplot (study.type, aes(y=percentage.type,x=2, fill=Obs.Exp))+
  geom_bar(stat="identity", width=1)+
  coord_polar(theta="y", start=200)+
  geom_text(aes(label=percentage),position=position_stack(vjust=0.5),
            col="white",size=3, fontface="bold")+
  ggtitle("Study type (% obs)")+
  guides(fill=guide_legend(nrow =1, override.aes = list(size=5)),
         color=guide_legend(override.aes = list(size=4)))+
  format.donus

donus.type
##
##fig number of studies per year
f1<-effect.size.total%>%
  group_by(Year)%>%
  summarise(numberstudies=n_distinct(Title))

p<-ggplot(data=f1,aes(x=Year, y=numberstudies))+
  geom_bar(stat="identity", fill="#358DB9")+
  ylim(0,20)+ ggtitle("Publication year")+
  labs(y="Number of studies")+theme_classic()+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text=element_text(size=8),
        axis.title=element_text(size=9))+
  scale_y_continuous(expand = c(0,0)) 
p

ggsave(p, filename="RData/figures/fig.s2.png",
       width = 7.5, height = 5)  

##multipanel figure
cowbc<- plot_grid(p,NULL,p.l, labels=c("B","","C"),
                  rel_heights = c(1, 0.01, 1),
                  ncol=1,hjust=-3, label_size=14)
cowa <- plot_grid(NULL, lolli,
                   rel_heights = c(0.1,6.6),
                   ncol = 1, hjust=-3,
                   labels = c("","A"),
                   label_size = 14)

multi<-plot_grid(cowa, cowbc, rel_widths = c(2.6,3),
                 rel_heights = c(6,6),nrow = 1)
cowd<- plot_grid(NULL,p.b, labels = c("","D"), rel_widths = c(0.07, 0.85),
                 rel_heights = c(-0.01,1.3),
                 ncol=1, hjust=-3, label_size = 14)
donus.multi <- plot_grid(donus, donus.type, labels=c("E","F"), 
                         rel_heights = c(4.5,4), 
                         rel_widths = c(4, 3.5),
                         ncol=2,label_size=14, hjust=-3)


multipanel.fig<- plot_grid(multi,cowd,donus.multi, ncol=1,
                           rel_heights = c(5.2,3,6.5), scale=c(1,0.8,1))
cowplot::save_plot("Figs/multipanelfigure.new.png", multipanel.fig, base_height = 13.2, base_width = 9)

####NULL MODELS
m0<- rma.mv(yi,vi, random= ~1|Title, data=effect.size.total, method="ML")
m1<- rma.mv(yi,vi, random= ~1|Country, data=effect.size.total,method="ML")
m2<- rma.mv(yi,vi, random= ~1|Year, data=effect.size.total,method="ML")
m3<- rma.mv(yi,vi, random=  list(~ 1 | Year, ~ 1 | Title), data=effect.size.total,method="ML")
m4<- rma.mv(yi,vi, random=  list(~ 1 | Year, ~ 1 | Title, ~1|Country), data=effect.size.total,method="ML")
m5<- rma.mv(yi,vi, random=  list( ~ 1 | Title, ~1|Country), data=effect.size.total,method="ML")
m6<- rma.mv(yi,vi, random= ~1|Title/Year, data=effect.size.total,method="ML")
m7<- rma.mv(yi,vi, random= ~1|Year/Country, data=effect.size.total,method="ML")
m8<- rma.mv(yi,vi, random= ~1|Country/Title, data=effect.size.total,method="ML")
m9<- rma.mv(yi,vi, random= ~1|Country/Year, data=effect.size.total,method="ML")


AIC(m0,m1,m2,m3,m4,m5,m6,m7,m8,m9)
##m3

# The best null model is fitted with REML
m3.reml<- rma.mv(yi,vi, random=  list(~ 1 | Year, ~ 1 | Title), data=effect.size.total,method="REML")
summary(m3.reml)
##I2
i2_ml(m3.reml)
##other way: 
W <- diag(1/m3.reml$vi)
X <- model.matrix(m3.reml)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100 * sum(m3.reml$sigma2) / (sum(m3.reml$sigma2) + (m3.reml$k-m3.reml$p)/sum(diag(P)))

library(orchaRd)
m3.reml.tabla <- orchaRd::mod_results(m3.reml, mod = "1", group="Title") # extracting table of results

effect.size.total$Reproductive.succes.measure<-recode(effect.size.total$Reproductive.succes.measure,
                                                      "Yield"="yield")

#######ORCHARD PLOT
install.packages("latex2exp")
library(latex2exp)
####OVERALL EFFECT PLOT
resnul <- orchaRd::mod_results(m3.reml, group="Title")

k <- dim(effect.size.total)[1]
stdy <- effect.size.total %>% summarise(stdy = length(unique(Title)))
anno <- annotate("text", x = 1.2, y = 2.7, size=5.5,label = TeX(paste0("\\textit{k} = ", k, " (", stdy, ")")))

format.orchard<-list(theme(legend.title = element_text(size =13),
                           legend.text = element_text(size = 13),
                           text=element_text(size=13),
                           axis.text.y = element_text(size =13),
                           plot.title = element_text(lineheight=.8),
                           axis.text.x=element_text(size=18),
                           axis.title=element_text(size=18)),
                     scale_y_continuous(limits = c(-8.8,5)))

fig3a<-orchaRd::orchard_plot(resnul, xlab = "Hedges´g",
                            angle = 45, transfm = "none", alpha = 0.5, k=FALSE, g=FALSE,branch.size=10,trunk.size = 11)+
  scale_fill_manual(values = c("#00A087B2"))+
  scale_color_manual(values = c("#00A087B2"))+
  scale_x_discrete(labels=c("Overall"))+
  annotate(geom = "text", x = 0.6, y = -4.4, 
           color = "black", size = 5.5, label=TeX(paste0("95% CI = ", round(resnul$mod_table[1,3], 3), " to ", round(resnul$mod_table[1,4], 3))))+
  anno+
  format.orchard

###CROPS AND WILD PLANTS
# We first fit the model with moderator with ML to compare it to the null model (AIC and likelihood ratio test)
# We fit the model without intercept to get estimated effect (slope) for each level of a categorical moderator
model<- rma.mv(yi,vi,mods=~Cultivo-1,random= list(~ 1 | Year, ~ 1 | Title), data=effect.size.total, method="ML")
anova(model,m3)
# We fit the model with REML to get estimates
model.reml<- rma.mv(yi,vi,mods=~Cultivo-1,random= list(~ 1 | Year, ~ 1 | Title), data=effect.size.total, method="REML")
summary(model.reml)
# We calculate marginal R^2 of model with moderator
r2_ml(model.reml)
modelsummary::modelsummary(model.reml)

# We now fit the same model with the intercept to calculate the heterogeneity explained by the levels of the moderator
model.reml.intercept <- rma.mv(yi,vi,mods=~Cultivo,random= list(~ 1 | Year, ~ 1 | Title), data=effect.size.total, method="REML")
summary(model.reml.intercept)

##estimates
crop.estimates<- mod_results(model.reml, mod ="Cultivo", group="Title")

tabla.crop <- flextable::flextable(mod_results(model.reml, group = "Title", mod = "Cultivo")$mod_table)

tabla.crop <- flextable::color(tabla.crop, part = "footer", color = "#666666")
tabla.crop<- flextable::set_caption(tabla.crop, caption = "Effect of pollinator diversity loss")
tabla.crop

crop<- orchaRd::mod_results(model.reml, mod = "Cultivo",
                            group = "Title",subset = TRUE,  weights = "prop")


cr<- effect.size.total%>%
  filter(Cultivo=="Crop")
k.1 <- dim(cr)[1]
stdy.1 <- cr %>% summarise(stdy = length(unique(Title)))
anno.1 <- annotate("text", x = 1.2, y = 2.7, size=5.5,label = TeX(paste0("\\textit{k} = ", k.1, " (", stdy.1, ")")))
no_cr<- effect.size.total%>%
  filter(Cultivo=="No_crop")
k.2 <- dim(no_cr)[1]
stdy.2 <- no_cr%>% summarise(stdy = length(unique(Title)))
anno.2 <- annotate("text", x = 2.2, y = 2.7, size=5.5,label = TeX(paste0("\\textit{k} = ", k.2, " (", stdy.2, ")")))



fig3b<-orchaRd::orchard_plot(crop, xlab = "Hedges´g", alpha=0.65,angle = 45, g = FALSE, k=F, branch.size=10,trunk.size = 11,
                             condition.lab = "Cultivo") +
  scale_fill_manual( values = c("#E64B35B2","#4DBBD5B2"))+
  scale_color_manual(values = c("#E64B35B2","#4DBBD5B2"))+
  scale_x_discrete(labels=c("Crops","Wild Plants"))+
  annotate(geom = "text", x = 2.15, y = -5.25, 
           color = "black", size = 5, label=TeX(paste0("95% CI = ", round(crop$mod_table[2,3], 3), " to ", round(crop$mod_table[2,4], 3))))+
  annotate(geom = "text", x = 1.15, y = -5.25, 
           color = "black", size = 5, label=TeX(paste0("95% CI = ", round(crop$mod_table[1,3], 3), " to ", round(crop$mod_table[1,4], 3))))+
  anno.1+anno.2+format.orchard

fig3b

ggsave(fig3b, filename="Figs/crops.nw.png",
       width = 9, height = 6)

fig3<-plot_grid(fig3a,fig3b,labels=c("A","B"), label_size = 14, hjust=-4)
ggsave(fig3, filename="Figs/Figure3.nw.png",
       width = 12, height = 4)

###ORCHARD PLOT FOR MAIN REPRODUCTIVE SUCCESS MEASURES
##We first fit the model with moderator with ML to compare it to the null model (AIC and likelihood ratio test)
# We fit the model without intercept to get estimated effect (slope) for each level of a categorical moderator
m3<- rma.mv(yi,vi, random=  list(~ 1 | Year, ~ 1 | Title), data=effect.size.total,method="ML")
rep.su <- rma.mv(yi,vi,mods=~Reproductive.succes.measure-1,random= list(~ 1 | Year, ~ 1 | Title), data=effect.size.total, method="ML")
anova(rep.su, m3)
# We fit the model with REML to get estimates
rep.reml <- rma.mv(yi,vi,mods=~Reproductive.succes.measure-1,random= list(~ 1 | Year, ~ 1 | Title), data=effect.size.total, method="REML")
summary(rep.reml)
# We calculate marginal R^2 of model with moderator
r2_ml(rep.reml)

# We now fit the same model with the intercept to calculate the heterogeneity explained by the levels of the moderator
rep.reml.intercept <- rma.mv(yi,vi,mods=~Reproductive.succes.measure,random= list(~ 1 | Year, ~ 1 | Title), data=effect.size.total, method="REML")
summary(rep.reml.intercept)

#table
tabla.measures <- flextable::flextable(mod_results(rep.reml, group = "Title", mod = "Reproductive.succes.measure", 
                                                   at = list(Reproductive.succes.measure = c("fruit set", "seed set", "fruit weight")), subset = TRUE)$mod_table)

tabla.measures <- flextable::color(tabla.measures, part = "footer", color = "#666666")
tabla.measures<- flextable::set_caption(tabla.measures, caption = "Effect of pollinator diversity loss")
tabla.measures


res <- orchaRd::mod_results(rep.reml, mod = "Reproductive.succes.measure", group = "Title",
                            at = list(Reproductive.succes.measure = c("fruit set", "seed set", "fruit weight")), subset = TRUE)

pred.m0.fw<- effect.size.total%>%
  filter(Reproductive.succes.measure=="fruit weight")
k.1 <- dim(pred.m0.fw)[1]
stdy.1 <- pred.m0.fw %>% summarise(stdy = length(unique(Title)))
anno.1 <- annotate("text", x = 3.2, y = 2.2, size=5.5, label = TeX(paste0("\\textit{k} = ", k.1, " (", stdy.1, ")")))

pred.m0.ss<- effect.size.total%>%
  filter(Reproductive.succes.measure=="seed set")
k.2<- dim(pred.m0.ss)[1]
stdy.2 <- pred.m0.ss %>% summarise(stdy = length(unique(Title)))
anno.2 <- annotate("text", x = 2.2, y = 2.2, size=5.5, label = TeX(paste0("\\textit{k} = ", k.2, " (", stdy.2, ")")))

pred.m0.fs<- effect.size.total%>%
  filter(Reproductive.succes.measure=="fruit set")
k.3 <- dim(pred.m0.fs)[1]
stdy.3 <- pred.m0.fs %>% summarise(stdy = length(unique(Title)))
anno.3 <- annotate("text", x = 1.2, y = 2.2, size=5.5, label = TeX(paste0("\\textit{k} = ", k.3, " (", stdy.3, ")")))

#pred.m0.fy<- effect.size.total%>%
#filter(Reproductive.succes.measure=="yield")
#k.4 <- dim(pred.m0.fy)[1]
#stdy.4 <- pred.m0.fy %>% summarise(stdy = length(unique(Title)))
#anno.4 <- annotate("text", x = 4.2, y = 2.2, size=5.5, label = TeX(paste0("\\textit{k} = ", k.4, " (", stdy.4, ")")))

effect.size.total$yi<-as.numeric(effect.size.total$yi)
fig4<-orchaRd::orchard_plot(res, xlab = "Hedges´g",
                            angle = 45, transfm = "none", alpha = 0.5, k=F, g=F,branch.size=10,trunk.size = 11)+
  theme(legend.title = element_text(size =13),
        legend.text = element_text(size = 13),
        text=element_text(size=16),
        axis.text.y = element_text(size =16),
        axis.text.x=element_text(size=18),
        axis.title=element_text(size=18))+
  scale_fill_manual("Reproductive success measure", values = c("#E64B35B2","#4DBBD5B2","#00A087B2"))+
  scale_color_manual("Reproductive success measure", values = c("#E64B35B2","#4DBBD5B2","#00A087B2"))+
  scale_y_continuous(limits = c(-8.8,5))+
  annotate(geom = "text", x = 3.2, y = -6, 
           color = "black", size = 5, label=TeX(paste0("95% CI = ", round(res$mod_table[3,3], 3), " to ", round(res$mod_table[3,4], 3))))+
  annotate(geom = "text", x = 2.2, y = -6, 
           color = "black", size = 5, label=TeX(paste0("95% CI = ", round(res$mod_table[2,3], 3), " to ", round(res$mod_table[2,4], 3))))+
  annotate(geom = "text", x = 1.2, y = -6, 
           color = "black", size = 5, label=TeX(paste0("95% CI = ", round(res$mod_table[1,3], 3), " to ", round(res$mod_table[1,4], 3))))+
  
  anno.1+anno.2+anno.3

###figure4
figure4<-plot_grid(fig4, plot.comp,labels=c("A","B"),label_size = 18, hjust=-4)
ggsave(figure4, filename="Figs/Fig5.png",
       width = 17, height = 6)

##Fruit weight info:
frutos<-effect.size.total%>%
  filter(Reproductive.succes.measure %in% c("fruit set","fruit weight"))

f <-frutos%>%
  group_by(Title, Plant.species)%>%
  mutate(medidas.repro= n_distinct(Reproductive.succes.measure))

datos.fw.fs <- f%>%
  select(Title,medidas.repro, Reproductive.succes.measure, yi, Plant.species)%>%
  filter(medidas.repro=="2")%>%
  group_by(Title, Plant.species, Reproductive.succes.measure)%>%
  summarise(mean=mean(yi))
####

###Conditioned orchard plot diurnal or nocturnal
d.n.orchard<-effect.size.total[!is.na(effect.size.total$Diurn_Noctur), ]
d.n.orchard%>%
  group_by(Diurn_Noctur)%>%
  summarise(N=n_distinct(Title))

m3.nd <- rma.mv(yi, vi,random= list(~ 1 | Year, ~ 1 | Title),
               data=d.n.orchard, method="ML")
full.nd <- rma.mv(yi, vi, mods = ~ Diurn_Noctur-1,random= list(~ 1 | Year, ~ 1 | Title),
               data=d.n.orchard, method="ML")
anova(m3.nd,full.nd)

##reml
nd.reml <- rma.mv(yi, vi, mods = ~ Diurn_Noctur-1,random= list(~ 1 | Year, ~ 1 | Title),
                  data=d.n.orchard, method="REML")

r2_ml(nd.reml)

##estimates
res_dn<- mod_results(nd.reml, mod ="Diurn_Noctur", group="Diurn_Noctur")
tabla.nd<-res_dn$mod_table


##orchard model
full.nd.reml <- rma.mv(yi, vi, mods = ~ Diurn_Noctur+Reproductive.succes.measure,random= list(~ 1 | Year, ~ 1 | Title),
                       data=d.n.orchard, method="REML")

HetModel.dn<- orchaRd::mod_results(full.nd.reml, mod = "Reproductive.succes.measure", group = "Title",
                            at = list(Reproductive.succes.measure = c("fruit set", "seed set", "fruit weight")), subset = TRUE, by="Diurn_Noctur", weights = "prop")


fw<- d.n.orchard%>%
  filter(Reproductive.succes.measure=="fruit weight")
k.1<- dim(fw)[1]
stdy.1 <- fw %>% summarise(stdy = length(unique(Title)))
anno.1 <- annotate("text", x = 3.2, y = 2.5, size=6.5,label = TeX(paste0("\\textit{k} = ", k.1, " (", stdy.1, ")")))


ss<- d.n.orchard%>%
  filter(Reproductive.succes.measure=="seed set")
k.2 <- dim(ss)[1]
stdy.2 <- ss%>% summarise(stdy = length(unique(Title)))
anno.2 <- annotate("text", x = 2.2, y = 2.5, size=6.5,label = TeX(paste0("\\textit{k} = ", k.2, " (", stdy.2, ")")))


fs<- d.n.orchard%>%
  filter(Reproductive.succes.measure=="fruit set")
k.3 <- dim(fs)[1]
stdy.3 <- fs %>% summarise(stdy = length(unique(Title)))
anno.3 <- annotate("text", x = 1.2, y = 2.5, size=6.5,label = TeX(paste0("\\textit{k} = ", k.3, " (", stdy.3, ")")))

sun<-"images/sun.png"
moon<-"images/moon.png"

plot.dn<-orchaRd::orchard_plot(HetModel.dn, xlab = "Hedges´g", angle = 45, alpha=0.7,g = FALSE, k=F, branch.size=10,trunk.size = 11,legend.pos = "top.left",
                      condition.lab = "Diurn_Noctur") + theme(legend.direction = "vertical")+
  theme(legend.title = element_text(size =20),
        legend.text = element_text(size = 20),
        text=element_text(size=20),
        axis.text.y = element_text(size =20),
        plot.title = element_text(lineheight=.8),
        axis.text.x=element_text(size=20),
        axis.title=element_text(size=20))+
  scale_y_continuous(limits = c(-8.8,5))+
  scale_fill_manual("Reproductive success measure", values = c("#E64B35B2","#4DBBD5B2","#00A087B2"))+
  scale_color_manual("Reproductive success measure", values = c("#E64B35B2","#4DBBD5B2","#00A087B2"))+
 anno.1+anno.2+anno.3

install.packages("magick")
library(magick)
plot.dn<-ggdraw() + 
  draw_plot(plot.dn) +
  draw_image(image=sun, x = -0.075, y =  0.3, scale = 0.07)+
  draw_image(image=sun, x = -0.075, y =  0.025, scale = 0.07)+
  draw_image(image=sun, x = -0.075, y =  -0.27, scale = 0.07)+
  draw_image(image=moon, x = -0.075, y =  0.38, scale = 0.06)+
  draw_image(image=moon, x = -0.075, y =  0.1, scale = 0.06)+
  draw_image(image=moon, x = -0.075, y =  -0.19, scale = 0.06)
    
ggsave(plot.dn, filename="RData/figures/ES.dn.2.png",
       width = 8.5, height = 6)


###MANAGED WILD
m.w.orchard<-effect.size.total[!is.na(effect.size.total$Managed_Wild), ]
m.w.orchard%>%
  group_by(Managed_Wild)%>%
  summarise(N=n())

m3.mw <- rma.mv(yi, vi,random= list(~ 1 | Year, ~ 1 | Title),
                data=m.w.orchard, method="ML")
full.mw <- rma.mv(yi, vi, mods = ~ Managed_Wild-1,random= list(~ 1 | Year, ~ 1 | Title),
                  data=m.w.orchard, method="ML")
anova(m3.mw,full.mw)

##reml
mw.reml <- rma.mv(yi, vi, mods = ~ Managed_Wild-1,random= list(~ 1 | Year, ~ 1 | Title),
                       data=m.w.orchard, method="REML")
r2_ml(mw.reml)

#estimates
res_mw<- mod_results(mw.reml, mod ="Managed_Wild", group="Managed_Wild")
tabla.mw<-res_mw$mod_table

##orchard model
full.mw.reml <- rma.mv(yi, vi, mods = ~ Managed_Wild+Reproductive.succes.measure,random= list(~ 1 | Year, ~ 1 | Title),
                       data=m.w.orchard, method="REML")

HetModel.mw<- orchaRd::mod_results(full.mw.reml, mod = "Reproductive.succes.measure", group = "Title",
                                   at = list(Reproductive.succes.measure = c("fruit set", "seed set", "fruit weight")), subset = TRUE, by="Managed_Wild", weights = "prop")



fw<- m.w.orchard%>%
  filter(Reproductive.succes.measure=="fruit weight")
k.1<- dim(fw)[1]
stdy.1 <- fw %>% summarise(stdy = length(unique(Title)))
anno.1 <- annotate("text", x = 3.2, y = 2.5, size=6.5,label = TeX(paste0("\\textit{k} = ", k.1, " (", stdy.1, ")")))


ss<- m.w.orchard%>%
  filter(Reproductive.succes.measure=="seed set")
k.2 <- dim(ss)[1]
stdy.2 <- ss%>% summarise(stdy = length(unique(Title)))
anno.2 <- annotate("text", x = 2.2, y = 2.5, size=6.5,label = TeX(paste0("\\textit{k} = ", k.2, " (", stdy.2, ")")))


fs<- m.w.orchard%>%
  filter(Reproductive.succes.measure=="fruit set")
k.3 <- dim(fs)[1]
stdy.3 <- fs %>% summarise(stdy = length(unique(Title)))
anno.3 <- annotate("text", x = 1.2, y = 2.5, size=6.5,label = TeX(paste0("\\textit{k} = ", k.3, " (", stdy.3, ")")))

bee<-"images/bee.png"
beehive<-"images/beehive.png"

plot.mw.1<-orchaRd::orchard_plot(HetModel.mw, xlab = "Hedges´g", alpha=0.7, angle = 45, g = FALSE, k=F, branch.size=10,trunk.size = 11,legend.pos = "top.left",
                            condition.lab = "Managed_Wild") +theme(legend.direction = "vertical")+
  theme(legend.title = element_text(size =15),
        legend.text = element_text(size = 15),
        text=element_text(size=20),
        axis.text.y = element_text(size =20),
        plot.title = element_text(lineheight=.8),
        axis.text.x=element_text(size=20),
        axis.title=element_text(size=20))+
  scale_y_continuous(limits = c(-8.8,5))+
  scale_fill_manual("Reproductive success measure", values = c("#E64B35B2","#4DBBD5B2","#00A087B2"))+
  scale_color_manual("Reproductive success measure", values = c("#E64B35B2","#4DBBD5B2","#00A087B2"))+
anno.1+anno.2+anno.3

plot.mw<-ggdraw() + 
  draw_plot(plot.mw.1) +
  draw_image(image=beehive, x = -0.075, y =  0.29, scale = 0.07)+
  draw_image(image=beehive, x = -0.075, y =  0.01, scale = 0.07)+
  draw_image(image=beehive, x = -0.075, y =  -0.26, scale = 0.07)+
  draw_image(image=bee, x = -0.075, y =  0.36, scale = 0.08)+
  draw_image(image=bee, x = -0.075, y =  0.08, scale = 0.08)+
  draw_image(image=bee, x = -0.075, y =  -0.185, scale = 0.08)

ggsave(plot.mw, filename="RData/figures/ES.mw.2.png",
       width = 8.5, height = 6)


####VERT_INVERT
v.i.orchard<-effect.size.total[!is.na(effect.size.total$Vert_Invert), ]
v.i.orchard%>%
  group_by(Vert_Invert)%>%
  summarise(N=n())

m3.vi <- rma.mv(yi, vi,random= list(~ 1 | Year, ~ 1 | Title),
                data=v.i.orchard, method="ML")
full.vi <- rma.mv(yi, vi, mods = ~ Vert_Invert-1,random= list(~ 1 | Year, ~ 1 | Title),
                  data=v.i.orchard, method="ML")
anova(m3.vi,full.vi)

##reml
full.vi.reml <- rma.mv(yi, vi, mods = ~ Vert_Invert-1,random= list(~ 1 | Year, ~ 1 | Title),
                       data=v.i.orchard, method="REML")
r2_ml(full.vi.reml)
summary(full.vi.reml)

##estimates
res_vi <- mod_results(full.vi.reml, mod ="Vert_Invert", group="Title")
tabla.vi<-res_vi$mod_table

ft <- flextable::flextable(mod_results(full.vi.reml, group = "Title", mod = "Vert_Invert")$mod_table)

ft <- flextable::color(ft, part = "footer", color = "#666666")
ft <- flextable::set_caption(ft, caption = "Effect of different lost pollinators")
ft


##orchard model
full.vi.reml <- rma.mv(yi, vi, mods = ~ Vert_Invert+Reproductive.succes.measure,random= list(~ 1 | Year, ~ 1 | Title),
                       data=v.i.orchard, method="REML")

HetModel.vi<- orchaRd::mod_results(full.vi.reml, mod = "Reproductive.succes.measure", group = "Title",
                                   at = list(Reproductive.succes.measure = c("fruit set", "seed set", "fruit weight")), subset = TRUE, by="Vert_Invert", weights = "prop")


fw<- v.i.orchard%>%
  filter(Reproductive.succes.measure=="fruit weight")
k.1<- dim(fw)[1]
stdy.1 <- fw %>% summarise(stdy = length(unique(Title)))
anno.1 <- annotate("text", x = 3.2, y = 2.5, size=6.5,label = TeX(paste0("\\textit{k} = ", k.1, " (", stdy.1, ")")))


ss<- v.i.orchard%>%
  filter(Reproductive.succes.measure=="seed set")
k.2 <- dim(ss)[1]
stdy.2 <- ss%>% summarise(stdy = length(unique(Title)))
anno.2 <- annotate("text", x = 2.2, y = 2.5, size=6.5,label = TeX(paste0("\\textit{k} = ", k.2, " (", stdy.2, ")")))


fs<- v.i.orchard%>%
  filter(Reproductive.succes.measure=="fruit set")
k.3 <- dim(fs)[1]
stdy.3 <- fs %>% summarise(stdy = length(unique(Title)))
anno.3 <- annotate("text", x = 1.2, y = 2.5, size=6.5,label = TeX(paste0("\\textit{k} = ", k.3, " (", stdy.3, ")")))

hb<-"images/hummingbird.png"

plot.vi.1<-orchaRd::orchard_plot(HetModel.vi, xlab = "Hedges´g", alpha=0.7,angle = 45, g = FALSE, k=F, branch.size=10,trunk.size = 11,legend.pos = "top.left",
                            condition.lab = "Vert_Invert")+theme(legend.direction = "vertical")+
  theme(legend.title = element_text(size =15),
        legend.text = element_text(size = 15),
        text=element_text(size=20),
        axis.text.y = element_text(size =20),
        plot.title = element_text(lineheight=.8),
        axis.text.x=element_text(size=20),
        axis.title=element_text(size=20))+
  scale_y_continuous(limits = c(-8.8,5))+
  scale_fill_manual("Reproductive success measure", values = c("#E64B35B2","#4DBBD5B2","#00A087B2"))+
  scale_color_manual("Reproductive success measure", values = c("#E64B35B2","#4DBBD5B2","#00A087B2"))+
 anno.1+anno.2+anno.3

plot.vi<-ggdraw() + 
  draw_plot(plot.vi.1) +
  draw_image(image=bee, x = -0.075, y =  0.29, scale = 0.1)+
  draw_image(image=bee, x = -0.075, y =  0.01, scale = 0.1)+
  draw_image(image=bee, x = -0.075, y =  -0.255, scale = 0.1)+
  draw_image(image=hb, x = -0.075, y =  0.36, scale = 0.1)+
  draw_image(image=hb, x = -0.075, y =  0.1, scale = 0.1)+
  draw_image(image=hb, x = -0.075, y =  -0.175, scale = 0.1)

ggsave(plot.vi, filename="RData/figures/ES.vi.2.png",
       width = 8.5, height = 6)

##cowplot dn,mw,vi 
fig.6<-plot_grid(plot.vi, labels=c("A"), label_size = 20,  hjust = -5)
bottom.6<-plot_grid(plot.dn,plot.mw, labels=c("B","C"),label_size = 20,  hjust = -4)
figure.6<-plot_grid(fig.6, bottom.6, ncol=1,
                    scale = c(0.8,1,1))
ggsave(figure.6, filename="Figs/figure.6.png",
       width = 19, height = 13)

####LANDSCAPE
##as there are some NAs:
l.orchard<-effect.size.total[!is.na(effect.size.total$Landscape), ]
table(l.orchard$Landscape)
l.orchard<- l.orchard%>%
  subset(!(Landscape=="Experimental"))

land <- rma.mv(yi, vi, mods = ~ Landscape-1,random= list(~ 1 | Year, ~ 1 | Title),
                data=l.orchard, method="ML")
m3.land <- rma.mv(yi, vi,random= list(~ 1 | Year, ~ 1 | Title),
               data=l.orchard, method="ML")
anova(land,m3.land)

##REML model
model.land<- rma.mv(yi,vi,mods=~Landscape-1,random= list(~ 1 | Year, ~ 1 | Title), data=l.orchard, method="REML")
r2_ml(model.land)

l.orchard.mang <- l.orchard%>%
  filter(Reproductive.succes.measure %in% c("fruit weight","seed set","fruit set"))%>%
  subset(!(Landscape=="Mangrove"))

unique(l.orchard.mang$Landscape)

model.land.rep<- rma.mv(yi,vi,mods=~Reproductive.succes.measure+Landscape-1,random= list(~ 1 | Year, ~ 1 | Title), data=l.orchard.mang, method="REML")

HetModel.1 <- orchaRd::mod_results(model.land.rep, mod = "Landscape",
                                   group = "Title",subset = TRUE, by="Reproductive.succes.measure", weights = "prop")

fw<- l.orchard%>%
  filter(Landscape=="Agricultural")
k.1<- dim(fw)[1]
stdy.1 <- fw %>% summarise(stdy = length(unique(Title)))
anno.1 <- annotate("text", x = 1.2, y = 3, size=4.5,label = TeX(paste0("\\textit{k} = ", k.1, " (", stdy.1, ")")))


ss<- l.orchard%>%
  filter(Landscape=="Dune")
k.2 <- dim(ss)[1]
stdy.2 <- ss%>% summarise(stdy = length(unique(Title)))
anno.2 <- annotate("text", x = 2.2, y = 3, size=4.5,label = TeX(paste0("\\textit{k} = ", k.2, " (", stdy.2, ")")))


fs<- l.orchard%>%
  filter(Landscape=="Forest")
k.3 <- dim(fs)[1]
stdy.3 <- fs %>% summarise(stdy = length(unique(Title)))
anno.3 <- annotate("text", x = 3.2, y = 3, size=4.5,label = TeX(paste0("\\textit{k} = ", k.3, " (", stdy.3, ")")))

fg<- l.orchard%>%
  filter(Landscape=="Grassland")
k.4 <- dim(fg)[1]
stdy.4 <- fg %>% summarise(stdy = length(unique(Title)))
anno.4 <- annotate("text", x = 4.2, y = 3, size=4.5,label = TeX(paste0("\\textit{k} = ", k.4, " (", stdy.4, ")")))

fm<- l.orchard%>%
  filter(Landscape=="Marsh")
k.5 <- dim(fm)[1]
stdy.5 <- fm %>% summarise(stdy = length(unique(Title)))
anno.5 <- annotate("text", x = 5.2, y = 3, size=4.5,label = TeX(paste0("\\textit{k} = ", k.5, " (", stdy.5, ")")))

fsa<- l.orchard%>%
  filter(Landscape=="Savanna")
k.6 <- dim(fsa)[1]
stdy.6 <- fsa %>% summarise(stdy = length(unique(Title)))
anno.6 <- annotate("text", x = 6.2, y = 3, size=4.5,label = TeX(paste0("\\textit{k} = ", k.6, " (", stdy.6, ")")))

fsu<- l.orchard%>%
  filter(Landscape=="Shrubland")
k.7 <- dim(fsu)[1]
stdy.7 <- fsu %>% summarise(stdy = length(unique(Title)))
anno.7 <- annotate("text", x = 7.2, y = 3, size=4.5,label = TeX(paste0("\\textit{k} = ", k.7, " (", stdy.7, ")")))

fsur<- l.orchard%>%
  filter(Landscape=="Urban")
k.8 <- dim(fsur)[1]
stdy.8 <- fsur %>% summarise(stdy = length(unique(Title)))
anno.8 <- annotate("text", x = 8.2, y = 3, size=4.5,label = TeX(paste0("\\textit{k} = ", k.8, " (", stdy.8, ")")))


plot<-orchaRd::orchard_plot(HetModel.1, xlab = "Hedges´g", alpha=0.65,angle = 45, g = FALSE, k=F, branch.size=1.5,trunk.size = 4.5,legend.pos = "top.left",
                            condition.lab = "Reproductive.succes.measure") + theme(legend.direction = "vertical")+
  theme(legend.title = element_text(size =13),
        legend.text = element_text(size = 13),
        text=element_text(size=13),
        axis.text.y = element_text(size =13),
        plot.title = element_text(lineheight=.8),
        axis.text.x=element_text(size=15),
        axis.title=element_text(size=15))+
  scale_y_continuous(limits = c(-8.8,5))+
  scale_x_discrete(labels=c("Agricultural","Dune","Forest","Grassland","Marsh","Savannah","Shrubland","Urban"))+
  scale_fill_manual("Reproductive success measure", values = c("#E64B35B2","#4DBBD5B2","#00A087B2","#FFB84D","#3A6589","#9B5672","#6A6599FF","#DB735C"))+
  scale_color_manual("Reproductive success measure", values = c("#E64B35B2","#4DBBD5B2","#00A087B2","#FFB84D","#3A6589","#9B5672","#6A6599FF","#DB735C"))+
  anno.1+anno.2+anno.3+anno.4+anno.5+anno.6+anno.7+anno.8


ggsave(plot, filename="Figs/habitat.S5.png",
       width = 7.5, height = 8)

############Clustered Biomes#####
biome <- rma.mv(yi, vi, mods = ~ Biomes-1,random= list(~ 1 | Year, ~ 1 | Title),
                     data=effect.size.total, method="ML")
anova(biome,m3)


##graph with clustered biomes
b.reml <- rma.mv(yi, vi, mods = ~ Biomes-1,random= list(~ 1 | Year, ~ 1 | Title),
                     data=effect.size.total, method="REML")
r2_ml(b.reml)
summary(b.reml)

b.orchard<-effect.size.total[!is.na(effect.size.total$Biomes), ]
table(b.orchard$Biomes)
b.orchard <- b.orchard%>%
  filter(Reproductive.succes.measure %in% c("fruit weight","seed set","fruit set"))

biome.reml <- rma.mv(yi, vi, mods = ~ Reproductive.succes.measure+Biomes,random= list(~ 1 | Year, ~ 1 | Title),
                     data=b.orchard, method="REML")

HetModel.1 <- orchaRd::mod_results(biome.reml, mod = "Biomes",
                                   group = "Title",subset = TRUE, by="Reproductive.succes.measure", 
                                   weights = "prop")

fd<- b.orchard%>%
  filter(Biomes=="Deserts")
k.1 <- dim(fd)[1]
stdy.1<- fd%>% summarise(stdy = length(unique(Title)))
anno.1 <- annotate("text", x = 1.2, y = 2.5, size=4.5,label = TeX(paste0("\\textit{k} = ", k.1, " (", stdy.1, ")")))

fmed<- b.orchard%>%
  filter(Biomes=="Mediterranean")
k.2 <- dim(fmed)[1]
stdy.2<- fmed%>% summarise(stdy = length(unique(Title)))
anno.2 <- annotate("text", x = 2.2, y = 2.5, size=4.5,label = TeX(paste0("\\textit{k} = ", k.2, " (", stdy.2, ")")))

fo<- b.orchard%>%
  filter(Biomes=="Others")
k.3 <- dim(fo)[1]
stdy.3<- fd%>% summarise(stdy = length(unique(Title)))
anno.3 <- annotate("text", x = 3.2, y = 2.5, size=4.5,label = TeX(paste0("\\textit{k} = ", k.3, " (", stdy.3, ")")))

ftem<- b.orchard%>%
  filter(Biomes=="Temperate")
k.4 <- dim(ftem)[1]
stdy.4<- ftem%>% summarise(stdy = length(unique(Title)))
anno.4 <- annotate("text", x = 4.2, y = 2.5, size=4.5,label = TeX(paste0("\\textit{k} = ", k.4, " (", stdy.4, ")")))

ftrop<- b.orchard%>%
  filter(Biomes=="Tropical")
k.5 <- dim(ftrop)[1]
stdy.5<- ftrop%>% summarise(stdy = length(unique(Title)))
anno.5 <- annotate("text", x = 5.2, y = 2.5, size=4.5,label = TeX(paste0("\\textit{k} = ", k.5, " (", stdy.5, ")")))


plot.biome<-orchaRd::orchard_plot(HetModel.1, xlab = "Hedges´g", alpha=0.65,angle = 45, g = FALSE, k=F, branch.size=1.5,trunk.size = 4.5,legend.pos = "top.left",
                            condition.lab = "Reproductive.succes.measure") + theme(legend.direction = "vertical")+
  theme(legend.title = element_text(size =11),
        legend.text = element_text(size = 11),
        axis.text.y = element_text(size =13),
        plot.title = element_text(lineheight=.8),
        axis.text.x=element_text(size=15),
        axis.title=element_text(size=15))+
  scale_y_continuous(limits = c(-8.8,5))+
 scale_fill_manual("Reproductive success measure", values = c("#E64B35B2","#4DBBD5B2","#00A087B2","#FFB84D","#3A6589","#9B5672","#6A6599FF","#DB735C","#EFA86E"))+
  scale_color_manual("Reproductive success measure", values = c("#E64B35B2","#4DBBD5B2","#00A087B2","#FFB84D","#3A6589","#9B5672","#6A6599FF","#DB735C","#EFA86E"))+
  anno.1+anno.2+anno.3+anno.4+anno.5


ggsave(plot.biome, filename="Figs/Fig4.nw.png",
       width = 7.5, height = 8)
##biomes wilpants
wild.plants<-effect.size.total%>%
  filter(Cultivo=="No_crop")
m3 <- rma.mv(yi, vi, random= list(~ 1 | Year, ~ 1 | Title),
             data=wild.plants, method="ML")
biome <- rma.mv(yi, vi, mods = ~ Biomes-1,random= list(~ 1 | Year, ~ 1 | Title),
                data=wild.plants, method="ML")
anova(biome,m3)
wild.pl.reml <- rma.mv(yi, vi, mods = ~ Biomes-1,random= list(~ 1 | Year, ~ 1 | Title),
                       data=wild.plants, method="REML")
r2_ml(wild.pl.reml)
summary(wild.pl.reml)
HetModel.wp <- orchaRd::mod_results(wild.pl.reml, mod = "Biomes",
                                   group = "Title",
                                   weights = "prop")

fd<- wild.plants%>%
  filter(Biomes=="Deserts")
k.1 <- dim(fd)[1]
stdy.1<- fd%>% summarise(stdy = length(unique(Title)))
anno.1 <- annotate("text", x = 1.2, y = 2.5, size=4.5,label = TeX(paste0("\\textit{k} = ", k.1, " (", stdy.1, ")")))

fmed<- wild.plants%>%
  filter(Biomes=="Mediterranean")
k.2 <- dim(fmed)[1]
stdy.2<- fmed%>% summarise(stdy = length(unique(Title)))
anno.2 <- annotate("text", x = 2.2, y = 2.5, size=4.5,label = TeX(paste0("\\textit{k} = ", k.2, " (", stdy.2, ")")))

fo<- wild.plants%>%
  filter(Biomes=="Others")
k.3 <- dim(fo)[1]
stdy.3<- fd%>% summarise(stdy = length(unique(Title)))
anno.3 <- annotate("text", x = 3.2, y = 2.5, size=4.5,label = TeX(paste0("\\textit{k} = ", k.3, " (", stdy.3, ")")))

ftem<- wild.plants%>%
  filter(Biomes=="Temperate")
k.4 <- dim(ftem)[1]
stdy.4<- ftem%>% summarise(stdy = length(unique(Title)))
anno.4 <- annotate("text", x = 4.2, y = 2.5, size=4.5,label = TeX(paste0("\\textit{k} = ", k.4, " (", stdy.4, ")")))

ftrop<- wild.plants%>%
  filter(Biomes=="Tropical")
k.5 <- dim(ftrop)[1]
stdy.5<- ftrop%>% summarise(stdy = length(unique(Title)))
anno.5 <- annotate("text", x = 5.2, y = 2.5, size=4.5,label = TeX(paste0("\\textit{k} = ", k.5, " (", stdy.5, ")")))


plot.wp.biome<-orchaRd::orchard_plot(HetModel.wp, xlab = "Hedges´g", alpha=0.65,angle = 45, g = FALSE, k=F, branch.size=1.5,trunk.size = 4.5,legend.pos = "top.left",
) + theme(legend.direction = "vertical")+
  theme(legend.title = element_text(size =11),
        legend.text = element_text(size = 11),
        axis.text.y = element_text(size =13),
        plot.title = element_text(lineheight=.8),
        axis.text.x=element_text(size=15),
        axis.title=element_text(size=15))+
  scale_y_continuous(limits = c(-8.8,5))+
  scale_fill_manual(values = c("#E64B35B2","#4DBBD5B2","#00A087B2","#FFB84D","#3A6589","#9B5672","#6A6599FF","#DB735C","#EFA86E"))+
  scale_color_manual(values = c("#E64B35B2","#4DBBD5B2","#00A087B2","#FFB84D","#3A6589","#9B5672","#6A6599FF","#DB735C","#EFA86E"))+
  anno.1+anno.2+anno.3+anno.4+anno.5

##biomes for vertebrate pollinators--> compare to Ratto et al 2018
vert<-effect.size.total%>%
  select(Title,Lower_diversity_guild,ACC, PAGE,Vert_Invert,Ratto_Vert,Diurn_Noctur,yi,vi, Biomes)
vert<-vert%>%
  subset(Vert_Invert=="Vertebrate" | Ratto_Vert=="vertebrate")
vert%>%
  group_by(Biomes)%>%
  summarise(n=n())

m3.vert.ml <- rma.mv(yi, vi, random= list(~ 1 | Year, ~ 1 | Title),
                    data=vert, method="ML")
b.vert.ml <- rma.mv(yi, vi, mods = ~ Biomes-1,random= list(~ 1 | Year, ~ 1 | Title),
                      data=vert, method="ML")
anova(b.vert.ml,m3.vert.ml)

b.vert.reml <- rma.mv(yi, vi, mods = ~ Biomes-1,random= list(~ 1 | Year, ~ 1 | Title),
                 data=vert, method="REML")
r2_ml(b.vert.reml)
summary(b.vert.reml)

HetModel.1 <- orchaRd::mod_results(b.vert.reml, mod = "Biomes",
                                   group = "Title",
                                   weights = "prop")

fd<- vert%>%
  filter(Biomes=="Deserts")
k.1 <- dim(fd)[1]
stdy.1<- fd%>% summarise(stdy = length(unique(Title)))
anno.1 <- annotate("text", x = 1.2, y = 2.5, size=4.5,label = TeX(paste0("\\textit{k} = ", k.1, " (", stdy.1, ")")))

fmed<- vert%>%
  filter(Biomes=="Mediterranean")
k.2 <- dim(fmed)[1]
stdy.2<- fmed%>% summarise(stdy = length(unique(Title)))
anno.2 <- annotate("text", x = 2.2, y = 2.5, size=4.5,label = TeX(paste0("\\textit{k} = ", k.2, " (", stdy.2, ")")))

fo<- vert%>%
  filter(Biomes=="Others")
k.3 <- dim(fo)[1]
stdy.3<- fd%>% summarise(stdy = length(unique(Title)))
anno.3 <- annotate("text", x = 3.2, y = 2.5, size=4.5,label = TeX(paste0("\\textit{k} = ", k.3, " (", stdy.3, ")")))

ftem<- vert%>%
  filter(Biomes=="Temperate")
k.4 <- dim(ftem)[1]
stdy.4<- ftem%>% summarise(stdy = length(unique(Title)))
anno.4 <- annotate("text", x = 4.2, y = 2.5, size=4.5,label = TeX(paste0("\\textit{k} = ", k.4, " (", stdy.4, ")")))

ftrop<- vert%>%
  filter(Biomes=="Tropical")
k.5 <- dim(ftrop)[1]
stdy.5<- ftrop%>% summarise(stdy = length(unique(Title)))
anno.5 <- annotate("text", x = 5.2, y = 2.5, size=4.5,label = TeX(paste0("\\textit{k} = ", k.5, " (", stdy.5, ")")))


plot.vert.biome<-orchaRd::orchard_plot(HetModel.1, xlab = "Hedges´g", alpha=0.65,angle = 45, g = FALSE, k=F, branch.size=1.5,trunk.size = 4.5,legend.pos = "top.left",
                                  ) + theme(legend.direction = "vertical")+
  theme(legend.title = element_text(size =11),
        legend.text = element_text(size = 11),
        axis.text.y = element_text(size =13),
        plot.title = element_text(lineheight=.8),
        axis.text.x=element_text(size=15),
        axis.title=element_text(size=15))+
  scale_y_continuous(limits = c(-8.8,5))+
  scale_fill_manual(values = c("#E64B35B2","#4DBBD5B2","#00A087B2","#FFB84D","#3A6589","#9B5672","#6A6599FF","#DB735C","#EFA86E"))+
  scale_color_manual(values = c("#E64B35B2","#4DBBD5B2","#00A087B2","#FFB84D","#3A6589","#9B5672","#6A6599FF","#DB735C","#EFA86E"))+
  anno.1+anno.2+anno.3+anno.4+anno.5

##by far tropical and desserts most studied for vertebrate poll.

ggsave(plot.vert.biome, filename="Figs/vert.biome.nw.png",
       width = 6, height = 5)

biomes<-plot_grid(plot.vert.biome, plot.wp.biome,labels=c("A","B"),label_size = 18,  hjust = -4)
ggsave(biomes, filename="Figs/vert.wp.biome.png",
       width = 10, height = 6)

###vertebrates nocturnal or diurnal
v<-vert%>%
  select(Ratto_Vert, Vert_Invert, Diurn_Noctur)%>%
  group_by(Diurn_Noctur,Ratto_Vert, Vert_Invert)%>%
  summarise(n=n())

vert.dn<-vert[!is.na(vert$Diurn_Noctur), ]
m3.vert.ml <- rma.mv(yi, vi, random= list(~ 1 | Year, ~ 1 | Title),
                     data=vert.dn, method="ML")
dn.vert.ml <- rma.mv(yi, vi, mods = ~ Diurn_Noctur-1,random= list(~ 1 | Year, ~ 1 | Title),
                       data=vert.dn, method="ML")
anova(m3.vert.ml,dn.vert.ml)


dn.vert.reml <- rma.mv(yi, vi, mods = ~ Diurn_Noctur-1,random= list(~ 1 | Year, ~ 1 | Title),
                      data=vert.dn, method="REML")

r2_ml(dn.vert.reml)
summary(dn.vert.reml)

HetModel.1 <- orchaRd::mod_results(dn.vert.reml, mod = "Diurn_Noctur",
                                   group = "Title",
                                   weights = "prop")
fd.n<- vert%>%
  filter(Diurn_Noctur=="Diurnal")
k.1 <- dim(fd.n)[1]
stdy.1<- fd.n%>% summarise(stdy = length(unique(Title)))
anno.1 <- annotate("text", x = 1.2, y = 2.5, size=4.5,label = TeX(paste0("\\textit{k} = ", k.1, " (", stdy.1, ")")))

fn<- vert%>%
  filter(Diurn_Noctur=="Nocturnal")
k.2 <- dim(fn)[1]
stdy.2<- fn%>% summarise(stdy = length(unique(Title)))
anno.2 <- annotate("text", x = 2.2, y = 2.5, size=4.5,label = TeX(paste0("\\textit{k} = ", k.2, " (", stdy.2, ")")))

plot.vert.dn<-orchaRd::orchard_plot(HetModel.1, xlab = "Hedges´g", 
                                    alpha=0.65,angle = 45, g = FALSE, k=F, 
                                    branch.size=1.5,trunk.size = 7.5)+
  theme(legend.direction = "horizontal")+
  theme(legend.title = element_text(size =11),
        legend.text = element_text(size = 11),
        text=element_text(size=10),
        axis.text.y = element_text(size =14),
        plot.title = element_text(lineheight=.8),
        axis.text.x=element_text(size=15),
        axis.title=element_text(size=15))+
  scale_y_continuous(limits = c(-8.8,5))+
  scale_fill_manual(values = c("#E64B35B2","#4DBBD5B2"))+
  scale_color_manual(values = c("#E64B35B2","#4DBBD5B2"))+
  anno.1+anno.2

ggsave(plot.vert.dn, filename="Figs/Ratto.vert_dn.png",
       width = 7, height = 5)
##vert/invert for nocturnal and diurnal pollinators 
vi.dn<-d.n.orchard[!is.na(d.n.orchard$Vert_Invert), ]
vert_invert <- data.frame(
  Vert_Invert = unique(vi.dn$Vert_Invert),  
  id = 1:length(unique(vi.dn$Vert_Invert))
)

color<- c("#E64B35B2","#4DBBD5B2")
vert_invert$color <- color[vert_invert$id]
datos <- merge(vi.dn, vert_invert, by = "Vert_Invert", all.x = TRUE)

fd.n<- vi.dn%>%
  filter(Diurn_Noctur=="Diurnal")
k.1 <- dim(fd.n)[1]
stdy.1<- fd.n%>% summarise(stdy = length(unique(Title)))
anno.1 <- annotate("text", x = 1.2, y = 2.5, size=4.5,label = TeX(paste0("\\textit{k} = ", k.1, " (", stdy.1, ")")))

fn<- vi.dn%>%
  filter(Diurn_Noctur=="Nocturnal")
k.2 <- dim(fn)[1]
stdy.2<- fn%>% summarise(stdy = length(unique(Title)))
anno.2 <- annotate("text", x = 2.2, y = 2.5, size=4.5,label = TeX(paste0("\\textit{k} = ", k.2, " (", stdy.2, ")")))

vi.dn.reml <- rma.mv(yi, vi, mods = ~ Diurn_Noctur-1,random= list(~ 1 | Year, ~ 1 | Title),
                         data=datos, method="REML")


HetModel.vi.dn <- orchaRd::mod_results(vi.dn.reml, mod = "Diurn_Noctur",
                                         group = "Vert_Invert",subset = TRUE,weights = "prop")

plot.vi.dn<-orchaRd::orchard_plot(HetModel.vi.dn, xlab = "Hedges´g",
                                    group="Vert_Invert", colour = TRUE, 
                                    alpha=0.65,angle = 45, g = FALSE, k=F, 
                                    branch.size=1.5,trunk.size = 7.5)+
  theme(legend.direction = "horizontal")+
  theme(legend.title = element_text(size =11),
        legend.text = element_text(size = 11),
        text=element_text(size=10),
        axis.text.y = element_text(size =14),
        plot.title = element_text(lineheight=.8),
        axis.text.x=element_text(size=15),
        axis.title=element_text(size=15))+
  scale_y_continuous(limits = c(-8.8,5))+
  scale_fill_manual(values = c("#E64B35B2","#4DBBD5B2"))+
  scale_color_manual(values = c("#E64B35B2","#4DBBD5B2"))+
  anno.1+anno.2

ggsave(plot.vi.dn, filename="Figs/vert.inv_dn.png",
       width = 7, height = 5)


##vert/invert for nocturnal and diurnal pollinators Ratto
d.n.orchard <- d.n.orchard %>% 
  mutate(Vert_Invert = ifelse(Vert_Invert%in% NA, Ratto_Vert, Vert_Invert))

d.n.orchard$Vert_Invert<-recode(d.n.orchard$Vert_Invert, "vertebrate"="Vertebrate",
                                "no"="Invertebrate")

d.n.orchard<-d.n.orchard[!is.na(d.n.orchard$Vert_Invert),]

unique(d.n.orchard$Vert_Invert)

vert_invert <- data.frame(
  Vert_Invert = unique(d.n.orchard$Vert_Invert),  
  id = 1:length(unique(d.n.orchard$Vert_Invert))
)

color<- c("#E64B35B2","#4DBBD5B2")
vert_invert$color <- color[vert_invert$id]
datos <- merge(d.n.orchard, vert_invert, by = "Vert_Invert", all.x = TRUE)


v.ratto.reml <- rma.mv(yi, vi, mods = ~ Diurn_Noctur-1,random= list(~ 1 | Year, ~ 1 | Title),
                         data=datos, method="REML")


HetModel.v.ratto <- orchaRd::mod_results(v.ratto.reml, mod = "Diurn_Noctur",
                                         group = "Vert_Invert",subset = TRUE,weights = "prop")

fd.n<- d.n.orchard%>%
  filter(Diurn_Noctur=="Diurnal")
k.1 <- dim(fd.n)[1]
stdy.1<- fd.n%>% summarise(stdy = length(unique(Title)))
anno.1 <- annotate("text", x = 1.2, y = 2.5, size=4.5,label = TeX(paste0("\\textit{k} = ", k.1, " (", stdy.1, ")")))

fn<- d.n.orchard%>%
  filter(Diurn_Noctur=="Nocturnal")
k.2 <- dim(fn)[1]
stdy.2<- fn%>% summarise(stdy = length(unique(Title)))
anno.2 <- annotate("text", x = 2.2, y = 2.5, size=4.5,label = TeX(paste0("\\textit{k} = ", k.2, " (", stdy.2, ")")))


plot.v.ratto<-orchaRd::orchard_plot(HetModel.v.ratto, xlab = "Hedges´g",
                                    group="Vert_Invert", colour = TRUE, 
                                    alpha=0.65,angle = 45, g = FALSE, k=F, 
                                    branch.size=1.5,trunk.size = 7.5)+
  theme(legend.direction = "horizontal")+
  theme(legend.title = element_text(size =11),
        legend.text = element_text(size = 11),
        text=element_text(size=10),
        axis.text.y = element_text(size =14),
        plot.title = element_text(lineheight=.8),
        axis.text.x=element_text(size=15),
        axis.title=element_text(size=15))+
  scale_y_continuous(limits = c(-8.8,5))+
  scale_fill_manual(values = c("#E64B35B2","#4DBBD5B2"))+
  scale_color_manual(values = c("#E64B35B2","#4DBBD5B2"))+
  anno.1+anno.2

ggsave(plot.v.ratto, filename="Figs/Ratto.vert.inv_dn.png",
       width = 7, height = 5)

fig.sup.r<- plot_grid(plot.vi.dn,plot.v.ratto, labels=c("A","B"),label_size = 20,  hjust = -1)

ggsave(fig.sup.r, filename="Figs/vert.ratto.png",
       width = 9, height = 4)

##plant fams studies for nocturnal and diurnal
library(Polychrome)
set.seed(757629)
P69 <- createPalette(69, c("#5A5156", "#E4E1E3", "#F6222E","#4DBBD5B2"))
swatch(P69)
names(P69) <- NULL

familias <- data.frame(
  Plant.species.family = unique(effect.size.total$Plant.species.family),  
  id = 1:length(unique(effect.size.total$Plant.species.family))
)

familias$color <- P69[familias$id]
datos <- merge(effect.size.total, familias, by = "Plant.species.family", all.x = TRUE)


d.n.orchard<-datos[!is.na(datos$Diurn_Noctur), ]
p42<-unique(d.n.orchard$color)
swatch(unique(d.n.orchard$color))
unique(d.n.orchard$Plant.species.family)
table(d.n.orchard$Diurn_Noctur)
table<-effect.size.total%>%
  group_by(Plant.species.family)%>%
  summarise(N=n_distinct(Plant.species))

v.plantsp.reml <- rma.mv(yi, vi, mods = ~ Diurn_Noctur+Plant.species.family-1,random= list(~ 1 | Year, ~ 1 | Title),
                   data=d.n.orchard, method="REML")


HetModel.v.plant <- orchaRd::mod_results(v.plantsp.reml, mod = "Diurn_Noctur",
                                         group = "Plant.species.family",subset = TRUE,weights = "prop")
                      
fd.n<- d.n.orchard%>%
filter(Diurn_Noctur=="Diurnal")
k.1 <- dim(fd.n)[1]
stdy.1<- fd.n%>% summarise(stdy = length(unique(Title)))
anno.1 <- annotate("text", x = 1.2, y = 2.5, size=4.5,label = TeX(paste0("\\textit{k} = ", k.1, " (", stdy.1, ")")))

fn<- d.n.orchard%>%
  filter(Diurn_Noctur=="Nocturnal")
k.2 <- dim(fn)[1]
stdy.2<- fn%>% summarise(stdy = length(unique(Title)))
anno.2 <- annotate("text", x = 2.2, y = 2.5, size=4.5,label = TeX(paste0("\\textit{k} = ", k.2, " (", stdy.2, ")")))

plot.v.plant<-orchaRd::orchard_plot(HetModel.v.plant, xlab = "Hedges´g",
                                    group="Plant.species.family", colour = TRUE, 
                                    alpha=0.65,angle = 45, g = FALSE, k=F, 
                                    branch.size=1.5,trunk.size = 7.5)+
  theme(legend.direction = "horizontal")+
  theme(legend.title = element_text(size =11),
        legend.text = element_text(size = 11),
        text=element_text(size=10),
        axis.text.y = element_text(size =14),
        plot.title = element_text(lineheight=.8),
        axis.text.x=element_text(size=15),
        axis.title=element_text(size=15))+
  scale_y_continuous(limits = c(-8.8,5))+
  scale_color_discrete(name="Color Group", values="p42")+
  scale_size_continuous(name="Point Size")
  anno.1+ anno.2

plot.v.plant<-orchaRd::orchard_plot(HetModel.v.plant, xlab = "Hedges´g",
                                    group="Plant.species.family",colour = TRUE, 
                                    alpha=0.65,angle = 45, g = FALSE, k=F, 
                                    branch.size=1.5,trunk.size = 7.5)+
  theme(legend.direction = "horizontal")+
  theme(legend.title = element_text(size =11),
        legend.text = element_text(size = 11),
        text=element_text(size=10),
        axis.text.y = element_text(size =14),
        plot.title = element_text(lineheight=.8),
        axis.text.x=element_text(size=15),
        axis.title=element_text(size=15))+
  scale_y_continuous(limits = c(-8.8,5))+
  scale_fill_manual(values=c("#00C5FB", "#E1E0E0", "#3B4F1C", "#BEE5FC",
                              "#665660", "#EB6342", "#00FCD6", "#FEDD79",
                             "#FA1C38", "#D19FFE" ,"#7C5389", "#897D0D",
                             "#A3A3CE" ,"#5AFC96", "#0053FD", "#008A35",
                             "#FCA4B9", "#F765AA", "#7DA316", "#FFBDA6",
                             "#FDB0E1", "#A80D42", "#E3804B", "#7F6CE6",
                             "#167FB4" ,"#40A7FE" ,"#79E0A5", "#165570",
                             "#32FE0D" ,"#DF69FC" ,"#DCEA00", "#47B9D1",
                             "#8316A8", "#C9EBA7", "#F0DFAC", "#B8008F",
                             "#FD1C85", "#AC967D", "#88D800", "#724922",
                             "#FFE316", "#A40DEA"))+
scale_color_manual(values=c( "#00C5FB", "#E1E0E0", "#3B4F1C", "#BEE5FC",
                             "#665660", "#EB6342", "#00FCD6", "#FEDD79",
                             "#FA1C38", "#D19FFE" ,"#7C5389", "#897D0D",
                             "#A3A3CE" ,"#5AFC96", "#0053FD", "#008A35",
                             "#FCA4B9", "#F765AA", "#7DA316", "#FFBDA6",
                             "#FDB0E1", "#A80D42", "#E3804B", "#7F6CE6",
                             "#167FB4" ,"#40A7FE" ,"#79E0A5", "#165570",
                             "#32FE0D" ,"#DF69FC" ,"#DCEA00", "#47B9D1",
                             "#8316A8", "#C9EBA7", "#F0DFAC", "#B8008F",
                             "#FD1C85", "#AC967D", "#88D800", "#724922",
                             "#FFE316", "#A40DEA"))+
  anno.1+ anno.2

ggsave(plot.v.plant, filename="Figs/S9.png",
       width = 9, height = 6)

##fams studied for vert/invert
v.i.orchard<-datos[!is.na(datos$Vert_Invert), ]
v.i.orchard
table(v.i.orchard$Vert_Invert)
table<-v.i.orchard%>%
  group_by(Vert_Invert,Plant.species.family)%>%
  summarise(N=n())
unique(v.i.orchard$Plant.species.family)
unique(v.i.orchard$color)

vi.plantsp.reml <- rma.mv(yi, vi, mods = ~ Vert_Invert+Plant.species.family-1,random= list(~ 1 | Year, ~ 1 | Title),
                         data=v.i.orchard, method="REML")


HetModel.vi.plant <- orchaRd::mod_results(vi.plantsp.reml, mod = "Vert_Invert",
                                         group = "Plant.species.family",subset = TRUE,  
                                         weights = "prop")


fvi<- v.i.orchard%>%
  filter(Vert_Invert=="Invertebrate")
k.1 <- dim(fvi)[1]
stdy.1<- fvi%>% summarise(stdy = length(unique(Title)))
anno.1 <- annotate("text", x = 1.2, y = 2.5, size=4.5,label = TeX(paste0("\\textit{k} = ", k.1, " (", stdy.1, ")")))

fv<- v.i.orchard%>%
  filter(Vert_Invert=="Vertebrate")
k.2 <- dim(fv)[1]
stdy.2<- fv%>% summarise(stdy = length(unique(Title)))
anno.2 <- annotate("text", x = 2.2, y = 2.5, size=4.5,label = TeX(paste0("\\textit{k} = ", k.2, " (", stdy.2, ")")))

plot.vi.plant<-orchaRd::orchard_plot(HetModel.vi.plant, xlab = "Hedges´g",group="Plant.species.family", colour = TRUE, alpha=0.65,angle = 45, g = FALSE, k=F, branch.size=1.5,trunk.size = 7.5)+
  theme(legend.direction = "horizontal")+
  theme(legend.title = element_text(size =11),
        legend.text = element_text(size = 11),
        text=element_text(size=10),
        axis.text.y = element_text(size =14),
        plot.title = element_text(lineheight=.8),
        axis.text.x=element_text(size=15),
        axis.title=element_text(size=15))+
  scale_y_continuous(limits = c(-8.8,5))+
  scale_fill_manual(values=c( "#00C5FB", "#32408E", "#E1E0E0" ,"#AF00AF",
                              "#3B4F1C", "#FB00FF", "#26FAFF" ,"#BEE5FC",
                              "#665660", "#84321C", "#EB6342", "#00FCD6",
                              "#D2DD6C", "#FEDD79", "#FA1C38", "#D19FFE",
                              "#7C5389", "#A3A3CE", "#FFA92A", "#5AFC96",
                              "#0053FD" ,"#008A35" ,"#FCA4B9", "#A1EEDF",
                              "#F765AA", "#7DA316", "#FFBDA6", "#FDB0E1",
                              "#FC8A92", "#A80D42", "#E3804B", "#7F6CE6",
                              "#167FB4", "#40A7FE", "#22AF1C", "#79E0A5",
                              "#165570" ,"#32FE0D" ,"#F975E8", "#DE2649",
                              "#B45A7D", "#DF69FC", "#FD22C5", "#DCEA00",
                              "#47B9D1", "#819A65" ,"#8316A8", "#C673C4",
                              "#AF731C", "#00B1A8" ,"#870D78", "#3B8270",
                              "#B8008F", "#FD1C85", "#AC967D", "#88D800",
                              "#FFE316", "#A40DEA"))+
  scale_color_manual(values=c("#00C5FB", "#32408E", "#E1E0E0" ,"#AF00AF",
                              "#3B4F1C", "#FB00FF", "#26FAFF" ,"#BEE5FC",
                              "#665660", "#84321C", "#EB6342", "#00FCD6",
                              "#D2DD6C", "#FEDD79", "#FA1C38", "#D19FFE",
                              "#7C5389", "#A3A3CE", "#FFA92A", "#5AFC96",
                              "#0053FD" ,"#008A35" ,"#FCA4B9", "#A1EEDF",
                              "#F765AA", "#7DA316", "#FFBDA6", "#FDB0E1",
                              "#FC8A92", "#A80D42", "#E3804B", "#7F6CE6",
                              "#167FB4", "#40A7FE", "#22AF1C", "#79E0A5",
                              "#165570" ,"#32FE0D" ,"#F975E8", "#DE2649",
                              "#B45A7D", "#DF69FC", "#FD22C5", "#DCEA00",
                              "#47B9D1", "#819A65" ,"#8316A8", "#C673C4",
                              "#AF731C", "#00B1A8" ,"#870D78", "#3B8270",
                              "#B8008F", "#FD1C85", "#AC967D", "#88D800",
                              "#FFE316", "#A40DEA"))+
  anno.1+anno.2

ggsave(plot.vi.plant, filename="Figs/S9.famplantas.vi.png",
       width = 9, height = 6)

##plant fams studied for managed wild pollinators
m.w.orchard<-datos[!is.na(datos$Managed_Wild), ]
unique(m.w.orchard$color)
unique(m.w.orchard$Plant.species.family)

m.w.orchard
table(m.w.orchard$Managed_Wild)
table<-m.w.orchard%>%
  group_by(Managed_Wild,Plant.species.family)%>%
  summarise(N=n())
unique(m.w.orchard$Plant.species.family)

mw.plantsp.reml <- rma.mv(yi, vi, mods = ~Managed_Wild+Plant.species.family-1,random= list(~ 1 | Year, ~ 1 | Title),
                          data=m.w.orchard, method="REML")


HetModel.mw.plant <- orchaRd::mod_results(mw.plantsp.reml, mod = "Managed_Wild",
                                          group = "Plant.species.family",subset = TRUE,  
                                          weights = "prop")


fmw<- m.w.orchard%>%
  filter(Managed_Wild=="Managed")
k.1 <- dim(fmw)[1]
stdy.1<- fmw%>% summarise(stdy = length(unique(Title)))
anno.1 <- annotate("text", x = 1.2, y = 2.5, size=4.5,label = TeX(paste0("\\textit{k} = ", k.1, " (", stdy.1, ")")))

fw<- m.w.orchard%>%
  filter(Managed_Wild=="Wild")
k.2 <- dim(fw)[1]
stdy.2<- fw%>% summarise(stdy = length(unique(Title)))
anno.2 <- annotate("text", x = 2.2, y = 2.5, size=4.5,label = TeX(paste0("\\textit{k} = ", k.2, " (", stdy.2, ")")))

plot.mw.plant<-orchaRd::orchard_plot(HetModel.mw.plant, xlab = "Hedges´g",group="Plant.species.family", colour = TRUE, alpha=0.65,angle = 45, g = FALSE, k=F, branch.size=1.5,trunk.size = 7.5)+
  theme(legend.direction = "horizontal")+
  theme(legend.title = element_text(size =11),
        legend.text = element_text(size = 11),
        text=element_text(size=10),
        axis.text.y = element_text(size =14),
        plot.title = element_text(lineheight=.8),
        axis.text.x=element_text(size=15),
        axis.title=element_text(size=15))+
  scale_y_continuous(limits = c(-8.8,5))+
  scale_fill_manual(values=c("#00C5FB", "#BC9AAF", "#32408E", "#E1E0E0",
                             "#AF00AF" ,"#3B4F1C", "#FB00FF", "#26FAFF",
                             "#9DEC83", "#BEE5FC" ,"#665660", "#84321C",
                             "#FF5D88", "#EB6342", "#00FCD6" ,"#D2DD6C",
                             "#FEDD79", "#FA1C38", "#D19FFE", "#7C5389",
                             "#897D0D", "#A3A3CE" ,"#FFA92A", "#5AFC96",
                             "#0053FD", "#008A35", "#FCA4B9", "#A1EEDF",
                             "#F765AA", "#84979F", "#7DA316", "#FFBDA6",
                             "#FDB0E1", "#FC8A92" ,"#A0AFFC", "#A80D42",
                             "#E3804B", "#7F6CE6", "#167FB4", "#40A7FE",
                             "#22AF1C" ,"#79E0A5", "#165570", "#32FE0D",
                             "#F975E8", "#DE2649", "#B45A7D", "#FE8500",
                             "#DF69FC", "#A33200", "#FD22C5", "#DCEA00",
                             "#47B9D1", "#819A65", "#8316A8", "#C673C4",
                             "#C9EBA7", "#AF731C", "#00B1A8", "#870D78",
                             "#3B8270", "#F0DFAC", "#B8008F", "#FD1C85",
                             "#AC967D", "#88D800", "#724922", "#FFE316",
                             "#A40DEA"))+
  scale_color_manual(values=c("#00C5FB", "#BC9AAF", "#32408E", "#E1E0E0",
                              "#AF00AF" ,"#3B4F1C", "#FB00FF", "#26FAFF",
                              "#9DEC83", "#BEE5FC" ,"#665660", "#84321C",
                              "#FF5D88", "#EB6342", "#00FCD6" ,"#D2DD6C",
                              "#FEDD79", "#FA1C38", "#D19FFE", "#7C5389",
                              "#897D0D", "#A3A3CE" ,"#FFA92A", "#5AFC96",
                              "#0053FD", "#008A35", "#FCA4B9", "#A1EEDF",
                              "#F765AA", "#84979F", "#7DA316", "#FFBDA6",
                              "#FDB0E1", "#FC8A92" ,"#A0AFFC", "#A80D42",
                              "#E3804B", "#7F6CE6", "#167FB4", "#40A7FE",
                              "#22AF1C" ,"#79E0A5", "#165570", "#32FE0D",
                              "#F975E8", "#DE2649", "#B45A7D", "#FE8500",
                              "#DF69FC", "#A33200", "#FD22C5", "#DCEA00",
                              "#47B9D1", "#819A65", "#8316A8", "#C673C4",
                              "#C9EBA7", "#AF731C", "#00B1A8", "#870D78",
                              "#3B8270", "#F0DFAC", "#B8008F", "#FD1C85",
                              "#AC967D", "#88D800", "#724922", "#FFE316",
                              "#A40DEA"))+
  anno.1+anno.2

ggsave(plot.mw.plant, filename="Figs/S9.famplantas.mw.png",
       width = 9, height = 6)

##supplementary figure
fig.sup<- plot_grid(plot.v.plant,plot.vi.plant,plot.mw.plant, labels=c("A","B","C"),label_size = 20,  hjust = -1)

ggsave(fig.sup, filename="Figs/S9.fig.png",
       width = 11, height = 8)

###model for plant families
m3<- rma.mv(yi,vi, random=  list(~ 1 | Year, ~ 1 | Title), data=effect.size.total,method="ML")
rep.fam <- rma.mv(yi,vi,mods=~Plant.species.family-1,random= list(~ 1 | Year, ~ 1 | Title), data=effect.size.total, method="ML")
anova(rep.fam, m3)
# We fit the model with REML to get estimates
rep.fam.reml <- rma.mv(yi,vi,mods=~Plant.species.family-1,random= list(~ 1 | Year, ~ 1 | Title), data=effect.size.total, method="REML")
summary(rep.fam.reml)
# We calculate marginal R^2 of model with moderator
r2_ml(rep.fam.reml)

##estimates
res_fam<- mod_results(rep.fam.reml, mod ="Plant.species.family", group="Title")
box<-res_fam$mod_table
box<-box%>%
  filter(!str_detect(name, ","))
box%>%arrange(estimate) 

##forest plot plant families for supl. material
figu2 <- ggplot (box, aes (y=name))+
  theme_classic()+
    geom_point(aes(x=estimate),  size=3)+
  geom_linerange(aes(xmin=lowerCL, xmax=upperCL))+
  geom_vline(xintercept=0, linetype="dashed", col="#FF5E5B")+
  labs(x="Hedges´g", y="", size=15)+
  theme(axis.text.y = element_text(size =15),
        axis.text.x=element_text(size=15),
        text=element_text(size=15))

ggsave(figu2,filename="Figs/familie.S7.png",
       width = 5.5, height = 12)
###shared families for crops and wild plants
shared_ids <- effect.size.total %>% 
  group_by(Plant.species.family) %>%
  filter(Cultivo %in% c("Crop", "No_crop"))%>%
  filter(n_distinct(Cultivo) == 2) 

p.f<-shared_ids%>%
  group_by(Plant.species.family, Cultivo)%>%
  summarise(mean_yi = mean(yi),
            ci_lower = mean(yi) - qt(0.975, df = n() - 1) * sd(yi) / sqrt(n()),
            ci_upper = mean(yi) + qt(0.975, df = n() - 1) * sd(yi) / sqrt(n())
  )

overall <- shared_ids %>%
  group_by(Cultivo) %>%
  summarise(
    mean_yi = mean(yi),
    ci_lower = mean(yi) - qt(0.975, df = n() - 1) * sd(yi) / sqrt(n()),
    ci_upper = mean(yi) + qt(0.975, df = n() - 1) * sd(yi) / sqrt(n()),
    .groups = 'drop'
  )

# Combine the results into a single data frame
combined_results <- bind_rows(p.f, overall %>% mutate(Plant.species.family = "Overall"))
# Ordenar las familias de plantas según su importancia
plant_order <- c("Thymelaeaceae","Solanaceae","Rubiaceae","Rosaceae",
                 "Proteaceae","Polygonaceae","Malvaceae","Liliaceae",
                 "Fabaceae","Ericaceae","Cucurbitaceae", "Cactaceae",
                 "Brassicaceae","Asteraceae","Amaryllidaceae", "Overall")  # Agrega aquí el orden deseado

# Convertir Plant.species.family en factor y establecer el orden deseado
combined_results$Plant.species.family <- factor(combined_results$Plant.species.family, levels = plant_order)

##forest plot shared families crop no crop
fig.shared <- ggplot (combined_results, aes (y=Plant.species.family, x=mean_yi))+
  theme_classic()+
  facet_wrap(~Cultivo, nrow = 1,labeller = labeller(Cultivo = c("Crop" = "Crops",
                                                             "No_crop" = "Wild plants")))+
  geom_point(size=3.5)+
  geom_linerange(aes(xmin=ci_lower, xmax=ci_upper))+
  geom_vline(xintercept=0, linetype="dashed", col="#FF5E5B")+
  labs(x="Hedges´g", y="", size=15)+
  theme(axis.text.y = element_text(size =15),
        axis.text.x=element_text(size=15),
        text=element_text(size=15))+ 
  geom_text(aes(label = round(mean_yi, 2), y=Plant.species.family), nudge_x = -1.4,nudge_y = 0.35,size = 4, color = "black")
ggsave(fig.shared,filename="Figs/Sharedfam.wc.png",
       width = 6.5, height = 7)
###shared families for wild and managed pollinators
###shared families for crops and wild plants
shared_wild.man <- effect.size.total %>% 
  group_by(Plant.species.family) %>%
  filter(Managed_Wild %in% c("Managed", "Wild"))%>%
  filter(n_distinct(Managed_Wild) == 2) 

p.wm<-shared_wild.man%>%
  group_by(Plant.species.family, Managed_Wild)%>%
  summarise(mean_yi = mean(yi),
            ci_lower = mean(yi) - qt(0.975, df = n() - 1) * sd(yi) / sqrt(n()),
            ci_upper = mean(yi) + qt(0.975, df = n() - 1) * sd(yi) / sqrt(n())
  )

overall <- shared_wild.man%>%
  group_by(Managed_Wild) %>%
  summarise(
    mean_yi = mean(yi),
    ci_lower = mean(yi) - qt(0.975, df = n() - 1) * sd(yi) / sqrt(n()),
    ci_upper = mean(yi) + qt(0.975, df = n() - 1) * sd(yi) / sqrt(n()),
    .groups = 'drop'
  )

# Combine the results into a single data frame
combined_results.wm <- bind_rows(p.wm, overall %>% mutate(Plant.species.family = "Overall"))
# Ordenar las familias de plantas según su importancia
plant_order <- c("Anacardiaceae","Apiaceae","Brassicaceae","Cucurbitaceae",
                 "Ericaceae","Fabaceae", "Rosaceae","Solanaceae" ,"Overall")  # Agrega aquí el orden deseado

# Convertir Plant.species.family en factor y establecer el orden deseado
combined_results.wm$Plant.species.family <- factor(combined_results.wm$Plant.species.family, levels = plant_order)

##forest plot shared families crop no crop
fig.shared.wm <- ggplot (combined_results.wm, aes (y=Plant.species.family, x=mean_yi))+
  theme_classic()+
  facet_wrap(~Managed_Wild, nrow = 1)+
  geom_point(size=3)+
  geom_linerange(aes(xmin=ci_lower, xmax=ci_upper))+
  geom_vline(xintercept=0, linetype="dashed", col="#FF5E5B")+
  labs(x="Hedges´g", y="", size=15)+
  theme(axis.text.y = element_text(size =15),
        axis.text.x=element_text(size=15),
        text=element_text(size=15))+ 
  geom_text(aes(label = round(mean_yi, 2)), hjust=0,nudge_x = -0.4 ,nudge_y = 0.2,size = 4, color = "black")

ggsave(fig.shared.wm,filename="Figs/Sharedfam.wm.png",
       width = 6.5, height = 7)

s10<- plot_grid(fig.shared, fig.shared.wm, labels=c("A","B"),label_size = 18,  hjust = -1)
ggsave(s10, filename="Figs/S.shared.png",
       width = 11, height = 6)

###study type orchard plot
mesoc<- effect.size.total%>%
  mutate(Obs.Exp = case_when(
    Landscape== "Experimental" ~ "Mesocosmos",
    TRUE~ Obs.Exp))

mesoc$Obs.Exp<- recode(mesoc$Obs.Exp, 
                       "Exp"="Exclusions",
                       "Obs"="Observational")
mesoc<-mesoc%>%
  filter(!is.na(Obs.Exp))

unique(mesoc$Obs.Exp)  
table(mesoc$Obs.Exp)

study.type.m3 <- rma.mv(yi, vi,random= list(~ 1 | Year, ~ 1 | Title),
                     data=mesoc, method="ML")
study.type <- rma.mv(yi, vi, mods = ~ Obs.Exp -1,random= list(~ 1 | Year, ~ 1 | Title),
                data=mesoc, method="ML")
anova(study.type.m3, study.type)

stype.reml <- rma.mv(yi, vi, mods = ~ Obs.Exp -1,random= list(~ 1 | Year, ~ 1 | Title),
                 data=mesoc, method="REML")
r2_ml(stype.reml)
summary(stype.reml)

##with intercept
stype.reml.inter <- rma.mv(yi, vi, mods = ~ Obs.Exp,random= list(~ 1 | Year, ~ 1 | Title),
                     data=mesoc, method="REML")

summary(stype.reml.inter)

HetModel.st <- orchaRd::mod_results(stype.reml, mod = "Obs.Exp",
                                   group = "Title",subset = TRUE,
                                   weights = "prop")

st<- mesoc%>%
  filter(Obs.Exp=="Observational")
k.1<- dim(st)[1]
stdy.1 <- st %>% summarise(stdy = length(unique(Title)))
anno.1 <- annotate("text", x = 3.2, y = 1.6, size=4.5,label = TeX(paste0("\\textit{k} = ", k.1, " (", stdy.1, ")")))

st.ms<- mesoc%>%
  filter(Obs.Exp=="Mesocosmos")
k.2<- dim(st.ms)[1]
stdy.2 <- st.ms %>% summarise(stdy = length(unique(Title)))
anno.2 <- annotate("text", x = 2.2, y = 1.6, size=4.5,label = TeX(paste0("\\textit{k} = ", k.2, " (", stdy.2, ")")))

st.ex<- mesoc%>%
  filter(Obs.Exp=="Exclusions")
k.3<- dim(st.ex)[1]
stdy.3 <- st.ex %>% summarise(stdy = length(unique(Title)))
anno.3 <- annotate("text", x = 1.2, y = 1.6, size=4.5,label = TeX(paste0("\\textit{k} = ", k.3, " (", stdy.3, ")")))


plot.stype<-orchaRd::orchard_plot(HetModel.st, xlab = "Hedges´g", alpha=0.65,angle = 45, g = FALSE, k=F, branch.size=1.5,trunk.size = 4.5,
                                  ) + theme(legend.direction = "horizontal")+
  theme(legend.title = element_text(size =13),
        legend.text = element_text(size = 13),
        text=element_text(size=13),
        axis.text.y = element_text(size =13),
        axis.text.x=element_text(size=15),
        axis.title=element_text(size=15))+
  scale_y_continuous(limits = c(-8.8,5))+
  scale_fill_manual("Obs.Exp", values = c("#00A087B2","#4DBBD5B2","#9B5672"))+
  scale_color_manual("Obs.Exp", values = c("#00A087B2","#4DBBD5B2","#9B5672"))+
   annotate(geom = "text", x = 3.15, y = -5.25, 
         color = "black", size = 5, label=TeX(paste0("95% CI = ", round(HetModel.st$mod_table[3,3], 3), " to ", round(HetModel.st$mod_table[3,4], 3))))+
  annotate(geom = "text", x = 2.15, y = -5.25, 
           color = "black", size = 5, label=TeX(paste0("95% CI = ", round(HetModel.st$mod_table[2,3], 3), " to ", round(HetModel.st$mod_table[2,4], 3))))+
  annotate(geom = "text", x = 1.15, y = -5.25, 
          color = "black", size = 5, label=TeX(paste0("95% CI = ", round(HetModel.st$mod_table[1,3], 3), " to ", round(HetModel.st$mod_table[1,4], 3))))+
     anno.1+ anno.2 + anno.3
     

ggsave(plot.stype, filename="Figs/S6study.type.png",
       width =9, height = 6)


###van diagram
install.packages("VennDiagram")
library(VennDiagram)
library(eulerr)


df<-effect.size.total[,c(1,54,55,56)]
# Inicializar listas vacías para cada combinación
listas_diurnal <- list()
listas_nocturnal <- list()
listas_vertebrate <- list()
listas_invertebrate <- list()
listas_managed <- list()
listas_wild <- list()


status_column <- "Diurn_Noctur"  
status_column2 <- "Managed_Wild"    
status_column3<- "Vert_Invert"

# Iterate over each row of the DataFrame
for (i in 1:nrow(df)) {
  # Check if the value is not NA and is "Diurnal"
  if (!is.na(df[i, status_column]) && df[i, status_column] == "Diurnal") {
    listas_diurnal <- c(listas_diurnal, list(df[i, ]))
  }
  
  if (!is.na(df[i, status_column]) && df[i, status_column] == "Nocturnal") {
    listas_nocturnal <- c(listas_nocturnal, list(df[i, ]))
  }
  
  if (!is.na(df[i, status_column2]) && df[i, status_column2] == "Wild") {
    listas_wild <- c(listas_wild, list(df[i, ]))
  }
  if (!is.na(df[i, status_column2]) && df[i, status_column2] == "Managed") {
    listas_managed <- c(listas_managed, list(df[i, ]))
  }
  if (!is.na(df[i, status_column3]) && df[i, status_column3] == "Vertebrate") {
    listas_vertebrate <- c(listas_vertebrate, list(df[i, ]))
  }
  if (!is.na(df[i, status_column3]) && df[i, status_column3] == "Invertebrate") {
    listas_invertebrate <- c(listas_invertebrate, list(df[i, ]))
  }
  
}




# List of sets
venn_data <- list(
  A = listas_diurnal,
  B = listas_nocturnal,
  C = listas_vertebrate,
  D = listas_invertebrate,
  #E = listas_managed,
  G = listas_wild
)

str(venn_data)


# Create the Venn diagram
venn.plot <- venn.diagram(
  x = venn_data,
  category.names = c("Diurnal", "Nocturnal", "Vertebrate", "Invertebrate",  "Wild"),
  filename = NULL, # Use NULL to not save automatically to a file
  output = TRUE,
  fill = c("red", "green", "blue",  "yellow", "purple"),
  alpha = 0.5,
  cex = 2,
  cat.cex = 2,
  cat.pos = 0,
  cat.dist = 0.05
)

# Display the Venn diagram

png(file="Figs/S10.Vennplot.png",units = "in",res = 1200, width=7, height=6) # Open PNG device with specific file name
grid.draw(venn.plot)  
dev.off() 
                                                      
