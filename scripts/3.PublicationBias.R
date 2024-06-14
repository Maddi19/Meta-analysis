#######Publication bias and time lags bias#########
repr.success <- read.csv("data/3.effectsizes_clean_nw.csv")

##remove data without effect size (there is one without country)
##remove also observations on abundance and visitation rates
effect.size.total<-repr.success[!is.na(repr.success$yi), ]
unique(effect.size.total$Pollinator.variable.measure)
effect.size.total<-effect.size.total%>%
  filter(!str_detect(Pollinator.variable.measure, "abundance")) %>%
  filter(!str_detect(Pollinator.variable.measure, "visitation"))
effect.size.total<-effect.size.total[!is.na(effect.size.total$vi), ]

##m3 from models on effect size figures script (4.script)
m3<- rma.mv(yi,vi, random=  list(~ 1 | Year, ~ 1 | Title), data=effect.size.total,method="ML")
AIC(m3)
m3.reml<- rma.mv(yi,vi, random=  list(~ 1 | Year, ~ 1 | Title), data=effect.size.total,method="REML")
summary(m3.reml)

# Time-lag bias figure
years<- effect.size.total%>%
  subset(Year%in%c(1990:2022))
# We construct a model with publication year as moderator. Here we allow the intercept so the model output reports the intercept and slope for any time trend
YRS<- rma.mv(yi, vi, mods = ~Year, random = list(~ 1 | Year, ~ 1 | Title), data = effect.size.total, method = "ML")
anova(YRS, m3)

YRS.reml<- rma.mv(yi,vi, mods = ~Year, random = list(~ 1 | Year, ~ 1 | Title), data = effect.size.total, method = "REML")
summary(YRS.reml)

lim_bubble <- orchaRd::mod_results(YRS.reml, mod = "Year", group = "Title", weights = "prop")

publi.bias <- orchaRd::bubble_plot(lim_bubble, group = "Title", mod = "Year", xlab = "Year",
                                   legend.pos = "top.left", ci.col = "#6A6599FF",
                                   pi.col = "#DB735C",ci.lwd =0.75,pi.lwd=0.75)+
  theme(legend.title = element_text(size =15),
        legend.text = element_text(size = 15),
        text=element_text(size=20),
        axis.text.y = element_text(size =20))+
 ggtitle("Effect sizes across years")

ggsave(publi.bias, filename="Figs/S3publi.bias.png",
       width = 8.5, height = 6)

###publication bias
## Funnel plot ----
funnel_full <- rma.mv(yi, vi,
                     random =  list(~ 1 | Year, ~ 1 | Title), data = effect.size.total, method = "REML")
f2 <- funnel(funnel_full, yaxis = "seinv", level = c(90, 95, 99),
             shade = c("white", "gray55", "gray75"),
             refline = 0,  ylim=c(0.45,3))

png(file="Figs/S2.funnel.png",units = "in",res = 1200, width=7, height=6) # Open PNG device with specific file name
funnel(funnel_full, yaxis = "seinv", level = c(90, 95, 99),
       shade = c("white", "gray55", "gray75"),
       refline = 0,  ylim=c(0.45,3))
dev.off() 


dmetar::eggers.test(x = funnel_full)

## Egger's regression ----
# Using the square-root of the inverse of the effective sample size to handle nonindependence
effect.size.total$inv_effect_n<-(1/effect.size.total$seT+(1/effect.size.total$seC))
effect.size.total$sqrt_inv_effect_n<-sqrt(effect.size.total$inv_effect_n)
effect.size.total<-effect.size.total%>%
  filter(!is.na(sqrt_inv_effect_n))

egger1 <- rma.mv(yi,vi,mods=~sqrt_inv_effect_n, random = list(~ 1 | Year, ~ 1 | Title),
                 data=effect.size.total,method="REML")

summary(egger1)
r2_ml(egger1)

library(dmetar)
eggers.test(funnel_full)

regtest(funnel_full, model="lm")
ranktest(funnel_full)


##fail safe number
fsn(yi,vi, data=effect.size.total)
5*780+10

##trimfill
m3.remlk<- rma(yi,vi, data=effect.size.total,method="REML")
funnel(m3.remlk)
trimfill(m3.remlk)
funnel(m3.remlk)

## Hat values ----
# To identify outliers in the dataset
rsRES5.reml=rstandard(m3.reml)
hatRES5.reml=hatvalues(m3.reml)/mean(hatvalues(m3.reml))
plot(hatRES5.reml,rsRES5.reml$resid, xlab="Hat values", ylab="lnRR residuals", yaxt = "n", xaxt="n", ylim=c(-1.2, 3.5))
abline(h=3, lty=3)
abline(v=2, lty=3)
axis(side = 2,las = 2,mgp = c(3,0.75,0))
axis(side=1, mgp=c(3,0.75,0))


# Sensitivity analyses (SA)

## Cook's distance
# Calculate and plot Cook's distance for every effect size
cooksd <- cooks.distance(m3.reml, progbar=TRUE, reestimate=F)
sample_size <- nrow(funnel_full)

png(file="Figs/S4.cook.png",units = "in",res = 1200, width=7, height=6) # Open PNG device with specific file name
plot(cooksd, pch="*", cex=2, xlab="Observation number", ylab="Cook's distance", yaxt = "n", xaxt="n")
axis(side=2,las =2,mgp = c(3,0.75, 0))
axis(side=1, mgp=c(3,0.75,0))
abline(h = 4/751, col="red")
dev.off() 

##14outliers. 4/sample size

influential.1 <- as.numeric(names(cooksd)[(cooksd > (4/780))])
remove.outliers<- effect.size.total[-influential.1, ] # create new dataset removing influential observations
unique(remove.outliers$Title)
RES5.reml.SA2 <- rma.mv(yi = yi, V = vi, random = list(~1|Title, ~1|Year),  data = remove.outliers, method = "REML")
summary(RES5.reml.SA2)
RES5.reml.SA2.res <- (orchaRd::mod_results(RES5.reml.SA2, mod = "1", group="yi")) # table of results
print(RES5.reml.SA2.res)
##similar overall effect size without outliers. 
##table results to supl. material


## Vcv matrix with studyID as clustering variable ----
# Clustering the effect sizes at the level of individual studies 
install.packages("devtools")
library(devtools)
install_github("daniel1noble/metaAidR")
library(metaAidR)
install.packages("matrixcalc")
library(matrixcalc) 

V.ser1.study_0.5<-metaAidR::make_VCV_matrix(data = effect.size.total, V = "vi", cluster = "Title", obs = "yi", type = "vcv", rho = 0.5)

RES5.reml.SA4 <- rma.mv(yi = yi, V = V.ser1.study_0.5, random = list(~1|Title, ~1|Year),  data = effect.size.total, method = "REML")
summary(RES5.reml.SA4)

RES5.reml.SA4.res <- (orchaRd::mod_results(RES5.reml.SA4, mod = "1", group="yi")) # table of results
print(RES5.reml.SA4.res)
###table results to supl. material