# Publication bias ----
## Time-lag bias ----
# We construct a model with publication year as moderator. Here we allow the intercept so the model output reports the intercept and slope for any time trend
repr.success <- read.csv("data/3.effectsizes_clean.csv")

##eliminar datos sin effect size (hay uno sin country)
effect.size.total<-repr.success[!is.na(repr.success$yi), ]
unique(effect.size.total$Pollinator.variable.measure)
effect.size.total<-effect.size.total%>%
  filter(!str_detect(Pollinator.variable.measure, "abundance")) %>%
  filter(!str_detect(Pollinator.variable.measure, "visitation"))
effect.size.total<-effect.size.total[!is.na(effect.size.total$vi), ]

##m3 sacado en fig.effectsizes
m3<- rma.mv(yi,vi, random=  list(~ 1 | Year, ~ 1 | Title), data=effect.size.total,method="ML")
AIC(m3)
m3.reml<- rma.mv(yi,vi, random=  list(~ 1 | Year, ~ 1 | Title), data=effect.size.total,method="REML")
summary(m3.reml)


# Time-lag bias figure
years<- effect.size.total%>%
  subset(Year%in%c(1990:2022))
  

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

ggsave(publi.bias, filename="RData/figures/publi.bias.png",
       width = 8.5, height = 6)




## Funnel plot ----
# Funnel plot of residuals
# Reproductive success measure was added as moderator to account for some heterogeneity
funel<-effect.size.total%>%
  subset(Reproductive.succes.measure %in% c("fruit set", "seed set","fruit weight"))
funnel_ser <- rma.mv(yi, vi, mods = ~Reproductive.succes.measure,
                     random =  list(~ 1 | Year, ~ 1 | Title), data = funel, method = "REML")

f1 <- funnel(funnel_ser, yaxis = "seinv", level = c(90, 95, 99),
             shade = c("white", "gray55", "gray75"),
             refline = 0,  ylim=c(0.45,1.23))


funnel(funnel_ser,xlab="Hedges´g", yaxis = "seinv")

funnel_full <- rma.mv(yi, vi,
                     random =  list(~ 1 | Year, ~ 1 | Title), data = effect.size.total, method = "REML")
f2 <- funnel(funnel_full, yaxis = "seinv", level = c(90, 95, 99),
             shade = c("white", "gray55", "gray75"),
             refline = 0,  ylim=c(0.45,3))


dmetar::eggers.test(x = funnel_ser)
dmetar::eggers.test(x = funnel_full)

## Egger's regression ----
# Using the square-root of the inverse of the effective sample size to handle nonindependence
# Quality trait was added as moderator to account for some heterogeneity
effect.size.total$inv_effect_n<-(1/effect.size.total$seT+(1/effect.size.total$seC))
effect.size.total$sqrt_inv_effect_n<-sqrt(effect.size.total$inv_effect_n)
effect.size.total<-effect.size.total%>%
  filter(!is.na(sqrt_inv_effect_n))

egger1 <- rma.mv(yi,vi,mods=~sqrt_inv_effect_n, random = list(~ 1 | Year, ~ 1 | Title),
                 data=effect.size.total,method="REML")

summary(egger1)
r2_ml(egger1)

library(meta)
meta::metabias(funnel_ser, method.bias = "linreg")

library(dmetar)
eggers.test(funnel_full)

regtest(funnel_full, model="lm")
ranktest(funnel_full)
regtest()

fsn(yi,vi, data=effect.size.total)
5*751+10

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


# Sensitivity analyses (SA) ----

## Cook's distance (SA2) ----
# Calculate and plot Cook's distance for every effect size
cooksd <- cooks.distance(m3.reml, progbar=TRUE, reestimate=F)
sample_size <- nrow(funnel_full)
plot(cooksd, pch="*", cex=2, xlab="Observation number", ylab="Cook's distance", yaxt = "n", xaxt="n")
axis(side=2,las =2,mgp = c(3,0.75, 0))
axis(side=1, mgp=c(3,0.75,0))
abline(h = 4/751, col="red")

##12outliers. 4/sample sized

influential.1 <- as.numeric(names(cooksd)[(cooksd > (4/751))])
remove.outliers<- effect.size.total[-influential.1, ] # create new dataset removing influential observations

write.csv(remove.outliers, file="RData/removedoutliers.csv")

RES5.reml.SA2 <- rma.mv(yi = yi, V = vi, random = list(~1|Title, ~1|Year),  data = remove.outliers, method = "REML")
summary(RES5.reml.SA2)
RES5.reml.SA2.res <- (orchaRd::mod_results(RES5.reml.SA2, mod = "1", group="yi")) # table of results
print(RES5.reml.SA2.res)

##eliminamos 11 outliers segun funnel plot
remove.outliers<- remove.outliers%>%
  arrange(yi)%>%
  slice(-(1:15))

funel<-remove.outliers%>%
  subset(Reproductive.succes.measure %in% c("fruit set", "seed set","fruit weight"))
funnel_ser <- rma.mv(yi, vi, mods = ~Reproductive.succes.measure,
                     random =  list(~ 1 | Year, ~ 1 | Title), data = funel, method = "REML")

f1 <- funnel(funnel_ser, yaxis = "seinv", level = c(90, 95, 99),
             shade = c("white", "gray55", "gray75"),
             refline = 0,  ylim=c(0.45,1.23))


funel$inv_effect_n<-(1/funel$N.control)+(1/funel$N.treat)
funel$sqrt_inv_effect_n<-sqrt(funel$inv_effect_n)
egger1 <- rma.mv(yi=yi,
                 V=vi,
                 mods=~Reproductive.succes.measure+sqrt_inv_effect_n , random = list(~ 1 | Year, ~ 1 | Title),
                 data=funel,method="REML", sparse=TRUE)
summary(egger1)
r2




## Vcv matrix with studyID as clustering variable (SA4) ----
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
