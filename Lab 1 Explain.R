##Load all necessary packages
library("haven")
library("dplyr")
library("psych")
library("stringr")
library("lavaan")
library("corrplot")
library("tidySEM")

##Read file in
ESS_BE<-read.csv("ESS-Data-Wizard-subset-2023-10-04.csv",na.string=TRUE)
str(ESS_BE)

##Select the 21 variables needed
ESS_BE_lab<-select(ESS_BE,gvslvol,gvslvue,gvhlthc,gvcldcr,gvjbevn,gvpdlwk,
                   sbstrec,sbbsntx,sbprvpv,sbeqsoc,sbcwkfm,sblazy,sblwcoa,sblwlka,
                   agea,eduyrs,gndr,hinctnta,gincdif,dfincac,smdfslv)
str(ESS_BE_lab)

##Select the variables for welfare support and criticism 
WelfareOpinion<-select(ESS_BE_lab,gvslvol,gvslvue,gvhlthc,gvcldcr,gvjbevn,gvpdlwk,
                       sbstrec,sbbsntx,sbprvpv,sbeqsoc,sbcwkfm,sblazy,sblwcoa,sblwlka)

##Need to get rid of the missing value
WelfareOpinion$gvslvol<-as.numeric(sub(88,NA,WelfareOpinion$gvslvol))
WelfareOpinion$gvslvue<-as.numeric(sub(88,NA,WelfareOpinion$gvslvue))
WelfareOpinion$gvhlthc<-as.numeric(sub(88,NA,WelfareOpinion$gvhlthc))
WelfareOpinion$gvcldcr<-as.numeric(sub(88,NA,WelfareOpinion$gvcldcr))
WelfareOpinion$gvjbevn<-as.numeric(sub(88,NA,WelfareOpinion$gvjbevn))
WelfareOpinion$gvpdlwk<-as.numeric(sub(88,NA,WelfareOpinion$gvpdlwk))

WelfareOpinion$sbstrec<-as.numeric(sub(8,NA,WelfareOpinion$sbstrec))
WelfareOpinion$sbbsntx<-as.numeric(sub(8,NA,WelfareOpinion$sbbsntx))
WelfareOpinion$sbprvpv<-as.numeric(sub(8,NA,WelfareOpinion$sbprvpv))
WelfareOpinion$sbeqsoc<-as.numeric(sub(8,NA,WelfareOpinion$sbeqsoc))
WelfareOpinion$sbcwkfm<-as.numeric(sub(8,NA,WelfareOpinion$sbcwkfm))
WelfareOpinion$sblazy<-as.numeric(sub(8,NA,WelfareOpinion$sblazy))
WelfareOpinion$sblwcoa<-as.numeric(sub(8,NA,WelfareOpinion$sblwcoa))
WelfareOpinion$sblwlka<-as.numeric(sub(8,NA,WelfareOpinion$sblwlka))

##To ask for the descriptive statistics
WelfareOpinion_Descriptives<-as.data.frame(psych::describe(WelfareOpinion))
View(WelfareOpinion_Descriptives)

##Make the subset of welfare support
Welfare_Support<-select(WelfareOpinion,gvslvol,gvslvue,gvhlthc,gvcldcr,gvjbevn,gvpdlwk)

##To make the observed covariance matrix:
Welfare_Support_ObCov<-cov(Welfare_Support,use = "pairwise.complete.obs")
Welfare_Support_ObCov

##To make the correlation matrix
Welfare_Support_ObCor<-cov2cor(Welfare_Support_ObCov)
Welfare_Support_ObCor


corrplot::corrplot(Welfare_Support_ObCor,
                   is.corr = FALSE,
                   method = "circle",
                   type = "upper",
                   addCoef.col = "black")

corrplot::corrplot(Welfare_Support_ObCov,
                   is.corr = FALSE,
                   method = "square",
                   type = "lower",
                   addCoef.col = "blue")

##CFA steps
#First, we need to specify the measurement model with the indicators for the latent variable
model_ws_3Ind<-'welfareSupport=~gvslvol+ gvslvue+gvhlthc'
cfa_fit_3Ind<-cfa(model = model_ws_3Ind, data=WelfareOpinion)
summary(cfa_fit_3Ind,standardized=TRUE)

#For the constrain on latent variable variance as 1
model_ws_alt<-'welfareSupport=~NA*gvslvol+ gvslvue+gvhlthc
welfareSupport~~1*welfareSupport'
cfa_fit_alt<-cfa(model_ws_alt,data=Welfare_Support)
summary(cfa_fit_alt, standardized=TRUE)

#Trying for the 6 indicators instead of 3 indicators
model_ws_6Ind<-'welfareSupport=~gvslvol+gvslvue+gvhlthc+gvcldcr+gvjbevn+gvpdlwk'
model_fit_ws_6Ind<-cfa(model=model_ws_6Ind,data=Welfare_Support)
summary(model_fit_ws_6Ind,standardized=TRUE)


##Compare the observed and implied variance-covariance matrix
lavInspect(model_fit_ws_6Ind,"sampstat")
fitted(model_fit_ws_6Ind)
residuals(model_fit_ws_6Ind)

##Request the SE?
lavTech(model_fit_ws_6Ind,"se")

##To see the confidence interval of the factor loading, and the CI the common variance of each indicator 
parameterestimates(model_fit_ws_6Ind,standardized=TRUE)
parameterEstimates(model_fit_ws_6Ind,standardized=TRUE)

#To see model fit:
summary(model_fit_ws_6Ind,fit.measures=TRUE,
        standardized=TRUE)

##Subtract the standardized factor loading:
inspect(model_fit_ws_6Ind,what = "std")$lambda

##Tidy result for specific parameters:
tidy_results<-table_results(model_fit_ws_6Ind,
                            columns = c("label","est_sig","se","confint"),
                            digits=3)
tidy_results<-filter(tidy_results,str_detect(label,"welfareSupport"))

##Residual Variance
RegUniqueVar<-round(inspect(model_fit_ws_6Ind,"est")$theta,digits = 3)
RegUniqueVar
uniqueVar<-round(inspect(model_fit_ws_6Ind,"std")$theta,digits = 3)
uniqueVar
R2<-round(inspect(model_fit_ws_6Ind,"r2"),digits = 3)
R2

residualVar<-data.frame(RegularResiduals=diag(RegUniqueVar),
                        StandUniResiduals=diag(uniqueVar),
                        R_square=R2)
residualVar


##Modification Indices
mi<-inspect(model_fit_ws_6Ind,"mi")
mi.sorted<-mi[order(-mi$mi),]
head(mi.sorted)

plot(mi.sorted$mi)
abline(h=3.84)

###CFA for 3-factor
##first select the welfare criticism variables:
welfare_criticism<-select(WelfareOpinion,sbstrec,sbbsntx,sbprvpv,sbeqsoc,sbcwkfm,sblazy,sblwcoa,sblwlka)

##Sample variance-covariance matrix
welfareCriCov<-cov(welfare_criticism,use="pairwise.complete.obs")
welfareCriCor<-cov2cor(welfareCriCov)
corrplot::corrplot(welfareCriCor,
                   is.corr=FALSE,
                   method = "circle",
                   type = "upper",
                   addCoef.col = "black")


##3-factor model:
model_3factor<-'
wc_econo=~sbstrec+sbbsntx
wc_socio=~sbprvpv+sbeqsoc+sbcwkfm
wc_moral=~sblazy+sblwcoa+sblwlka
'

fit_3factor<-cfa(model_3factor,data=welfare_criticism)
summary(fit_3factor,standardized=TRUE)

##plotting
layout<-get_layout("wc_econo","","","wc_socio","","","wc_moral","",
                   "sbstrec","sbbsntx","sbprvpv","sbeqsoc","sbcwkfm","sblazy","sblwcoa","sblwlka",
                   rows=2)
layout

plot_wc<-graph_sem(model=fit_3factor, layout=layout, angle=200)
plot_wc

