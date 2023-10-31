##Read the data in
ESS4_BE<-haven::read_sav("https://github.com/albertostefanelli/SEM_labs/raw/master/data/ESS4_belgium.sav")

##Variables needed for analysis: gvslvol gvhlthc gvcldcr gvpdlwk
hist_gvslvol<-ggplot(ESS4_BE,aes(gvslvol)) + geom_blank()+ geom_histogram(aes(y=after_stat(density)),
                                                            binwidth = 1,
                                                            colour="black",
                                                            alpha=0.3)
hist_gvhlthc<-ggplot(ESS4_BE, aes(gvhlthc))+geom_histogram(aes(y=after_stat(density)),
                                                  binwidth = 1,
                                                  colour="black",
                                                  alpha=0.3)
hist_gvcldcr<-ggplot(ESS4_BE,aes(gvcldcr))+geom_histogram(aes(y=after_stat(density)),
                                                          binwidth = 1,
                                                          colour="black",
                                                          alpha=0.3)
hist_gvpdlwk<-ggplot(ESS4_BE,aes(gvpdlwk))+geom_histogram(aes(y=after_stat(density)),
                                                          binwidth = 1,
                                                          colour="black",
                                                          alpha=0.3)
hist_gvslvol + hist_gvhlthc + hist_gvcldcr + hist_gvpdlwk


##perform Kolmogorov-Smirnov (KS) test for each of the variable:gvslvol gvhlthc gvcldcr gvpdlwk
ks_gvslvol<-ks.test(x=ESS4_BE$gvslvol,y="pnorm",mean(ESS4_BE$gvslvol, na.rm=TRUE),
                    sd(ESS4_BE$gvslvol,na.rm = TRUE))
ks_gvhlthc<-ks.test(x=ESS4_BE$gvhlthc,y="pnorm",mean(ESS4_BE$gvhlthc,na.rm=TRUE),
                    sd(ESS4_BE$gvhlthc,na.rm = TRUE))
ks_gvcldcr<-ks.test(x=ESS4_BE$gvcldcr,y="pnorm",mean(ESS4_BE$gvcldcr,na.rm=TRUE),
                    sd(ESS4_BE$gvcldcr,na.rm = TRUE))
ks_gvpdlwk<-ks.test(x=ESS4_BE$gvpdlwk, y="pnorm", mean(ESS4_BE$gvpdlwk,na.rm=TRUE),
                    sd(ESS4_BE$gvpdlwk,na.rm=TRUE))

##To bind all the ks test results together for comparison:
ks_statCom<-data.frame(VarName=c("gvslvol","gvhlthc","gvcldcr","gvpdlwk"),
                       ksStat=c(ks_gvslvol$statistic,ks_gvhlthc$statistic,ks_gvcldcr$statistic,ks_gvpdlwk$statistic),
                       pValue=c(ks_gvslvol$p.value,ks_gvhlthc$p.value,ks_gvcldcr$p.value,ks_gvpdlwk$p.value))
ks_statCom2<-data.frame(VarName=c("gvslvol","gvhlthc","gvcldcr","gvpdlwk"),
                       ksStat=c(round(ks_gvslvol$statistic,digits = 2),
                                round(ks_gvhlthc$statistic,digits = 2),
                                round(ks_gvcldcr$statistic,digits = 2),
                                round(ks_gvpdlwk$statistic, digits = 2)),
                       pValue=c(ks_gvslvol$p.value,ks_gvhlthc$p.value,ks_gvcldcr$p.value,ks_gvpdlwk$p.value))

##Henze-Zirkler’s multivariate normality test.
ESS4_BE_4Var<-select(ESS4_BE,gvslvol, gvhlthc, gvcldcr, gvpdlwk)
ESS4_BE_4Var_complete<-na.omit(ESS4_BE_4Var)
HZtest_4var<-mvn(data = ESS4_BE_4Var_complete,
                 mvnTest = "hz")
HZtest_4var$multivariateNormality


##Do the CFA with the 4 variables: gvslvol gvhlthc gvcldcr gvpdlwk
model_welfareSup<-'
welfareSup=~gvslvol+gvhlthc+gvcldcr+gvpdlwk
'

#The usual ML estimates:
fit_welfareSup_ML<-cfa(model = model_welfareSup,
                       data = ESS4_BE,
                       estimator="ML")

#The new MLR estimates: 
#Meijun
fit_welfareSup_MLR<-cfa(model = model_welfareSup,
                       data = ESS4_BE_4Var_complete,
                       estimator="MLR")

#Alberto with MLM instead of MLR:
fit_welfareSup_MLM<-cfa(model = model_welfareSup,
                        data = ESS4_BE_4Var_complete,
                        estimator="MLM")

summary(fit_welfareSup_ML,fit.measures=TRUE,standardized=TRUE)
summary(fit_welfareSup_MLR,fit.measures=TRUE,standardized=TRUE)
summary(fit_welfareSup_MLM,fit.measures=TRUE,standardized=TRUE)
##Question??? difference between estimator MLM and MLR


##Categorical Variables in CFA
##Variables to use: social (sbprvpv, sbeqsoc, sbcwkfm) and moral (sblazy, sblwcoa, sblwlka) 
model_social_moral<-'
SocioCri=~sbprvpv+sbeqsoc+sbcwkfm
MoralCri=~sblazy+sblwcoa+sblwlka
'

fit_social_moral<-cfa(model = model_social_moral,
                      data = ESS4_BE,
                      ordered = c("sbprvpv", "sbeqsoc", "sbcwkfm","sblazy","sblwcoa","sblwlka"),
                      estimator="WLSMV")
summary(fit_social_moral,standardized=TRUE)


##Listwise deletion to casewise (FIML)
##Variables to use: economic (sbstrec,sbbsntx)
##social (sbprvpv, sbeqsoc, sbcwkfm) 
##moral (sblazy, sblwcoa, sblwlka)
model_criticsm<-'
eco_crit=~sbstrec+sbbsntx
social_crit=~sbprvpv+sbeqsoc+sbcwkfm
moral_crit=~sblazy+sblwcoa+sblwlka
'

#LISTWISE
fit_criticism_listwise<-cfa(model = model_criticsm,
                            data = ESS4_BE,
                            missing = "listwise")
summary(fit_criticism_listwise,standardized=TRUE)

#Case-wise
fit_criticism_FIML<-cfa(model = model_criticsm,
                        data = ESS4_BE,
                        missing="direct")
summary(fit_criticism_FIML,standardized=TRUE)

#Case-wise and MLR instead of ML
fit_criticism_FIML_MLR<-cfa(model = model_criticsm,
                            data = ESS4_BE,
                            missing="direct",
                            estimator="MLR")
summary(fit_criticism_FIML_MLR, fit.measures=TRUE, standardized=TRUE)


##Imputation
##Variables to use: economic (sbstrec,sbbsntx)
##social (sbprvpv, sbeqsoc, sbcwkfm) 
##moral (sblazy, sblwcoa, sblwlka)

ESS4_wsCriticism<-select(ESS4_BE,sbstrec,sbbsntx,sbprvpv, sbeqsoc, sbcwkfm,sblazy, sblwcoa, sblwlka)
ESS4_wsCriticism<-as.data.frame(ESS4_wsCriticism)

wsCriticism_imputeOutput<-amelia(ESS4_wsCriticism,
                                 m=15,
                                 seed=23)
summary(wsCriticism_imputeOutput)

##Compare sbstrec before and after imputation
cbind(ESS4_wsCriticism$sbbsntx,wsCriticism_imputeOutput$imputations$imp1$sbbsntx)[c(41:57),]
cbind(ESS4_wsCriticism$sbstrec,wsCriticism_imputeOutput$imputations$imp1$sbstrec)[c(75:85),]

##CFA through semTools
model_criticsm<-'
eco_crit=~sbstrec+sbbsntx
social_crit=~sbprvpv+sbeqsoc+sbcwkfm
moral_crit=~sblazy+sblwcoa+sblwlka
'

fit_criticism_impute<-semTools::runMI(model = model_criticsm,
                                      data = wsCriticism_imputeOutput$imputations,
                                      fun = "cfa",
                                      estimator="MLR")
summary(fit_criticism_impute,fit.measures=TRUE, standardized=TRUE)
