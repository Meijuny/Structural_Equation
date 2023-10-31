##Read the clean data in:
ESS4_BE<-haven::read_sav("https://github.com/albertostefanelli/SEM_labs/raw/master/data/ESS4_belgium.sav")

##Ex.1
##Q1: Check the multivariate normality for the question 
##gincdif, dfincac, smdfslv (egalitarianism) 
##and gvslvol gvhlthc gvcldcr gvpdlwk (welfare support).

##First to check the visual of the normality with ggplot:
hist_gincdif<-ggplot(data = ESS4_BE,aes(gincdif))+geom_histogram(aes(y=after_stat(density)),
                                                                 binwidth = 1,
                                                                 colour="blue",
                                                                 alpha=0.3)
hist_dfincac<-ggplot(data = ESS4_BE,aes(dfincac))+geom_histogram(aes(y=after_stat(density)),
                                                                 binwidth = 1,
                                                                 colour="blue",
                                                                 alpha=0.3)
hist_smdfslv<-ggplot(data=ESS4_BE,aes(smdfslv))+geom_histogram(aes(y=after_stat(density)),
                                                               binwidth = 1,
                                                               colour="blue",
                                                               alpha=0.3)
hist_gvslvol<-ggplot(data = ESS4_BE,aes(gvslvol))+geom_histogram(aes(y=after_stat(density)),
                                                                 binwidth = 1,
                                                                 colour="black",
                                                                 alpha=0.3)
hist_gvhlthc<-ggplot(data = ESS4_BE,aes(gvhlthc))+geom_histogram(aes(y=after_stat(density)),
                                                                 binwidth = 1,
                                                                 colour="black",
                                                                 alpha=0.3)
hist_gvcldcr<-ggplot(data = ESS4_BE,aes(gvcldcr))+geom_histogram(aes(y=after_stat(density)),
                                                                 binwidth = 1,
                                                                 colour="black",
                                                                 alpha=0.3)
hist_gvpdlwk<-ggplot(data = ESS4_BE,aes(gvpdlwk))+geom_histogram(aes(y=after_stat(density)),
                                                                 binwidth = 1,
                                                                 colour="black",
                                                                 alpha=0.3)

hist_gincdif+hist_dfincac+hist_smdfslv+hist_gvslvol+hist_gvhlthc+hist_gvcldcr+hist_gvpdlwk

##The plot shows that the egalitarianism 3 items are a bit skewed to the right 
##while the welfare support 4 items are a bit skewed to the left

##Do a ks test:
##ks for the egalitarianism 3 items
ks_gincdif<-ks.test(x=ESS4_BE$gincdif,y="pnorm",mean(ESS4_BE$gincdif,na.rm=TRUE),
                    sd(ESS4_BE$gincdif,na.rm=TRUE))
ks_dfincac<-ks.test(x=ESS4_BE$dfincac,y="pnorm",mean(ESS4_BE$dfincac, na.rm=TRUE),
                    sd(ESS4_BE$dfincac,na.rm=TRUE))
ks_smdfslv<-ks.test(x=ESS4_BE$smdfslv,y="pnorm",mean(ESS4_BE$smdfslv,na.rm=TRUE),
                    sd(ESS4_BE$smdfslv,na.rm=TRUE))
ks_egali_compare<-data.frame(varName=c("gincdif","dfincac","smdfslv"),
                             ksStat=round(c(ks_gincdif$statistic,ks_dfincac$statistic,ks_smdfslv$statistic),digits = 3),
                             pValue=round(c(ks_gincdif$p.value,ks_dfincac$p.value,ks_smdfslv$p.value),digits = 3))

##ks for the gvslvol gvhlthc gvcldcr gvpdlwk (welfare support)
ks_gvslvol<-ks.test(x=ESS4_BE$gvslvol,y="pnorm",mean(ESS4_BE$gvslvol,na.rm = TRUE),
                    sd(ESS4_BE$gvslvol,na.rm=TRUE))
ks_gvhlthc<-ks.test(x=ESS4_BE$gvhlthc,y="pnorm",mean(ESS4_BE$gvhlthc,na.rm=TRUE),
                    sd(ESS4_BE$gvhlthc,na.rm=TRUE))
ks_gvcldcr<-ks.test(x=ESS4_BE$gvcldcr,y="pnorm",mean(ESS4_BE$gvcldcr,na.rm = TRUE),
                    sd(ESS4_BE$gvcldcr,na.rm=TRUE))
ks_gvpdlwk<-ks.test(x=ESS4_BE$gvpdlwk, y="pnorm", mean(ESS4_BE$gvpdlwk,na.rm=TRUE),
                    sd(ESS4_BE$gvpdlwk,na.rm=TRUE))
ks_welfaresup_compare<-data.frame(VarName=c("gvslvol","gvhlthc","gvcldcr","gvpdlwk"),
                                  ksStat=round(c(ks_gvslvol$statistic,ks_gvhlthc$statistic,ks_gvcldcr$statistic,ks_gvpdlwk$statistic),digits = 3),
                                  pValue=c(ks_gvslvol$p.value,ks_gvhlthc$p.value,ks_gvcldcr$p.value, ks_gvpdlwk$p.value))

##Doing the HZ MVN test:
ESS4_BE_VarSelected<-select(ESS4_BE,gincdif, dfincac, smdfslv,gvslvol, gvhlthc, gvcldcr, gvpdlwk)
ESS4_BE_VarSelected_complete<-na.omit(ESS4_BE_VarSelected)
HZtest_VarSelected<-mvn(data = ESS4_BE_VarSelected_complete,mvnTest = "hz")
HZtest_VarSelected$multivariateNormality

##The variables are not normally distributed 

##Q2: Estimate a simple CFA (2 latent variable), without any error covariances 
##for egalitarianism (gincdif, dfincac, smdfslv) and welfare support (gvslvol gvhlthc gvcldcr gvpdlwk)
##Q3:Estimate the model using the ML estimation, then re-estimate using MLM.

model_2factor<-'
egali=~gincdif+dfincac+smdfslv
welfareSup=~gvslvol+gvhlthc+gvcldcr+gvpdlwk
'

#ML
fit_2factor_ML<-cfa(model = model_2factor,data = ESS4_BE_VarSelected_complete,
                 estimator="ML")
summary(fit_2factor_ML, fit.measures=TRUE, standardized=TRUE)

#MLM
fit_2factor_MLM<-cfa(model = model_2factor,data = ESS4_BE_VarSelected_complete,
                     estimator="MLR")
summary(fit_2factor_MLM,fit.measures=TRUE,standardized=TRUE)

##How to get the loadings, SE from ML model and MLM model to bind it together?
##ML loadings: underestimate the SE
##Latent Variables:
                  #Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
#egali =~                                                              
        #gincdif   1.000                               0.688    0.648
#dfincac          -0.615    0.059  -10.362    0.000   -0.423   -0.401
#smdfslv           0.870    0.082   10.628    0.000    0.598    0.612
#welfareSup =~                                                         
        #gvslvol   1.000                               1.161    0.785
#gvhlthc           0.912    0.038   23.711    0.000    1.059    0.720
#gvcldcr           0.847    0.043   19.896    0.000    0.983    0.559
#gvpdlwk           0.882    0.043   20.618    0.000    1.024    0.582

##MLM loadings:
#Latent Variables:
                    #Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
#egali =~                                                              
        #gincdif   1.000                               0.688    0.648
#dfincac          -0.615    0.061  -10.002    0.000   -0.423   -0.401
#smdfslv           0.870    0.083   10.437    0.000    0.598    0.612
#welfareSup =~                                                         
        #gvslvol   1.000                               1.161    0.785
#gvhlthc           0.912    0.043   21.043    0.000    1.059    0.720
#gvcldcr           0.847    0.052   16.259    0.000    0.983    0.559
#gvpdlwk           0.882    0.051   17.210    0.000    1.024    0.582


#Q5:Remove the covariance between egalitarianism and welfare support and re-estimate the model. Compare the fit
model_egaliWelfare_noFacCov<-'
egali=~gincdif+dfincac+smdfslv
welfareSup=~gvslvol+gvhlthc+gvcldcr+gvpdlwk
egali~~0*welfareSup
'

fit_egaliWelfare_noFacCov<-cfa(model = model_egaliWelfare_noFacCov,data = ESS4_BE_VarSelected_complete,
                               estimator="MLM")
summary(fit_egaliWelfare_noFacCov,fit.measures=TRUE,standardized=TRUE)


fitStat_2factor<-fitMeasures(fit_2factor_MLM,c("df","cfi","tli",
                                              "rmsea","rmsea.ci.upper","rmsea.ci.lower","rmsea.pvalue",
                                              "srmr"),
                              output="matrix")
fitStat_noFacCov<-fitMeasures(fit_egaliWelfare_noFacCov,c("df","cfi","tli",
                                                "rmsea","rmsea.ci.upper","rmsea.ci.lower","rmsea.pvalue",
                                                "srmr"),
                              output="matrix")

fitStatCompare<-cbind(round(fitStat_2factor,digits=3),round(fitStat_noFacCov,digits = 3))
colnames(fitStatCompare)<-c("withFacCov","NoFacCov")

anova(fit_2factor_MLM,fit_egaliWelfare_noFacCov)
##The deduction of factor covariance makes the model significantly worse

##----------------------------------------------------------------------------------
##Ex2
##Q1: Re-estimate a CFA (2 latent variable) for egalitarianism (gincdif, dfincac, smdfslv) 
##and welfare support (gvslvol gvhlthc gvcldcr gvpdlwk) treating the items as ordinal

model_egaliWelfare_ord<-'
egali=~gincdif+dfincac+smdfslv
welfareSup=~gvslvol+gvhlthc+gvcldcr+gvpdlwk
'

fit_egaliwelfare_ord<-cfa(model = model_egaliWelfare_ord,
                          data = ESS4_BE,
                          ordered = c("gincdif", "dfincac", "smdfslv","gvslvol", "gvhlthc", "gvcldcr", "gvpdlwk"),
                          estimator="WLSMV")
summary(fit_egaliwelfare_ord, standardized=TRUE)

##Q2: Test if the two latent constructs reach scalar invariance between male and feamle 
##(free/fixed factor loading and thresholds).
ESS4_BE$gender<-factor(ESS4_BE$gndr,
                       levels = c("1","2"),
                       labels = c("male","female"))

model_egaliWelfare_metric<-'
egali=~c(L1,L1)*gincdif+c(L2,L2)*dfincac+c(L3,L3)*smdfslv
wc_socia =~ c(L4,L4)*sbprvpv + c(L5,L5)*sbeqsoc + c(L6,L6)*sbcwkfm
'

fit_egaliwelfare_metric<-cfa(model = model_egaliWelfare_metric,
                             data = ESS4_BE,
                             ordered = c("gincdif", "dfincac", "smdfslv","sbprvpv", "sbeqsoc", "sbcwkfm"),
                             estimator="WLSMV",
                             group = "gender")

summary(fit_egaliwelfare_metric,standardized=TRUE)


model_egaliwelfare_scalar<-'
egali=~c(L1,L1)*gincdif+c(L2,L2)*dfincac+c(L3,L3)*smdfslv
wc_socia =~ c(L4,L4)*sbprvpv + c(L5,L5)*sbeqsoc + c(L6,L6)*sbcwkfm
gincdif|c(T1_1,T1_1)*t1
gincdif|c(T1_2,T1_2)*t2
gincdif|c(T1_3,T1_3)*t3
gincdif|c(T1_4,T1_4)*t4
dfincac|c(T2_1,T2_1)*t1 
dfincac|c(T2_2,T2_2)*t2 
dfincac|c(T2_3,T2_3)*t3
dfincac|c(T2_4,T2_4)*t4
smdfslv|c(T3_1,T3_1)*t1
smdfslv|c(T3_2,T3_2)*t2
smdfslv|c(T3_3,T3_3)*t3
smdfslv|c(T3_4,T3_4)*t4
'

fit_egaliwelfare_ord_scalar<-cfa(model = model_egaliwelfare_scalar,
                                 data = ESS4_BE,
                                 ordered = c("gincdif", "dfincac", "smdfslv","sbprvpv", "sbeqsoc", "sbcwkfm"),
                                 estimator="WLSMV",
                                 group = "gender")
summary(fit_egaliwelfare_ord_scalar,standardized=TRUE)

##error message: 
##Error in lav_samplestats_step1(Y = Data, wt = wt, ov.names = ov.names,  : 
##lavaan ERROR: some categories of variable `gvslvol' are empty in group 1; frequencies are [1 3 7 12 31 67 176 286 132 135 0]


##Alberto solution
ESS4_BE$gender<-factor(ESS4_BE$gndr, 
                       levels = c("1","2"),
                       labels = c("male","female"))


model_egaliWelfare_ord<-'
egali=~gincdif+dfincac+smdfslv
wc_socia =~ sbprvpv + sbeqsoc + sbcwkfm
'

fit_egaliwelfare_metricAl<-cfa(model=model_egaliWelfare_ord,
                               data = ESS4_BE,
                               ordered = c("gincdif", "dfincac", "smdfslv","sbprvpv", "sbeqsoc", "sbcwkfm"),
                               estimator="WLSMV",
                               group = "gender",
                               group.equal=c("loadings"))

