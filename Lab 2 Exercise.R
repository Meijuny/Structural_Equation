##Data preparations were done in Lab2Explain.R
str(ESS4_BE_lab2)

####Ex.1 - MIMIC model
#Q1:Fit the a measurement model with both welfare support and egalitarianism
#Fit welfare support and egalitarianism together in the same model
model_welsup_ega<-'
welSup=~gvslvol+gvslvue+gvhlthc
Egali=~gincdif+dfincac+smdfslv
'

fit_welsup_ega<-cfa(model = model_welsup_ega,data = ESS4_BE_lab2)

summary(fit_welsup_ega,standardized=TRUE)
summary(fit_welsup_ega,fit.measures=TRUE,standardized=TRUE)

#fit welfare support for the first measurement model, then egalitarianism
model_welfareSupport<-'
welfareSup=~gvslvol+gvslvue+gvhlthc
'
fit_welfareSupport<-cfa(model=model_welfareSupport,data=ESS4_BE_lab2)
summary(fit_welfareSupport, fit.measures=TRUE,standardized=TRUE)

model_egalitarian<-'
Egalitarian=~gincdif+dfincac+smdfslv
'
fit_egalitarian<-cfa(model = model_egalitarian,data=ESS4_BE_lab2)
summary(fit_egalitarian,fit.measures=TRUE,standardized=TRUE)

##To compare parameters to the population level
tidy_welsup_ega<-table_results(fit_welsup_ega,
                               columns = c("label","est_sig","se","confint"),
                               digits=3)
tidy_welsup_ega

#Q2:Predict welfare support by adding the gender covariate
model_welsup_ega_gndr<-'
welSup=~gvslvol+gvslvue+gvhlthc
Egali=~gincdif+dfincac+smdfslv
welSup~gndr
'
fit_welsup_ega_gndr<-cfa(model = model_welsup_ega_gndr,data = ESS4_BE_lab2)
summary(fit_welsup_ega_gndr,fit.measures=TRUE,standardized=TRUE)

fitStat_welsup_ega<-as.data.frame(fitMeasures(fit_welsup_ega,
                                c("chisq","df","pvalue","cfi","tli","rmsea"),
                                output="matrix"))
fitStat_welsup_ega_gndr<-as.data.frame(fitMeasures(fit_welsup_ega_gndr,
                                                   c("chisq","df","pvalue","cfi","tli","rmsea"),
                                                   output="matrix"))
fitStat_comp1<-cbind(fitStat_welsup_ega,fitStat_welsup_ega_gndr)
colnames(fitStat_comp1)<-c("without gndr","w/gndr")
fitStat_comp1

##Q3: Add in succession age, income, and education
model_welsup_ega_AllIndVar<-'
welSup=~gvslvol+gvslvue+gvhlthc
Egali=~gincdif+dfincac+smdfslv
welSup~gndr+agea+hinctnta+eduyrs
'
fit_welsup_ega_AllIndVar<-cfa(model = model_welsup_ega_AllIndVar,data = ESS4_BE_lab2)
summary(fit_welsup_ega_AllIndVar,fit.measures=TRUE,standardized=TRUE)


##Q4:Is there a trend for the fit statistics to change (improve or deteriorate) when including covariates?
fitStat_welsup_ega_AllIndVar<-as.data.frame(fitMeasures(fit_welsup_ega_AllIndVar,
                                                        c("chisq","df","pvalue","cfi","tli","rmsea"),
                                                        output="matrix"))
fitStat_comp2<-cbind(fitStat_welsup_ega,fitStat_welsup_ega_gndr,fitStat_welsup_ega_AllIndVar)
colnames(fitStat_comp2)<-c("without IndVar","with only gender","with all IndVar")
fitStat_comp2

##Q5:Calculate the degrees of freedom for each of these models manually
#Only measurement model:
#pieces of info: 9*(9+1)/2=45
#free parameters: 16
#measurement model=4 factor loadings+9 residual variances
#exogenous variables=2 factor variances+1 covariance between factor

#With gender add in:
#pieces of info: 7*(7+1)/2=28
#free parameters: 17
#measurement model=4 factor loadings+9 residual variances
#structural part=1 regression+1 residual factor variance
#exogenous variables = 2 variances

#With all Independent variables in:
#pieces of info: 13*(13+1)/2=91
#free parameters:32
#measurement model=7 factor loadings+9 residual variances
#structural part=4 regression+1 residual factor variance
#exogenous variables = 5 variances (including egali factor)+ 6 covariances ( covariance between egali and welsup should be excluded)

###-----------------------------------------------------------------------------
##Ex2.Mediation Analysis
#Q1:Fit a model where egalitarianism (gincdif, dfincac, smdfslv) mediates the 
##relationship between age (agea), education (eduyrs), gender (gndr), income (hinctnta) 
##and welfare support (gvslvol, gvslvue, gvhlthc)
model_ws_mediation_AllIndVar<-'
welSup=~gvslvol+gvslvue+gvhlthc
Egali=~gincdif+dfincac+smdfslv
welSup~f*agea+g*eduyrs+h*gndr+i*hinctnta
Egali~a*agea+c*eduyrs+d*gndr+e*hinctnta
welSup~b*Egali
indirect_age:=a*b
total:=a*b+f
'

fit_ws_mediation_AllIndVar<-cfa(model = model_ws_mediation_AllIndVar,
                                data = ESS4_BE_lab2)
summary(fit_ws_mediation_AllIndVar,standardized=TRUE)

#Q2:Assess if the model fits the data well
fitStat_mediation<-fitMeasures(fit_ws_mediation_AllIndVar,
                               c("chisq","df","pvalue","cfi","tli","rmsea"),
                               output="matrix")
fitStat_mediation

##Q3: Inspect the regression parameters and the R-squared values of the latent variables
R2<-round(inspect(fit_ws_mediation_AllIndVar,"r2"),digits=3)
R2

##Q7:Does egalitarianism mediates age? Can you interpret this result causally ?
#Age has a direct significant effect on welfare support: 0.005
#The total effect of age on welfare support is 0.006
#Age has extra significant indirect effect on welfare support through egalitarianism 