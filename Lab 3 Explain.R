##Read the clean data in
ESS4_BE_clean<-haven::read_sav("https://github.com/albertostefanelli/SEM_labs/raw/master/data/ESS4_belgium.sav")
str(ESS4_BE_clean)

##The variables that will be the base of separation of group 
##need to be coerced into factor

ESS4_BE_clean$gndr<-factor(ESS4_BE_clean$gndr,
                              levels=c("1","2"),
                              labels = c("male","female"))

##cfa configural invariance
model_ws_byGender<-'
welfareSup=~gvslvol+gvslvue+gvhlthc
'

fit_wsConfi_byGender<-cfa(model = model_ws_byGender,
                          data = ESS4_BE_clean,
                          group = "gndr")


summary(fit_wsConfi_byGender,standardized=TRUE)

##Compare with the sample mean
tapply(ESS4_BE_clean$gvslvol,ESS4_BE_clean$gndr,mean,na.rm=TRUE)
tapply(ESS4_BE_clean$gvslvue,ESS4_BE_clean$gndr,mean,na.rm=TRUE)
tapply(ESS4_BE_clean$gvhlthc,ESS4_BE_clean$gndr,mean,na.rm=TRUE)


##cfa with metric invariance
fit_wsMetric_byGender<-cfa(model=model_ws_byGender,
                           data = ESS4_BE_clean,
                           group = "gndr",
                           group.equal=c("loadings"))

summary(fit_wsMetric_byGender,standardized=TRUE)

##model fit statistics of configural invariance model and metric invariance model
fitStat_confi_byGender<-fitMeasures(fit_wsConfi_byGender,c("AIC","chisq","df","pvalue","cfi","tli","rmsea"),
                                    output="matrix")
fitStat_metric_byGender<-fitMeasures(fit_wsMetric_byGender,c("AIC","chisq","df","pvalue","cfi","tli","rmsea"),
                                     output="matrix")
fitcompare_confVSmetric<-cbind(fitStat_confi_byGender,fitStat_metric_byGender)


##cfa with scalar invariance
fit_wsScalar_byGender<-cfa(model = model_ws_byGender, data=ESS4_BE_clean,
                           group = "gndr",
                           group.equal=c("loadings","intercepts"))

summary(fit_wsScalar_byGender,standardized=TRUE)

fitStat_scalar_byGender<-fitMeasures(fit_wsScalar_byGender,c("AIC","chisq","df","pvalue","cfi","tli","rmsea"),
                                     output="matrix")

fitcompare_metricVSscalar<-cbind(fitStat_metric_byGender,fitStat_scalar_byGender)

##--------------------------------------------------------------
##Check the MI, doesn't work!!!
MI<-inspect(fitStat_scalar_byGender,"mi")
MIsorted<-mi[order(-mi$mi),]

##---------------------------------------------------------------

##cfa with strict invariance (with all residual variance the same for the same item across groups)
fit_wsStrict_byGender<-cfa(model = model_ws_byGender,data=ESS4_BE_clean,
                           group = "gndr",
                           group.equal=c("loadings","intercepts","residuals"))
summary(fit_wsStrict_byGender,standardized=TRUE)

fitStat_strict_byGender<-fitMeasures(fit_wsStrict_byGender,c("AIC","chisq","df","pvalue","cfi","tli","rmsea"),
                                     output="matrix")
fitcompare_scalarVSstrict<-cbind(fitStat_scalar_byGender,fitStat_strict_byGender)

##cfa with structural invariance (check the heterogeneity between groups)
fit_WSStructure_byGender<-cfa(model = model_ws_byGender,data=ESS4_BE_clean,
                              group = "gndr",
                              group.equal=c("loadings","intercepts","residuals",
                                            "lv.variances","lv.covariances"))
summary(fit_WSStructure_byGender,standardized=TRUE)

##use my own function to get the global fit statistics out:
##code for function see GitHub - Function for MGCFA Fit.R
fitStatCompare(ConfiguralFit = fit_wsConfi_byGender,MetricFit = fit_wsMetric_byGender,
               ScalarFit = fit_wsScalar_byGender,StrictFit = fit_wsStrict_byGender,
               StructuralFit = fit_WSStructure_byGender)

##ANOVA for the model fit 
##code for function see GitHub - Function for MGCFA Fit.R
ChiSqDifferenceTest_results(ConfiguralFit = fit_wsConfi_byGender,
                            MetricFit = fit_wsMetric_byGender,
                            ScalarFit = fit_wsScalar_byGender,
                            StrictFit = fit_wsStrict_byGender,
                            StructuralFit = fit_WSStructure_byGender)

##To check the MI
lavTestScore(fit_wsStrict_byGender)

##Once we see the higher chi-square change, we need to see what the original names are
parTable(fit_wsStrict_byGender)

##To free some parameters:
fit_wsStrict_byGender_free<-cfa(model = model_ws_byGender,data=ESS4_BE_clean,
                           group = "gndr",
                           group.equal=c("loadings","intercepts","residuals"),
                           group.partial=c(gvslvue ~~    gvslvue))
ChiSqDifferenceTest_results(ConfiguralFit = fit_wsConfi_byGender,
                            MetricFit = fit_wsMetric_byGender,
                            ScalarFit = fit_wsScalar_byGender,
                            StrictFit = fit_wsStrict_byGender_free,
                            StructuralFit = fit_WSStructure_byGender)



###------------------------------------------------------------------------
##MGCFA for Egalitarianism:

#Egalitarianism - Configural Equivalence
model_egali_byGender<-'
egali=~gincdif+dfincac+smdfslv
'

fit_egaliConfi_byGender<-cfa(model = model_egali_byGender,
                        data = ESS4_BE_clean,
                        group = "gndr")

summary(fit_egaliConfi_byGender,standardized=TRUE)

##Egalitarianism - Metric Equivalence
fit_egaliMetric_byGender<-cfa(model = model_egali_byGender,
                              data = ESS4_BE_clean,
                              group = "gndr",
                              group.equal=c("loadings"))
summary(fit_egaliMetric_byGender,fit.measures=TRUE,standardized=TRUE)

##Egalitarianism - Scalar Equivalence
fit_egaliScalar_byGender<-cfa(model = model_egali_byGender,
                              data = ESS4_BE_clean,
                              group = "gndr",
                              group.equal=c("loadings","intercepts"))
summary(fit_egaliScalar_byGender,fit.measures=TRUE,standardized=TRUE)

##Egalitarianism measurement has full scalar equivalence across two gender groups
##_-------------------------------------------------------------------------------

##MGSEM
#Define the model for the SEM
model_ws_onEgaliIncome<-'
WelSupp=~gvslvol+gvslvue+gvhlthc
Egali=~gincdif+dfincac+smdfslv
Egali~c(a1,a2)*hinctnta
WelSupp~c(b1,b2)*Egali+c(c1,c2)*hinctnta
IncomeIndirect_Male:=a1*b1
IncomeIndirect_Female:=a2*b2
IncomeTotal_Male:=a1*b1+c1
IncomeTotal_Female:=a2*b2+c2
'

fit_ws_onEgaliIncome<-cfa(model = model_ws_onEgaliIncome,
                          data = ESS4_BE_clean,
                          group = "gndr",
                          group.equal=c("loadings"))

summary(fit_ws_onEgaliIncome, standardized=TRUE)

##???Try to constrain the intercept??Why we don't constrain the intercept???
TEST_fit_ws_onEgaliIncome<-cfa(model = model_ws_onEgaliIncome,
                          data = ESS4_BE_clean,
                          group = "gndr",
                          group.equal=c("loadings","intercepts"))
summary(TEST_fit_ws_onEgaliIncome, standardized=TRUE)
##?????

##Constrain the path coefficient (regression coefficient)
model_constrain_wsOnIncomeEgali<-'
welSupp=~gvslvol+gvslvue+gvhlthc
Egali=~gincdif+dfincac+smdfslv
Egali~c(a1,a1)*hinctnta
welSupp~c(b1,b1)*Egali+c(c1,c1)*hinctnta
IndirectIncome:=a1*b1
TotalIncome:=a1*b1+c1
'

fit_constrain_wsOnIncomeEgali<-cfa(model = model_constrain_wsOnIncomeEgali,
                                   data = ESS4_BE_clean,
                                   group = "gndr",
                                   group.equal=c("loadings"))
summary(fit_constrain_wsOnIncomeEgali,standardized=TRUE)

##Compare the fit between the non-constrain and constrain model
anova(fit_ws_onEgaliIncome,fit_constrain_wsOnIncomeEgali)


##TEST to constrain loadings, intercepts and coefficients
model_constrain_wsOnIncomeEgali<-'
welSupp=~gvslvol+gvslvue+gvhlthc
Egali=~gincdif+dfincac+smdfslv
Egali~c(a1,a1)*hinctnta
welSupp~c(b1,b1)*Egali+c(c1,c1)*hinctnta
IndirectIncome:=a1*b1
TotalIncome:=a1*b1+c1
'
TEST_fit_constrain_wsOnIncomeEgali<-cfa(model = model_constrain_wsOnIncomeEgali,
                                   data = ESS4_BE_clean,
                                   group = "gndr",
                                   group.equal=c("loadings","intercepts"))
summary(TEST_fit_constrain_wsOnIncomeEgali,standardized=TRUE)

