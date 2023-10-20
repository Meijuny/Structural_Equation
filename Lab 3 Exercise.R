##Read the data in from Alberto Github
ESS4_BE<-haven::read_sav("https://github.com/albertostefanelli/SEM_labs/raw/master/data/ESS4_belgium.sav")

##Coerce gender into factor
ESS4_BE$gender<-factor(ESS4_BE$gndr,levels = c("1","2"),
                       labels = c("Male","Female"))

##Ex1
##Q1:Manually specify a configural invariance model for male and female
model_wsConfigure<-'
welfareSup=~gvslvol+gvslvue+gvhlthc
gvslvol~~gvslvol
gvslvue~~gvslvue
gvhlthc~~gvhlthc
gvslvol~1
gvslvue~1
gvhlthc~1
welfareSup~0
'

fit_wsConfigure<-cfa(model = model_wsConfigure, 
                          data = ESS4_BE,
                          group = "gender")
summary(fit_wsConfigure,standardized=TRUE)

##MEIJUN QUESTION????why we need gvslvol~1, gvslvue~1,gvhlthc~1,
##gvslvol~~gvslvol, gvslvue~~gvslvue, gvhlthc~~gvhlthc, welfareSup 
##it's the same results without any of these


##Q2:Manually specify a configural invariance model where you estimate residual covariances for the manifest indicators
##MEIJUN question????Now the model is under-identified?? df=-3
model_wsConfigure_free<-'
welfareSup=~gvslvol+gvslvue+gvhlthc
gvslvol~~gvslvue
gvslvol~~gvhlthc
gvslvue~~gvhlthc
'

fit_wsConfigure_free<-cfa(model = model_wsConfigure_free,
                          data = ESS4_BE,
                          group = "gender")


##Q3:Manually specify a metric invariance model for male and female
model_wsMetric<-'
welfareSup=~gvslvol+gvslvue+gvhlthc
'

fit_wsMetric<-cfa(model = model_wsMetric,
                  data = ESS4_BE,
                  group = "gender",
                  group.equal=c("loadings"))
summary(fit_wsMetric,standardized=TRUE)

##Q4: Manually specify a scalar invariance model for male and female
model_wsScalar<-'
welfareSup=~gvslvol+gvslvue+gvhlthc
'

fit_wsScalar<-cfa(model = model_wsScalar,
                  data = ESS4_BE,
                  group = "gender",
                  group.equal=c("loadings","intercepts"))
summary(fit_wsScalar)

##Q5:Explain differences in Degrees of Freedom for all the fitted models
#Source in function
fitStatCompare<-function(ConfiguralFit,MetricFit,ScalarFit){
        ConfiguralInvariance_Stat<-round(fitMeasures(ConfiguralFit,c("df","cfi","tli",
                                                               "rmsea","rmsea.ci.upper","rmsea.ci.lower","rmsea.pvalue",
                                                               "srmr"),
                                               output="matrix"),digits=3)
        MetricInvariance_Stat<-round(fitMeasures(MetricFit,c("df","cfi","tli",
                                                       "rmsea","rmsea.ci.upper","rmsea.ci.lower","rmsea.pvalue",
                                                       "srmr"),
                                           output="matrix"),digits=3)
        ScalarInvariance_Stat<-round(fitMeasures(ScalarFit,c("df","cfi","tli",
                                                       "rmsea","rmsea.ci.upper","rmsea.ci.lower","rmsea.pvalue",
                                                       "srmr"),
                                           output="matrix"),digits=3)
                Fit_Compare<-cbind(ConfiguralInvariance_Stat,MetricInvariance_Stat,ScalarInvariance_Stat)
        colnames(Fit_Compare)<-c("configural","metric","scalar")
        return(Fit_Compare)
}

#Use this function to get out the fit statistics for comparison
fitStatCompare(ConfiguralFit = fit_wsConfigure, MetricFit = fit_wsMetric, ScalarFit = fit_wsScalar)

##metric invariance: we gain 2 df as the female group loadings do not need to be estimated in var-cov structure
##Scalar invariance: we gain 2 more df as the free parameters for mean structure is: only 4

##Q6:Add gvcldcr and see if the model reaches scalar invariance
#gvcldcr configural equivalence:
model_wsChildcare_Configure<-'
welfareSupNEW=~gvslvol+gvslvue+gvhlthc+gvcldcr
'

fit_wsChildcare_Configure<-cfa(model = model_wsChildcare_Configure,
                               data = ESS4_BE,
                               group = "gender")
summary(fit_wsChildcare_Configure,standardized=TRUE)

##gvcldcr metric equivalence:
model_wsChildcare_metric<-'
welfareSupNEW=~gvslvol+gvslvue+gvhlthc+gvcldcr
'

fit_wsChildcare_metric<-cfa(model = model_wsChildcare_metric,
                            data = ESS4_BE,
                            group = "gender",
                            group.equal=c("loadings"))
summary(fit_wsChildcare_metric,fit.measures=TRUE,standardized=TRUE) 

####RMSEA is 0.052, starting to show problems

##gvcldcr scalar equivalence:
model_wsChildcare_scalar<-'
welfareSupNEW=~gvslvol+gvslvue+gvhlthc+gvcldcr
'

fit_wsChildcare_scalar<-cfa(model = model_wsChildcare_scalar,
                            data = ESS4_BE,
                            group = "gender",
                            group.equal=c("loadings","intercepts"))
summary(fit_wsChildcare_scalar,standardized=TRUE)

##compare fit index:
fitStatCompare(ConfiguralFit = fit_wsChildcare_Configure,
               MetricFit = fit_wsChildcare_metric,
               ScalarFit = fit_wsChildcare_scalar)

##source in chi-square difference test:
ChiSqDifferenceTest_results<-function(ConfiguralFit,MetricFit,ScalarFit){
        ConfiguralVSMetric<-anova(ConfiguralFit,MetricFit)
        MetricVSScalar<-anova(MetricFit,ScalarFit)
        ChiSqResults<-rbind(ConfiguralVSMetric,MetricVSScalar[2,])
        return(ChiSqResults)
}

ChiSqDifferenceTest_results(ConfiguralFit = fit_wsChildcare_Configure,
                            MetricFit = fit_wsChildcare_metric,
                            ScalarFit = fit_wsChildcare_scalar)

##The scalar invariance model worsens significantly, so there is NO full scalar invariance

##Q7:Request modification indices for the gvcldcr scalar model and interpret them (OPTIONAL)
lavTestScore(fit_wsChildcare_scalar)   ##--> the p13 == p27 is the biggest problem
parTable(fit_wsChildcare_scalar)       ##--> the intercept of gvcldcr ~1 needs to be set free

model_partialScalar_wsChildcare<-'
welfareSupNEW=~gvslvol+gvslvue+gvhlthc+gvcldcr
gvcldcr~1
'

fit_partialScalar_wsChildcare<-cfa(model = model_partialScalar_wsChildcare,
                                           data = ESS4_BE,
                                           group = "gender",
                                           group.equal=c("loadings","intercepts"),
                                           group.partial=c(gvcldcr~1))
summary(fit_partialScalar_wsChildcare)

ChiSqDifferenceTest_results(ConfiguralFit = fit_wsChildcare_Configure,
                            MetricFit = fit_wsChildcare_metric,
                            ScalarFit = fit_partialScalar_wsChildcare) ##check if the fit improve

##The results no longer shows a significant chi-square test
##so we can conclude that there is partial scalar invariance

##-----------------------------------------------------------------------------
##Ex2
##Q1:Fit a model where egalitarianism (gincdif, dfincac, smdfslv) mediates 
##the relationship between age (agea), education (eduyrs), income (hinctnta) 
##and welfare support (gvslvol, gvslvue, gvhlthc) in a multigroup model 
##where all paths coefficients are free to vary across group

##Q3:Use lavaan syntax to calculate indirect and direct effects for each predictor in each group. 

model_ws_MultiMediate<-'
welfareSup=~gvslvol+gvslvue+gvhlthc
Egali=~gincdif+dfincac+smdfslv
Egali~c(a11,a21)*agea+c(a12,a22)*eduyrs+c(a13,a23)*hinctnta
welfareSup~c(c11,c21)*agea+c(c12,c22)*eduyrs+c(c13,c23)*hinctnta+c(b1,b2)*Egali
Indirect_age_male:=a11*b1
Indirect_age_female:=a21*b2
Total_age_male:=a11*b1+c11
Total_age_female:=a21*b2+c21
Indirect_edu_male:=a12*b1
Indirect_edu_female:=a22*b2
Total_edu_male:=a12*b1+c12
Total_edu_female:=a22*b2+c22
Indirect_income_male:=a13*b1
Indirect_income_female:=a23*b2
Total_income_male:=a13*b1+c13
Total_income_female:=a23*b2+c23
'

fit_ws_MultiMediate<-cfa(model = model_ws_MultiMediate,
                         data = ESS4_BE,
                         group = "gender")
summary(fit_ws_MultiMediate,standardized=TRUE)

##Indirect_edu_male has bigger negative effect than total effect --> suppression???
##Indirect_edu_female has different sign than total effect --> suppression???
##Indirect_edu_income_male has different sign than total effect --> suppression??

#Q2:Assess if the model fits the data well
MultiMediate_fitStat<-fitMeasures(fit_ws_MultiMediate,c("df","cfi","tli",
                                                        "rmsea","rmsea.ci.upper","rmsea.ci.lower","rmsea.pvalue",
                                                        "srmr"),
                                  output="matrix")


#Q4:Interpret the difference in the path coefficients between male and female
#Male:
#age, edu, and income all have significant effect on Egalitarianism
#Both age and Egalitarianism have significant effect on Welfare support

#Female:
#no indicators have significant effects on Egalitarianism
#Both age and Egalitarianism have significant effect on Welfare Support

#Q5: Assess if the path from Education to Egalitarianism (Path A) can be set equal across the two groups
##Constrain the a12 and a22 to be equal
model_ws_MultiMediate_EDUcons<-'
welfareSup=~gvslvol+gvslvue+gvhlthc
Egali=~gincdif+dfincac+smdfslv
Egali~c(a11,a21)*agea+c(a12,a12)*eduyrs+c(a13,a23)*hinctnta
welfareSup~c(c11,c21)*agea+c(c12,c22)*eduyrs+c(c13,c23)*hinctnta+c(b1,b2)*Egali
Indirect_age_male:=a11*b1
Indirect_age_female:=a21*b2
Total_age_male:=a11*b1+c11
Total_age_female:=a21*b2+c21
Indirect_edu_male:=a12*b1
Indirect_edu_female:=a12*b2
Total_edu_male:=a12*b1+c12
Total_edu_female:=a12*b2+c22
Indirect_income_male:=a13*b1
Indirect_income_female:=a23*b2
Total_income_male:=a13*b1+c13
Total_income_female:=a23*b2+c23
'

fit_ws_MultiMediate_EDUcons<-cfa(model = model_ws_MultiMediate_EDUcons,
                                 data = ESS4_BE,
                                 group = "gender")
summary(fit_ws_MultiMediate_EDUcons,standardized=TRUE)

EDUcons_fitStat<-fitMeasures(fit_ws_MultiMediate_EDUcons,
                             c("df","cfi","tli","rmsea","rmsea.ci.upper",
                               "rmsea.ci.lower","rmsea.pvalue","srmr"),
                             output="matrix")
anova(fit_ws_MultiMediate,fit_ws_MultiMediate_EDUcons)

##We can constrain the path from Edu to Egali to be equal in two groups 
##as the chi-square difference test is not significant

#Q6: Assess if the Indirect effect of Education on Welfare Support (a*b) can be set equal across the two groups (OPTIONAL)
##constrain also Egali to welfareSup to be equal
model_ws_MultiMediate_EDUEgalicons<-'
welfareSup=~gvslvol+gvslvue+gvhlthc
Egali=~gincdif+dfincac+smdfslv
Egali~c(a11,a21)*agea+c(a12,a12)*eduyrs+c(a13,a23)*hinctnta
welfareSup~c(c11,c21)*agea+c(c12,c22)*eduyrs+c(c13,c23)*hinctnta+c(b1,b1)*Egali
Indirect_age_male:=a11*b1
Indirect_age_female:=a21*b1
Total_age_male:=a11*b1+c11
Total_age_female:=a21*b1+c21
Indirect_edu_male:=a12*b1
Indirect_edu_female:=a12*b1
Total_edu_male:=a12*b1+c12
Total_edu_female:=a12*b1+c22
Indirect_income_male:=a13*b1
Indirect_income_female:=a23*b1
Total_income_male:=a13*b1+c13
Total_income_female:=a23*b1+c23
'

fit_ws_MultiMediate_EDUEgalicons<-cfa(model = model_ws_MultiMediate_EDUEgalicons,
                                      data = ESS4_BE,
                                      group = "gender")
summary(fit_ws_MultiMediate_EDUEgalicons,standardized=TRUE)

anova(fit_ws_MultiMediate_EDUEgalicons,fit_ws_MultiMediate_EDUcons)
##The chi-square difference test is not significant, so we can also constrain
##the indirect path from edu-->egali-->welfareSup

##Q7:Think of a method to assess if path coefficients are statistically different using the := operator (OPTIONAL)
model_differenceBetweenGender<-'
welfareSup=~gvslvol+gvslvue+gvhlthc
Egali=~gincdif+dfincac+smdfslv
Egali~c(a11,a21)*agea+c(a12,a22)*eduyrs+c(a13,a23)*hinctnta
welfareSup~c(c11,c21)*agea+c(c12,c22)*eduyrs+c(c13,c23)*hinctnta+c(b1,b2)*Egali
Indirect_age_diff:=a11*b1-a21*b2
Total_age_diff:=a11*b1+c11-(a21*b2+c21)
Indirect_edu_diff:=a12*b1-a22*b2
Total_edu_diff:=a12*b1+c12-(a22*b2+c22)
Indirect_income_diff:=a13*b1-a23*b2
Total_income_diff:=a13*b1+c13-(a23*b2+c23)
'

fit_difference<-cfa(model = model_differenceBetweenGender,
                    data = ESS4_BE,
                    group = "gender")
summary(fit_difference,standardized=TRUE)

##Conclusion: no path coefficients are statistically different between two gender groups