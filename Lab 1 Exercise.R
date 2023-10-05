###Ex.1 Local Model Fit
model_ws_6_1<-'welf_supp=~gvslvol+gvslvue+gvhlthc+gvcldcr+gvjbevn+gvpdlwk'
model_ws_6_2<-'welf_supp=~gvslvol+gvslvue+gvhlthc+gvcldcr+gvjbevn+gvpdlwk
gvcldcr~~gvpdlwk
gvslvol ~~ gvhlthc
'

fit_ws_6_1<-cfa(model_ws_6_1,data=Welfare_Support)
fit_ws_6_2<-cfa(model_ws_6_2,data=Welfare_Support)

summary(fit_ws_6_1,standardized=TRUE)
summary(fit_ws_6_2,standardized=TRUE)

#Global fit:
fitMeasures(fit_ws_6_1, c("chisq", "df", "pvalue", "cfi", "tli","rmsea"), output = "matrix")
fitMeasures(fit_ws_6_2, c("chisq", "df", "pvalue", "cfi", "tli","rmsea"), output = "matrix")

#Local fit: 
residuals(fit_ws_6_1)
residuals(fit_ws_6_2)

##chi2 difference test
pchisq(q=(326.956-39.063),df=2,lower.tail = FALSE)
anova(fit_ws_6_1,fit_ws_6_2)

##-----------------------------------------------------------------------
##Ex.2 – Calculate the model-implied covariance matrix
#implied variance-covariance matrix
ObservedCov<-lavInspect(fit_ws_6_1,"sampstat")
ObservedCov
ImpliedCov<-fitted(fit_ws_6_2)
ImpliedCov
CovDifference<-residuals(fit_ws_6_2)
CovDifference

options(scipen = 999)


##-----------------------------------------------------------------------
##Ex.3 – 1-factor- vs 3-factor-model

#one factor
model_wc_1<-'
welfareCri=~sbstrec+sbbsntx+sbprvpv+sbeqsoc+sbcwkfm+sblazy+sblwcoa+sblwlka
'
fit_wc_1<-cfa(model_wc_1,data = welfare_criticism)
summary(fit_wc_1,standardized=TRUE)

#3 factor
model_wc_3<-'
wc_eco=~sbstrec+sbbsntx
wc_socio=~sbprvpv+sbeqsoc+sbcwkfm
wc_moral=~sblazy+sblwcoa+sblwlka
'
fit_wc_3<-cfa(model = model_wc_3,data = welfare_criticism)
summary(fit_wc_3, standardized=TRUE)

#model fit for 1 factor and 3 factors
ModelFit_1<-as.data.frame(fitMeasures(fit_wc_1,c("chisq","df","pvalue","cfi","tli","rmsea"),output="matrix"))
ModelFit_3<-as.data.frame(fitMeasures(fit_wc_3,c("chisq","df","pvalue","cfi","tli","rmsea"),output="matrix"))
FitCompare<-cbind(ModelFit_1,ModelFit_3)
colnames(FitCompare)<-c("1 factor","3 factor")
FitCompare

##-----------------------------------------------------------------------
##Ex.4 – Second order model
model_2order<-'
wc_eco=~sbstrec+sbbsntx
wc_socio=~sbprvpv+sbeqsoc+sbcwkfm
wc_moral=~sblazy+sblwcoa+sblwlka
wc_crit=~wc_eco+wc_socio+wc_moral
'
fit_wc_2order<-cfa(model_2order,data = welfare_criticism)
summary(fit_wc_2order,fit.measures=TRUE,standardized=TRUE)

#MI
mi<-inspect(fit_wc_2order,"mi")
mi.sorted<-mi[order(-mi$mi),]
head(mi.sorted)

##-----------------------------------------------------------------------
##Ex.5 – Compare first-order with second-order model
#Model fit for first-order model
ModelFit_3<-fitMeasures(fit_wc_3,c("chisq","df","pvalue","cfi","tli","rmsea"),output="matrix")

#Model fit for second-order model
ModelFit_2order<-fitMeasures(fit_wc_2order,c("chisq","df","pvalue","cfi","tli","rmsea"),output="matrix")

anova(fit_wc_3,fit_wc_2order)
