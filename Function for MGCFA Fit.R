##Function to view the statistics of different cfa invariance fit altogether
fitStatCompare<-function(ConfiguralFit,MetricFit,ScalarFit,StrictFit,StructuralFit){
        ConfiguralInvariance_Stat<-fitMeasures(ConfiguralFit,c("df","cfi","tli",
                                                     "rmsea","rmsea.ci.upper","rmsea.ci.lower","rmsea.pvalue",
                                                     "srmr"),
                                     output="matrix")
        MetricInvariance_Stat<-fitMeasures(MetricFit,c("df","cfi","tli",
                                                      "rmsea","rmsea.ci.upper","rmsea.ci.lower","rmsea.pvalue",
                                                      "srmr"),
                                           output="matrix")
        ScalarInvariance_Stat<-fitMeasures(ScalarFit,c("df","cfi","tli",
                                                       "rmsea","rmsea.ci.upper","rmsea.ci.lower","rmsea.pvalue",
                                                       "srmr"),
                                           output="matrix")
        StrictInvariance_Stat<-fitMeasures(StrictFit,c("df","cfi","tli",
                                                       "rmsea","rmsea.ci.upper","rmsea.ci.lower","rmsea.pvalue",
                                                       "srmr"),
                                           output="matrix")
        StructuralInvariance_Stat<-fitMeasures(StructuralFit,c("df","cfi","tli",
                                                               "rmsea","rmsea.ci.upper","rmsea.ci.lower","rmsea.pvalue",
                                                               "srmr"),
                                               output="matrix")
        Fit_Compare<-cbind(ConfiguralInvariance_Stat,MetricInvariance_Stat,ScalarInvariance_Stat,
                           StrictInvariance_Stat,StructuralInvariance_Stat)
        colnames(Fit_Compare)<-c("configural","metric","scalar","strict","structural")
        return(Fit_Compare)
}

fitStatCompare(ConfiguralFit = fit_wsConfi_byGender,MetricFit = fit_wsMetric_byGender,
               ScalarFit = fit_wsScalar_byGender,StrictFit = fit_wsStrict_byGender,
               StructuralFit = fit_WSStructure_byGender)


##function for putting the Chi-Square difference test results together
ChiSqDifferenceTest_results<-function(ConfiguralFit,MetricFit,ScalarFit,StrictFit,StructuralFit){
        ConfiguralVSMetric<-anova(ConfiguralFit,MetricFit)
        MetricVSScalar<-anova(MetricFit,ScalarFit)
        ScalarVSStrict<-anova(ScalarFit,StrictFit)
        StrictVSStructure<-anova(StrictFit,StructuralFit)
        ChiSqResults<-rbind(ConfiguralVSMetric,MetricVSScalar[2,],ScalarVSStrict[2,],StrictVSStructure[2,])
        return(ChiSqResults)
}

ChiSqDifferenceTest_results(ConfiguralFit = fit_wsConfi_byGender,
                            MetricFit = fit_wsMetric_byGender,
                            ScalarFit = fit_wsScalar_byGender,
                            StrictFit = fit_wsStrict_byGender,
                            StructuralFit = fit_WSStructure_byGender)