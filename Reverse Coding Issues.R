##Select the variables needed
ESS4_BE_test<-ESS4_BE %>%
        select(eduyrs, imsmetn,imdfetn,impcntr,
               ipmodst, ipbhprp, iprspot,imptrad)

##Handle missing values:
defineNA_7<-function(data){
        output<-data
        for (i in seq_along(names(data))) {
                output[,i]<-as.numeric(sub(7,NA,data[,i]))    
        }
        return(output)
}

ESS4_BE_test[,2:8]<-defineNA_7(ESS4_BE_test[,2:8])

defineNA_8<-function(data){
        output<-data
        for (i in seq_along(names(data))) {
                output[,i]<-as.numeric(sub(8,NA,data[,i]))    
        }
        return(output)
}

ESS4_BE_test[,2:8]<-defineNA_8(ESS4_BE_test[,2:8])

ESS4_BE_test$eduyrs<-as.numeric(sub(88,NA,ESS4_BE_test$eduyrs))

##Human Value - tradition/conformity items (ipmodst, ipbhprp, iprspot,imptrad) 
##in ESS is coded from 1(very much like me) to 6(not like me at all)
##So the idea is to reverse coding in the cfa model

##The below code without reverse works!
model_TRACO_Reject<-'
TRACO=~ipmodst+ipbhprp+iprspot+imptrad
Reject=~imsmetn+imdfetn+impcntr
TRACO~a*eduyrs
Reject~b*eduyrs+c*TRACO
Indirect_edu:=a*b
Total_edu:=a*b+c
'

fit_TRACO_Reject<-cfa(model = model_TRACO_Reject,data = ESS4_BE_test)

summary(fit_TRACO_Reject, standardized=TRUE)

##QUESTION FOR ALBERTO: I tried to use (-1) on the marker variable to reverse TRACO
##But I get the warning message:
##Warning message:
##In lavaan::lavaan(model = model_TRACOreversed_Reject, data = ESS4_BE_test,  :
                          ##lavaan WARNING:
                          ##the optimizer warns that a solution has NOT been found!
##See below.

model_TRACOreversed_Reject<-'
TRACO=~(-1)*ipmodst+ipbhprp+iprspot+imptrad
Reject=~imsmetn+imdfetn+impcntr
TRACO~a*eduyrs
Reject~b*eduyrs+c*TRACO
Indirect_edu:=a*b
Total_edu:=a*b+c
'

fit_TRACOreverse_Reject<-cfa(model = model_TRACOreversed_Reject,data = ESS4_BE_test)

summary(fit_TRACOreversed_Reject, standardized=TRUE)