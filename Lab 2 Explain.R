##Read the data in
ESS4_BE<-read.csv("ESS-Data-Wizard-subset-2023-10-04.csv")

##select the data needed
ESS4_BE_lab2<-select(ESS4_BE,gvslvol,gvslvue,gvhlthc,gvcldcr,gvjbevn,gvpdlwk,
                     sbstrec,sbbsntx,sbprvpv,sbeqsoc,sbcwkfm,sblazy,sblwcoa,sblwlka,
                     agea,eduyrs,gndr,hinctnta,gincdif,dfincac,smdfslv)
str(ESS4_BE_lab2)

##Request descriptive statistics to see what are the numbers representing missing values
ESS4_BE_lab2_descriptives<-psych::describe(ESS4_BE_lab2)
View(ESS4_BE_lab2_descriptives)

##Giving NA to missing value
ESS4_BE_lab2$gvslvol<-as.numeric(sub(88,NA,ESS4_BE_lab2$gvslvol))
ESS4_BE_lab2$gvslvue<-as.numeric(sub(88,NA,ESS4_BE_lab2$gvslvue))
ESS4_BE_lab2$gvhlthc<-as.numeric(sub(88,NA,ESS4_BE_lab2$gvhlthc))
ESS4_BE_lab2$gvcldcr<-as.numeric(sub(88,NA,ESS4_BE_lab2$gvcldcr))
ESS4_BE_lab2$gvjbevn<-as.numeric(sub(88,NA,ESS4_BE_lab2$gvjbevn))
ESS4_BE_lab2$gvpdlwk<-as.numeric(sub(88,NA,ESS4_BE_lab2$gvpdlwk))

ESS4_BE_lab2$sbstrec<-as.numeric(sub(8,NA,ESS4_BE_lab2$sbstrec))
ESS4_BE_lab2$sbbsntx<-as.numeric(sub(8,NA,ESS4_BE_lab2$sbbsntx))
ESS4_BE_lab2$sbprvpv<-as.numeric(sub(8,NA,ESS4_BE_lab2$sbprvpv))
ESS4_BE_lab2$sbeqsoc<-as.numeric(sub(8,NA,ESS4_BE_lab2$sbeqsoc))
ESS4_BE_lab2$sbcwkfm<-as.numeric(sub(8,NA,ESS4_BE_lab2$sbcwkfm))
ESS4_BE_lab2$sblazy<-as.numeric(sub(8,NA,ESS4_BE_lab2$sblazy))
ESS4_BE_lab2$sblwcoa<-as.numeric(sub(8,NA,ESS4_BE_lab2$sblwcoa))
ESS4_BE_lab2$sblwlka<-as.numeric(sub(8,NA,ESS4_BE_lab2$sblwlka))

ESS4_BE_lab2$eduyrs<-as.numeric(sub(88,NA,ESS4_BE_lab2$eduyrs))

ESS4_BE_lab2$hinctnta<-as.numeric(sub(77,NA,ESS4_BE_lab2$hinctnta))
ESS4_BE_lab2$hinctnta<-as.numeric(sub(88,NA,ESS4_BE_lab2$hinctnta))

ESS4_BE_lab2$gincdif<-as.numeric(sub(7,NA,ESS4_BE_lab2$gincdif))
ESS4_BE_lab2$gincdif<-as.numeric(sub(8,NA,ESS4_BE_lab2$gincdif))
ESS4_BE_lab2$dfincac<-as.numeric(sub(8,NA,ESS4_BE_lab2$dfincac))
ESS4_BE_lab2$smdfslv<-as.numeric(sub(7,NA,ESS4_BE_lab2$smdfslv))
ESS4_BE_lab2$smdfslv<-as.numeric(sub(8,NA,ESS4_BE_lab2$smdfslv))

##To view the descriptive again after handling the NA
ESS4_BE_lab2_descriptives<-psych::describe(ESS4_BE_lab2)

##MIMIC model to test gender and edu effect on welfare support
model_WelSup_mimic<-'
WelfareSup=~gvslvol+gvslvue+gvhlthc
WelfareSup~gndr+eduyrs
'

fit_WelSup_mimic<-cfa(model = model_WelSup_mimic,data = ESS4_BE_lab2)

summary(fit_WelSup_mimic,standardized=TRUE)
summary(fit_WelSup_mimic,fit.measures=TRUE,standardized=TRUE)

##Mediation Model
model_welsup_mediation<-'
WelfareSup=~gvslvol+gvslvue+gvhlthc
Egalitar=~gincdif+dfincac+smdfslv
Egalitar~a*hinctnta
WelfareSup~b*Egalitar
WelfareSup~c*hinctnta
indirect:=a*b
total:=c+a*b
'

fit_welsup_mediation<-cfa(model = model_welsup_mediation,data=ESS4_BE_lab2)

summary(fit_welsup_mediation,standardized=TRUE)

##plotting the mediation model
layout1<-get_layout("gincdif","dfincac","smdfslv","",
                    "","Egalitar","","",
                    "hinctnta","","WelfareSup","",
                    "","gvslvol","gvslvue","gvhlthc",
                    rows=4)

Mediation_plot<-graph_sem(model=fit_welsup_mediation,layout=layout1,angle=170)
Mediation_plot
