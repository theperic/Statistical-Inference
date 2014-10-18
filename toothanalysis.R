tooth<- function(){
  library (datasets)
  library(ggplot2)
  data(ToothGrowth)
  head(ToothGrowth)
  
  qplot(dose, len, data=ToothGrowth, color = dose, facets = .~supp )
  qplot(factor(dose), len, data=ToothGrowth, facets = .~supp, geom="boxplot" )
  ## compares to do
  ## overall mean VC to oj, t testhigh dose oj to VC
  ## Mid does OJ to high VC
  ## low OJ to High OJ
  ##low VC to high VC
  ##dataset<-subset(ToothGrowth, supp=="VC")
  
  p<-t.test(len~supp,paired = FALSE, var.equal=FALSE,data=ToothGrowth)$conf
  print (p)
  
  
  dataset1<-subset(ToothGrowth, supp=="OJ"& dose %in% c(0.5,1))
  dataset2<-subset(ToothGrowth, supp=="OJ"& dose %in% c(0.5,2))
  dataset3<-subset(ToothGrowth, supp=="OJ"& dose %in% c(1,2))
  dataset4<-subset(ToothGrowth, supp=="VC"& dose %in% c(0.5,1))
  dataset5<-subset(ToothGrowth, supp=="VC"& dose %in% c(0.5,2))
  dataset6<-subset(ToothGrowth, supp=="VC"& dose %in% c(1,2))
  dataset7<-subset(ToothGrowth, dose==.5)
  dataset8<-subset(ToothGrowth, dose==1)
  dataset9<-subset(ToothGrowth, dose==2)
  dataset10<-rbind(subset(ToothGrowth, supp=="OJ"& dose==1),
                   subset(ToothGrowth, supp=="VC"& dose==2))
  
  p<-rbind(
    t.test(len~supp,paired = FALSE, var.equal=FALSE,data=ToothGrowth)$conf,
    t.test(len~dose,paired = FALSE, var.equal=FALSE,data=dataset1)$conf,
    t.test(len~dose,paired = FALSE, var.equal=FALSE,data=dataset2)$conf,
    t.test(len~dose,paired = FALSE, var.equal=FALSE,data=dataset3)$conf,
    t.test(len~dose,paired = FALSE, var.equal=FALSE,data=dataset4)$conf,
    t.test(len~dose,paired = FALSE, var.equal=FALSE,data=dataset5)$conf,
    t.test(len~dose,paired = FALSE, var.equal=FALSE,data=dataset6)$conf,
    t.test(len~supp,paired = FALSE, var.equal=FALSE,data=dataset7)$conf,
    t.test(len~supp,paired = FALSE, var.equal=FALSE,data=dataset8)$conf,
    t.test(len~supp,paired = FALSE, var.equal=FALSE,data=dataset9)$conf,
    t.test(len~dose,paired = FALSE, var.equal=FALSE,data=dataset10)$conf,
    t.test(len~dose,paired = FALSE, var.equal=FALSE,data=dataset10,conf.level = 0.9)$conf
  )
  
  ## Checks if zero is in the t confidence range and converts result to text
  result<-p[,1]*p[,2]
  
  for(i in 1:12){
    if (result[i]>0)
      result[i]<-"Statistically different results"
    else
      result[i]<-"No Statistical difference"
  }
  
  ##labels the tests
  tests<-c("OJ vs. VC", "OJ 0.5 vs OJ 1.0","OJ 0.5 vs OJ 2.0","OJ 1.0 vs OJ 2.0",
           "VC 0.5 vs VC 1.0","VC 0.5 vs VC 2.0","VC 1.0 vs VC 2.0", "OJ 0.5 vs VC 0.5",
           "OJ 1.0 vs VC 1.0","OJ 2.0 vs VC 2.0", "OJ 1.0 vs VC 2.0","OJ 1.0 vs VC 2.0 90% test")
  
  ## combine the laels and results into a table
  p<-cbind(tests,p, result)       
  
  print(p)
  
  
}
