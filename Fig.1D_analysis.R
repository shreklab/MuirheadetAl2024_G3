########### FIG 1D ANALYSIS ###########
## Description:
##    copper avoidance for both nrx and nlg mutants and double mutant 
##

rm(list=ls()) # clean workspace
graphics.off() # clean rstudio graphics
library(readxl) # read XLSX files
library(car) # Companion for Applied Regression, ANOVAs etc.
library(multcomp) # Multiple Comparisons 
library(broom) # Functions that extract tables/clean up output from other functions

setwd("/Users/carolinemuirhead/Desktop/Data/Colab/ R data analysis/Fig. 1/D")
#upload data 

source("customfunctions.R")

datafile="Fig.1D_analysis.xlsx"
outputprefix="Fig.1D_analysis"
jittervalue=0.2 # scatter factor for plot points


d = as.data.frame(read_xlsx(datafile))
d$p.avoid = d$avoided / d$number
##########################SPECIFY FACTOR STRUCTURE############################

d$strain = factor(d$strain,
                  levels=c("N2","TV22998","TV13570", "IV91","VC228","IV936" ))
d$concentration =factor(d$concentration,
                        levels=c("5mM","20mM"))
d$Interaction = interaction(d$strain,d$concentration,sep = ":")
##########################PREP MODEL FIT############################

mdl = glm(cbind(avoided,number-avoided) ~ strain*concentration,
          data=d,family="binomial")
anodev.table = Anova(mdl)

##########################SUMMARY STATISTICS REPORT############################

anodev.table = as.data.frame(anodev.table)
anodev.table$pval2 = prettyPValue(anodev.table$`Pr(>Chisq)`)
anodev.table$sig = sigmarker(anodev.table$`Pr(>Chisq)`)
write.csv(anodev.table,
          file=paste0(outputprefix,"_anodevtable.csv")
)

# Expected Value Table 

exdata = expand.grid(strain=levels(d$strain), 
                     concentration=levels(d$concentration))
rownames(exdata) = levels(d$Interaction)

X = model.matrix(~strain*concentration,exdata)
exdata$expected.value = X %*% coef(mdl)
exdata$SE = diag( X %*% vcov(mdl) %*% t(X) )
exdata$lwr = qnorm(p=0.025, mean = exdata$expected.value , sd =exdata$SE)
exdata$upr = qnorm(p=0.975, mean = exdata$expected.value , sd =exdata$SE)

exdata$p.avoid = inv.logit(exdata$expected.value)
exdata$p.lwr = inv.logit(exdata$lwr)
exdata$p.upr = inv.logit(exdata$upr)
write.csv(exdata,
          file=paste0(outputprefix,"_expectedvaluetable.csv"),
)

##########################POSTHOC TESTS############################
cX = as.data.frame(read_xlsx(paste0(
  outputprefix,"_posthocdesign.xlsx"),sheet = "Sheet1"))
rownames(cX)=cX$comparison
cX=cX[,-1]
cX=as.matrix(cX)

linear.hypotheses = cX %*% X

hypothesis.tests = glht(mdl,linfct=linear.hypotheses)
hypothesis.test.summary = summary(hypothesis.tests)

hypothesis.test.summary = as.data.frame(tidy(hypothesis.test.summary))

hypothesis.test.summary$lwr = qnorm(p=0.025,mean = hypothesis.test.summary$estimate,
                                    sd = hypothesis.test.summary$std.error)

hypothesis.test.summary$upr = qnorm(p=0.975,mean = hypothesis.test.summary$estimate,
                                    sd = hypothesis.test.summary$std.error)

hypothesis.test.summary$raw.p.value = 2*pnorm(abs(hypothesis.test.summary$statistic),
                                              mean = 0,sd=1,lower.tail = FALSE)
## raw.p.value is a Z test
## Z = expected value / standard error (like a t stat)
## Z test p-value is define as the probability of observing a value >=Z if Z is positive
## or <= Z if Z is negative (just like a t test) on the standard normal distribution
## (mean=0, sd=1). 
## The two-tailed test is like the t-test. >= |Z|  * 2. In other words the probability
## that a test statistic is >=Z or <= -Z. For example, suppose Z=10. Then what is the prob
## that we see a value of >=10 on the standard normal + the prob of <= -10. 
## This is all identical to how a t-test is constructed just instead of doing a t-distribution
## it uses a z-score distribution (i.e. the standard normal)


ht.summary2=data.frame(logFC=hypothesis.test.summary$estimate,
                       SE=hypothesis.test.summary$std.error,
                       logFC.lwr=hypothesis.test.summary$lwr,
                       logFC.upr=hypothesis.test.summary$upr,
                       raw.p.value=hypothesis.test.summary$raw.p.value, 
                       raw.p.sig=sigmarker(hypothesis.test.summary$raw.p.value),
                       adj.p.value=hypothesis.test.summary$adj.p.value,
                       adj.p.sig=sigmarker(hypothesis.test.summary$adj.p.value))

rownames(ht.summary2) = hypothesis.test.summary$contrast

write.csv(ht.summary2,file=paste0(outputprefix,"_posthocSummary.csv"))


