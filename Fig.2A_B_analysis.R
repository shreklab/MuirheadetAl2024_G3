########### FIG 2A and B ANALYSIS ###########
## Description:
##    Glycerol avoidance in nu485 (long lof), kur7 (short lof) mutants, 
 #and kur7 short resc
##

rm(list=ls()) # clean workspace
graphics.off() # clean rstudio graphics
library(readxl) # read XLSX files
library(car) # Companion for Applied Regression, ANOVAs etc.
library(multcomp) # Multiple Comparisons 
library(broom) # Functions that extract tables/clean up output from other functions

## set workspace 
setwd("/Users/carolinemuirhead/Desktop/Data/Colab/ R data analysis/Fig. 2/A_B")

source("customfunctions.R")

datafile="Fig.2A_B_analysis.xlsx"
outputprefix="Fig.2A_B_analysis"
jittervalue=0.2 # scatter factor for plot points
jittervalue=0.2 # scatter factor for plot points


d = as.data.frame(read_xlsx(datafile))
d$p.avoid = d$avoided / d$number

dlow <- d[d$concentration=="0.8M",]
dhigh <- d[d$concentration=="1.5M",]

##########################SPECIFY FACTOR STRUCTURE############################

dlow$strain = factor(dlow$strain,
                  levels=c("N2","TV22997","PTK69","IV1066"))

dhigh$strain = factor(dhigh$strain, 
                      levels = c("N2", "TV22997", "PTK69"))

#d$concentration =factor(d$concentration,
 #                       levels=c("0.8M","1.5M"))
#d$Interaction = interaction(d$strain,d$concentration,sep = ":")

##########################PREP MODEL FIT############################

mdl_low = glm(cbind(avoided,number-avoided) ~ strain,
          data=dlow,family="binomial")

mdl_high = glm(cbind(avoided,number-avoided) ~ strain,
               data=dhigh,family="binomial")

anodev.table_low = Anova(mdl_low)
anodev.table_high = Anova(mdl_high)


##########################SUMMARY STATISTICS REPORT############################

#Report for low concentration
anodev.table_low = as.data.frame(anodev.table_low)
anodev.table_low$pval2 = prettyPValue(anodev.table_low$`Pr(>Chisq)`)
anodev.table_low$sig = sigmarker(anodev.table_low$`Pr(>Chisq)`)
write.csv(anodev.table_low,
          file=paste0(outputprefix,"_anodevtable_low.csv")
)

anodev.table_high = as.data.frame(anodev.table_high)
anodev.table_high$pval2 = prettyPValue(anodev.table_high$`Pr(>Chisq)`)
anodev.table_high$sig = sigmarker(anodev.table_high$`Pr(>Chisq)`)
write.csv(anodev.table_high,
          file=paste0(outputprefix,"_anodevtable_high.csv")
)

# Expected Value Table for low concentration 

exdata_low = expand.grid(strain=levels(dlow$strain))
rownames(exdata_low) = levels(dlow$strain)

X_low = model.matrix(~strain,exdata_low)
X_low <- X_low[-c(8), -c(8)]
exdata_low$expected.value = X_low %*% coef(mdl_low)
exdata_low$SE = diag( X_low %*% vcov(mdl_low) %*% t(X_low) )
exdata_low$lwr = qnorm(p=0.025, mean = exdata_low$expected.value , sd =exdata_low$SE)
exdata_low$upr = qnorm(p=0.975, mean = exdata_low$expected.value , sd =exdata_low$SE)

exdata_low$p.avoid = inv.logit(exdata_low$expected.value)
exdata_low$p.lwr = inv.logit(exdata_low$lwr)
exdata_low$p.upr = inv.logit(exdata_low$upr)
write.csv(exdata_low,
          file=paste0(outputprefix,"_expectedvaluetable_low.csv"),
)


# Expected Value Table for high concentration  

exdata_high = expand.grid(strain=levels(dhigh$strain))
rownames(exdata_high) = levels(dhigh$strain)

X_high = model.matrix(~strain,exdata_high)
X_high <- X_high[-c(8), -c(8)]
exdata_high$expected.value = X_high %*% coef(mdl_high)
exdata_high$SE = diag( X_high %*% vcov(mdl_high) %*% t(X_high) )
exdata_high$lwr = qnorm(p=0.025, mean = exdata_high$expected.value , sd =exdata_high$SE)
exdata_high$upr = qnorm(p=0.975, mean = exdata_high$expected.value , sd =exdata_high$SE)

exdata_high$p.avoid = inv.logit(exdata_high$expected.value)
exdata_high$p.lwr = inv.logit(exdata_high$lwr)
exdata_high$p.upr = inv.logit(exdata_high$upr)
write.csv(exdata_high,
          file=paste0(outputprefix,"_expectedvaluetable_high.csv"),
)

##########################POSTHOC TESTS############################
cX = as.data.frame(read_xlsx(paste0(
  outputprefix,"_posthocdesign_low.xlsx"),sheet = "Sheet1"))
rownames(cX)=cX$comparison
cX=cX[,-1]
cX=as.matrix(cX)

linear.hypotheses = cX %*% X_low

hypothesis.tests = glht(mdl_low,linfct=linear.hypotheses)
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

write.csv(ht.summary2,file=paste0(outputprefix,"_posthocSummary_low.csv"))
