library(car)
library(multcomp)

####################### CUSTOM FUNCTIONS ##########################################################
# I'm going to gift you the following custom functions 
# Makes a nice looking pvalue for "print"

prettyPValue=Vectorize(function(pvalue,digits=4){
  if(pvalue>=0.0001){
    pp=as.character(round(pvalue,digits=digits))
    return(pp)
  }else if(pvalue<.Machine$double.eps){
    expnt=floor(log10(.Machine$double.eps))
    coef=(10^log10(.Machine$double.eps))/(10^expnt)
    coef=round(coef,digits = floor(digits/2))
    pp=paste("<",coef,"E",expnt,sep="")
    return(pp)
  }else{
    expnt=floor(log10(pvalue))
    coef=(10^log10(pvalue))/(10^expnt)
    coef=round(coef,digits = 2)
    pp=paste(coef,"E",expnt,sep="")
    return(pp)
  }
})
### Makes an "asterisk" marker
sigmarker=Vectorize(function(pvalue){
  if(pvalue<0.05 & pvalue>=0.01){
    sigmark="*"
    return(sigmark)
  }else if(pvalue<0.01 & pvalue>=0.001){
    sigmark="**"
    return(sigmark)
  }else if(pvalue<0.001 & pvalue>=1E-4){
    sigmark="***"
    return(sigmark)
  }else if(pvalue<1E-4){
    sigmark="****"
    return(sigmark)
  }else if(pvalue<0.1 &pvalue>=0.05){
    sigmark="."
    return(sigmark)
  }else{
    sigmark="n.s."
  }
})

### In logistic regression, the fit predicted values are the log odds ratio
### log ( avoided/notavoided ). So define a inverse function to get P(avoid)
inv.logit = function(x){
  exp(x)/(1+exp(x))
}