library ("arm")
library("foreign")
library("readstata13")
library("geepack")
setwd("/Users/jhogan/GoogleDrive/TEACHING/PHP2517-MultilevelModeling/Analyses/")


## This section reads in the ctq data and does some pre-processing
ctq = read.dta13("ctq_mlm.dta")

ctq$include = rep(1, nrow(ctq))
for (i in unique(ctq$id))
{ 
  nmiss                   = sum(is.na( ctq$Y[(ctq$id == i)] ) )
  ctq$include[ctq$id==i]  = rep( ifelse(nmiss<9, 1, 0), 9 )
}

ctq = ctq[(ctq$include==1),]

ctq = ctq[(ctq$week>4),]
ctq$postweek = ctq$week - 5

ctq$totfager.cent = ctq$totfager - mean(ctq$totfager)
ctq$age.cent = ctq$age - mean(ctq$age)

# fit multilevel model
M1 = glmer( Y ~  Z  + totfager.cent + postweek + (1 | id ) , family=binomial, data=ctq)
display(M1)

# plot coefficients and associated probabilities
coefficients = coef(M1)$id
coefficients

alpha.hat    = coefficients[,1]
hist(alpha.hat, nclass=12)

p.hat        = exp(alpha.hat) / (1 + exp(alpha.hat))
hist(p.hat, nclass=12)



# Fit morginal models with GEE using different corr structures

G1.indep = geeglm(Y ~ Z + postweek + totfager.cent, family=binomial("logit"), data=ctq, 
                  id=id, corstr="independence", waves=week)
summary(G1.indep)



G1.exch = geeglm(Y ~ Z + postweek + totfager.cent, family=binomial("logit"), data=ctq, 
                  id=id, corstr="exchangeable", waves=week)
summary(G1.exch)


G1.unst = geeglm(Y ~ Z + postweek + totfager.cent, family=binomial("logit"), data=ctq, 
                 id=id, corstr="unstructured", waves=week)
summary(G1.unst)






