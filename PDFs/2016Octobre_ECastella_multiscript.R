library(ade4)
data(aviurba)
aviurba
head(aviurba$fau)
head(aviurba$mil)
head(aviurba$traits)


#### AFC faune
afcfaune<-dudi.coa(aviurba$fau)
s.label(afcfaune$li)
s.label(afcfaune$co,label=aviurba$species.names.fr)

par(mfrow=c(2,2))
s.value(afcfaune$li[,c(1,2)],aviurba$fau$Sp37,cpoint=1,csize=0.5)
s.distri(afcfaune$li[,c(1,2)],aviurba$fau$Sp37,add.plot=T,cellipse=2)
text(-0.8,-1,aviurba$species.names.fr[37],cex=2)

s.value(afcfaune$li[,c(1,2)],aviurba$fau$Sp30,cpoint=1,csize=0.5)
s.distri(afcfaune$li[,c(1,2)],aviurba$fau$Sp30,add.plot=T,cellipse=2)
text(-0.8,-1,aviurba$species.names.fr[30],cex=2)

s.value(afcfaune$li[,c(1,2)],aviurba$fau$Sp15,cpoint=1,csize=0.5)
s.distri(afcfaune$li[,c(1,2)],aviurba$fau$Sp15,add.plot=T,cellipse=2)
text(-0.8,-1,aviurba$species.names.fr[15],cex=2)

s.value(afcfaune$li[,c(1,2)],aviurba$fau$Sp4,cpoint=1,csize=0.5)
s.distri(afcfaune$li[,c(1,2)],aviurba$fau$Sp4,add.plot=T,cellipse=2)
text(-0.8,-1,aviurba$species.names.fr[4],cex=2)

#### ACM milieu
acmmil<-dudi.acm(aviurba$mil,row.w=afcfaune$lw)
s.label(acmmil$li)
scatter(acmmil)

par(mfrow = c(1, 2), mar = c(5, 5, 2, 0), cex = 1.7) 
barplot(acmmil$cr[, 1], horiz = TRUE, xlim = c(0, 1), names.arg = colnames(aviurba$mil),las = 1, main = "F1", col = "lightblue", xlab = "R. de correl.")
barplot(acmmil$cr[, 2], horiz = TRUE, xlim = c(0, 1), names.arg = colnames(aviurba$mil),las = 1, main = "F2", col = "lightblue", xlab = "R. de correl.")

#### coinertie faune x milieu

co1<-coinertia(afcfaune,acmmil)
test1 <- randtest(co1, fixed = 1)
plot(test1)
co1

par(mfrow=c(2,2)) 
s.match(co1$mX,co1$mY)
s.label(co1$co,label=aviurba$species.names.fr,boxes=F)
s.arrow(co1$li)
s.arrow(co1$li[21:28,])
s.corcircle(co1$aX)
s.corcircle(co1$aY)


#### ACM traits
acmtraits<-dudi.acm(aviurba$traits,row.w=afcfaune$cw)
s.label(acmtraits$li,label=aviurba$species.names.fr)
scatter(acmtraits,col = rep(c("black", "red3", "darkblue","darkgreen"),4))

acmtraits$cr
par(mfrow = c(1, 3), mar = c(5, 4.5, 2, 0), cex = 1.7) 
barplot(acmtraits$cr[, 1], horiz = TRUE, xlim = c(0, 1), names.arg = colnames(aviurba$traits),las = 1, main = "F1", col = "lightblue", xlab = "R. de correl.")
barplot(acmtraits$cr[, 2], horiz = TRUE, xlim = c(0, 1), names.arg = colnames(aviurba$traits),las = 1, main = "F2", col = "lightblue", xlab = "R. de correl.")
barplot(acmtraits$cr[, 3], horiz = TRUE, xlim = c(0, 1), names.arg = colnames(aviurba$traits),las = 1, main = "F3", col = "lightblue", xlab = "R. de correl.")


#### coinertie faune x traits
afcfaune2<-dudi.coa(t(aviurba$fau))
acmtraits2<-dudi.acm(aviurba$traits,row.w=afcfaune2$lw)

co2<-coinertia(afcfaune2,acmtraits2)
test2<-randtest(co2,fixed = 1)
plot(test2)
co2

par(mfrow=c(2,2)) 
s.match(co2$mX,co2$mY,clabel=0)
s.match(co2$mX,co2$mY,label=aviurba$species.names.fr)
s.label(co2$co,boxes=F)
s.arrow(co2$li)
s.corcircle(co2$aX)
s.corcircle(co2$aY)

#### RLQ


rlq1<-rlq(acmmil,afcfaune,acmtraits)
plot(rlq1)
sco.distri(rlq1$mR[,1],aviurba$fau,label=aviurba$species.names.fr)
## sites positionnés par le milieu
sco.boxplot(rlq1$mQ[,1],aviurba$traits)
## sp positionnées par les traits
sco.boxplot(rlq1$mR[,1],aviurba$mil)

s.corcircle(rlq1$aR)
s.corcircle(rlq1$aQ)



