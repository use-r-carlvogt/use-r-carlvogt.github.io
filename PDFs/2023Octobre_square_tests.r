## Ben Meuleman, 2022

### TESTS ON SQUARE CONTINGENCY TABLES
square.tests <- function(V,correct=FALSE) { 

  raw <- unname(as.matrix(V))
  dims <- dim(raw)[1]
  dt <- as.data.frame.table(raw)
  dt$Diag <- factor(cumsum(ifelse(as.character(dt$Var1)==as.character(dt$Var2),1,0))*ifelse(as.character(dt$Var1)==as.character(dt$Var2),1,0))
  dt$Symm <- factor(paste(pmin(as.numeric(dt$Var1),as.numeric(dt$Var2)), pmax(as.numeric(dt$Var1),as.numeric(dt$Var2)), sep = "-"))
  if(correct==TRUE) { dt$Freq <- ifelse(dt$Freq==0,0.0001,dt$Freq) }

  indep <- glm(Freq~Var1+Var2, data=dt, family=poisson)
  qindep <- glm(Freq~Var1+Var2+Diag, data=dt, family=poisson)
  symm <- glm(Freq~Symm, data=dt, family=poisson)
  qsymm <- glm(Freq~Var1+Symm, data=dt, family=poisson)
  mhom <- anova(symm, qsymm, test="Chisq")

  fitted <- list(Independence=matrix(indep$fitted,nrow=dims),Qindependence=matrix(qindep$fitted,nrow=dims),
                 Symmetry=matrix(symm$fitted,nrow=dims),Qsymmetry=matrix(qsymm$fitted,nrow=dims))

  residuals <- list(Independence=matrix(resid(indep,type="pearson"),nrow=dims)/sqrt(1 - lm.influence(indep,do.coef=FALSE)$hat),
                    Qindependence=matrix(resid(qindep,type="pearson"),nrow=dims)/sqrt(1 - lm.influence(qindep,do.coef=FALSE)$hat),
                    Symmetry=matrix(resid(symm,type="pearson"),nrow=dims)/sqrt(1 - lm.influence(symm,do.coef=FALSE)$hat),
                    Qsymmetry=matrix(resid(qsymm,type="pearson"),nrow=dims)/sqrt(1 - lm.influence(qsymm,do.coef=FALSE)$hat))
  diag(residuals$Qindependence) <- 0
  diag(residuals$Symmetry) <- 0
  diag(residuals$Qsymmetry) <- 0

  tests <- data.frame(Test=c("Independence","Quasi-independence","Symmetry","Quasi-symmetry","Marginal homogeneity"),
                    LRT=c(indep$deviance,qindep$deviance,symm$deviance,qsymm$deviance,mhom$Deviance[2]),
                    DF=c(indep$df.residual,qindep$df.residual,symm$df.residual,qsymm$df.residual,mhom$Df[2]))
  tests$P <- 1-pchisq(tests$LRT,tests$DF)
  print(tests)
  invisible(list(Data=dt,Tests=tests,Fitted=fitted,Residuals=residuals))
}
