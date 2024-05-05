library(metafor)
# revman example-------------------------------------------------------------------------
### create dataset
dat <- data.frame(author = c("Amore-Coffea", "Deliciozza", "Kahve-Paradiso", "Mama-Kaffa", "Morrocona", "Norscafe", "Oohlahlazza", "Piazza-Allerta"),
                  year   = c(2000, 2004, 2002, 1999, 1998, 1998, 1998, 2003),
                  ai     = c(2, 10, 0, 12, 3, 19, 4, 8),
                  n1i    = c(31, 40, 0, 53, 15, 68, 35, 35),
                  ci     = c(10, 9, 0, 9, 1, 9, 2, 6),
                  n2i    = c(34, 40, 0, 61, 17, 64, 37, 37),
                  rb.a   = c("?", "+", "-", "-", "+", "?", "+", "?"),
                  rb.b   = c("?", "?", "-", "-", "?", "?", "+", "?"),
                  rb.c   = c("?", "?", "?", "?", "?", "+", "+", "?"),
                  rb.d   = c("?", "?", "+", "-", "+", "?", "+", "+"),
                  rb.e   = c("-", "-", "+", "-", "+", "-", "+", "+"),
                  rb.f   = c("+", "+", "+", "+", "+", "-", "+", "+"))

### turn the risk of bias items into factors with levels +, -, and ?
dat[7:12] <- lapply(dat[7:12], factor, levels=c("+", "-", "?"))

### calculate log odds ratios and corresponding sampling variances (and use
### the 'slab' argument to store study labels as part of the data frame)
dat <- escalc(measure="OR", ai=ai, n1i=n1i, ci=ci, n2i=n2i, data=dat,
              slab=paste(author, year), drop00=TRUE)
dat

### note: using drop00=TRUE to leave the log odds ratio missing for the study
### with no events in either group (note the NAs for study 3)

### fit random-effects model (using DL estimator as RevMan does)
res <- rma(yi, vi, data=dat, method="DL")
res

### estimated average odds ratio (and 95% CI/PI)
pred <- predict(res, transf=exp, digits=2)
pred

############################################################################

### need the rounded estimate and CI bounds further below
pred <- formatC(c(pred$pred, pred$ci.lb, pred$ci.ub), format="f", digits=2)

### total number of studies
k <- nrow(dat)

### set na.action to "na.pass" (instead of the default, which is "na.omit"),
### so that even study 3 (with a missing log odds ratio) will be shown in the
### forest plot
options(na.action = "na.pass")

### get the weights and format them as will be used in the forest plot
weights <- paste0(formatC(weights(res), format="f", digits=1), "%")
weights[weights == "NA%"] <- ""

### adjust the margins
par(mar=c(10.8,0,1.3,1.3), mgp=c(3,0.2,0), tcl=-0.2)

### forest plot with extra annotations
sav <- forest(res, atransf=exp, at=log(c(0.01, 0.10, 1, 10, 100)), xlim=c(-30,11),
              xlab="", efac=c(0,4), textpos=c(-30,-4.7), lty=c(1,1,0), refline=NA,
              ilab=cbind(ai, n1i, ci, n2i, weights),
              ilab.xpos=c(-20.6,-18.6,-16.1,-14.1,-10.8), ilab.pos=2,
              cex=0.78, header=c("Study or Subgroup","IV, Random, 95% CI"), mlab="")

### add horizontal line at the top
segments(sav$xlim[1]+0.5, k+1, sav$xlim[2], k+1, lwd=0.8)

### add vertical reference line at 0
segments(0, -2, 0, k+1, lwd=0.8)

### now we add a bunch of text; since some of the text falls outside of the
### plot region, we set xpd=NA so nothing gets clipped
par(xpd=NA)

### adjust cex as used in the forest plot and use a bold font
par(cex=sav$cex, font=2)

text(sav$ilab.xpos, k+2, pos=2, c("Events","Total","Events","Total","Weight"))
text(c(mean(sav$ilab.xpos[1:2]),mean(sav$ilab.xpos[3:4])), k+3, pos=2, c("Caffeine","Decaf"))
text(sav$textpos[2], k+3, "Odds ratio", pos=2)
text(0, k+3, "Odds ratio")
text(sav$xlim[2]-0.6, k+3, "Risk of Bias", pos=2)
text(0, k+2, "IV, Random, 95% CI")
text(c(sav$xlim[1],sav$ilab.xpos[c(2,4,5)]), -1, pos=c(4,2,2,2,2),
     c("Total (95% CI)", sum(dat$n1i), sum(dat$n2i), "100.0%"))
text(sav$xlim[1], -7, pos=4, "Risk of bias legend")

### first hide the non-bold summary estimate text and then add it back in bold font
rect(sav$textpos[2], -1.5, sav$ilab.xpos[5], -0.5, col="white", border=NA)
text(sav$textpos[2], -1, paste0(pred[1], " [", pred[2], ",  ", pred[3], "]"), pos=2)

### use a non-bold font for the rest of the text
par(cex=sav$cex, font=1)

### add 'Favours caffeine'/'Favours decaf' text below the x-axis
text(log(c(0.01, 100)), -4, c("Favours caffeine","Favours decaf"), pos=c(4,2), offset=-0.5)

### add 'Not estimable' for study with missing log odds ratio
text(sav$textpos[2], k+1-which(is.na(dat$yi)), "Not estimable", pos=2)

### add text for total events
text(sav$xlim[1], -2, pos=4, "Total events:")
text(sav$ilab.xpos[c(1,3)], -2, c(sum(dat$ai),sum(dat$ci)), pos=2)

### add text with heterogeneity statistics
text(sav$xlim[1], -3, pos=4, bquote(paste("Heterogeneity: ", "Tau"^2, " = ",
                                          .(formatC(res$tau2, digits=2, format="f")), "; ", "Chi"^2, " = ",
                                          .(formatC(res$QE, digits=2, format="f")), ", df = ", .(res$k - res$p),
                                          " (P = ", .(formatC(res$QEp, digits=2, format="f")), "); ", I^2, " = ",
                                          .(formatC(res$I2, digits=0, format="f")), "%")))

### add text for test of overall effect
text(sav$xlim[1], -4, pos=4, bquote(paste("Test for overall effect: Z = ",
                                          .(formatC(res$zval, digits=2, format="f")),
                                          " (P = ", .(formatC(res$pval, digits=2, format="f")), ")")))

### add text for test of subgroup differences
text(sav$xlim[1], -5, pos=4, bquote(paste("Test for subgroup differences: Not applicable")))

### add risk of bias points and symbols
cols <- c("#00cc00", "#cc0000", "#eeee00")
syms <- levels(dat$rb.a)
pos  <- seq(sav$xlim[2]-5.5,sav$xlim[2]-0.5,length=6)
for (i in 1:6) {
  points(rep(pos[i],k), k:1, pch=19, col=cols[dat[[6+i]]], cex=2.2)
  text(pos[i], k:1, syms[dat[[6+i]]], font=2)
}
text(pos, k+2, c("A","B","C","D","E","F"), font=2)

### add risk of bias legend
text(sav$xlim[1], -8:-13, pos=4, c(
  "(A) Random sequence generation (selection bias)",
  "(B) Allocation concealment (selection bias)",
  "(C) Blinding of participants and personnel (performance bias)",
  "(D) Incomplete outcome data (attrition bias)",
  "(E) Selective reporting (reporting bias)",
  "(F) Other bias"))

# SBP-------------------------------------------------------------------------
library(tidyverse)
library(metafor)

outcomes <- readxl::read_xlsx(path = 'data/meta回归数据.xlsx', sheet = 1)
rob2 <- readxl::read_xlsx(path = 'data/meta回归数据 (2).xlsx', sheet = 7)
outcomes <- outcomes[,c(-9,-10,-11)]
colnames(rob2) <- c("author_year", paste0("rb",".", letters[1:6]))
outcomes <- outcomes %>% mutate(author_year = paste0(author, year))
outcomes <-  left_join(outcomes, rob2, by = "author_year")
dat <- select(outcomes, -author_year)



### turn the risk of bias items into factors with levels +, -, and ?
dat[9:14] <- lapply(dat[9:14], factor, levels=c("+", "-", "?"))

### calculate log odds ratios and corresponding sampling variances (and use
### the 'slab' argument to store study labels as part of the data frame)
dat <- escalc(measure="MD", n1i=n1i, n2i=n2i, m1i = m1i, m2i = m2i,
              sd1i = sd1i, sd2i = sd2i, data=dat, slab=paste(author, year), drop00 = TRUE)


dat

### note: using drop00=TRUE to leave the log odds ratio missing for the study
### with no events in either group (note the NAs for study 3)

### fit random-effects model (using DL estimator as RevMan does)
res <- rma(yi, vi, data=dat, method="DL")
res

### estimated average odds ratio (and 95% CI/PI)
pred <- predict(res, digits=2)
pred

############################################################################

### need the rounded estimate and CI bounds further below
pred <- formatC(c(pred$pred, pred$ci.lb, pred$ci.ub), format="f", digits=2)

### total number of studies
k <- nrow(dat)

### set na.action to "na.pass" (instead of the default, which is "na.omit"),
### so that even study 3 (with a missing log odds ratio) will be shown in the
### forest plot
# options(na.action = "na.pass")

### get the weights and format them as will be used in the forest plot
weights <- paste0(formatC(weights(res), format="f", digits=1), "%")
weights[weights == "NA%"] <- ""

### adjust the margins
par(mar=c(5.8,0,1.3,2.3), mgp=c(3,0.2,0), tcl=-0.2)

pdf("output/revman_SBP.pdf", width = 9.8, height = 7.26, family = "GB1")
### forest plot with extra annotations
sav <- forest(res, at=c(-50,-40,-30,-20,-10,0,10), xlim=c(-180,50),
              xlab="", efac=c(0,4), textpos=c(-180,-51), lty=c(1,1,0), refline=NA,
              ilab=cbind(m1i, sd1i, n1i, m2i, sd2i, n2i, weights),
              ilab.xpos=c(-150,-140,-130,-115,-105,-95,-82), ilab.pos=2,
              cex=0.55, header=c("Author and Year","IV, Random, 95% CI"), mlab="")

### add horizontal line at the top
segments(sav$xlim[1]+0.5, k+1, sav$xlim[2], k+1, lwd=0.8)

### add vertical reference line at 0
segments(0, -4, 0, k+1, lwd=0.8)

### now we add a bunch of text; since some of the text falls outside of the
### plot region, we set xpd=NA so nothing gets clipped
par(xpd=NA)

### adjust cex as used in the forest plot and use a bold font
par(cex=sav$cex, font=2)

text(sav$ilab.xpos, k+2, pos=2, c("Mean", "SD", "Total", "Mean", "SD", "Total","Weight"))
text(c(mean(sav$ilab.xpos[1:3]),mean(sav$ilab.xpos[4:6]))+0.7, k+3, pos=2, c("---试验组---","---对照组---"))
text(sav$textpos[2], k+3, "MD", pos=2)
text(0, k+3, "MD")
text(sav$xlim[2]-2, k+3, "Risk of Bias 2", pos=2)
text(0, k+2, "IV, Random, 95% CI")
text(c(sav$xlim[1],sav$ilab.xpos[c(3,6,7)]), -1, pos=c(4,2,2,2,2),
     c("Total (95% CI)", sum(dat$n1i), sum(dat$n2i), "100.0%"))
text(sav$xlim[1], -7, pos=4, "Risk of bias 2 legend")

### first hide the non-bold summary estimate text and then add it back in bold font
rect(sav$textpos[2], -1.7, sav$ilab.xpos[7], -0.3, col="white", border=NA)
text(sav$textpos[2], -1, paste0(pred[1], " [", pred[2], ",  ", pred[3], "]"), pos=2)

### use a non-bold font for the rest of the text
par(cex=sav$cex, font=1)

### add 'Favours caffeine'/'Favours decaf' text below the x-axis
# text(log(c(0.01, 100)), -4, c("Favours caffeine","Favours decaf"), pos=c(4,2), offset=-0.5)

### add 'Not estimable' for study with missing log odds ratio
text(sav$textpos[2], k+1-which(is.na(dat$yi)), "Not estimable", pos=2)

### add text for total events
# text(sav$xlim[1], -2, pos=4, "Total events:")
# text(sav$ilab.xpos[c(1,4)], -2, c(sum(dat$m1i),sum(dat$m2i)), pos=2)

### add text with heterogeneity statistics
text(sav$xlim[1], -3, pos=4, bquote(paste("Heterogeneity: ", "Tau"^2, " = ",
                                          .(formatC(res$tau2, digits=2, format="f")), "; ", "Chi"^2, " = ",
                                          .(formatC(res$QE, digits=2, format="f")), ", df = ", .(res$k - res$p),
                                          " (P = ", .(formatC(res$QEp, digits=2, format="f")), "); ", I^2, " = ",
                                          .(formatC(res$I2, digits=0, format="f")), "%")))

### add text for test of overall effect
text(sav$xlim[1], -4.5, pos=4, bquote(paste("Test for overall effect: Z = ",
                                          .(formatC(res$zval, digits=2, format="f")),
                                          " (P = ", .(formatC(res$pval, digits=2, format="f")), ")")))

### add text for test of subgroup differences
text(sav$xlim[1], -5.5, pos=4, bquote(paste("Test for subgroup differences: Not applicable")))

### add risk of bias points and symbols
cols <- c("#00cc00", "#cc0000", "#eeee00")
syms <- levels(dat$rb.a)
pos  <- seq(sav$xlim[2]-38,sav$xlim[2]-1,length=6)
for (i in 1:6) {
  points(rep(pos[i],k), k:1, pch=19, col=cols[dat[[8+i]]], cex=2)
  text(pos[i], k:1, syms[dat[[8+i]]], font=2)
}
text(pos, k+2, c("A","B","C","D","E","F"), font=2)

### add risk of bias legend
text(sav$xlim[1], -8:-13, pos=4, c(
  "(A) Randomization process",
  "(B) Deviations from intended interventions",
  "(C) Mising outcome data",
  "(D) Measurement of the outcome",
  "(E) Selection of the reported result",
  "(F) Overall Bias"))
dev.off()

# DBP ---------------------------------------------------------------------

outcomes <- readxl::read_xlsx(path = 'data/meta回归数据.xlsx', sheet = 2)
rob2 <- readxl::read_xlsx(path = 'data/meta回归数据 (2).xlsx', sheet = 7)
outcomes <- outcomes[,c(-9,-10,-11)]
colnames(rob2) <- c("author_year", paste0("rb",".", letters[1:6]))
outcomes <- outcomes %>% mutate(author_year = paste0(author, year))
outcomes <-  left_join(outcomes, rob2, by = "author_year")
dat <- select(outcomes, -author_year)



### turn the risk of bias items into factors with levels +, -, and ?
dat[9:14] <- lapply(dat[9:14], factor, levels=c("+", "-", "?"))

### calculate log odds ratios and corresponding sampling variances (and use
### the 'slab' argument to store study labels as part of the data frame)
dat <- escalc(measure="MD", n1i=n1i, n2i=n2i, m1i = m1i, m2i = m2i,
              sd1i = sd1i, sd2i = sd2i, data=dat, slab=paste(author, year), drop00 = TRUE)


dat

### note: using drop00=TRUE to leave the log odds ratio missing for the study
### with no events in either group (note the NAs for study 3)

### fit random-effects model (using DL estimator as RevMan does)
res <- rma(yi, vi, data=dat, method="DL")
res

### estimated average odds ratio (and 95% CI/PI)
pred <- predict(res, digits=2)
pred

############################################################################

### need the rounded estimate and CI bounds further below
pred <- formatC(c(pred$pred, pred$ci.lb, pred$ci.ub), format="f", digits=2)

### total number of studies
k <- nrow(dat)

### set na.action to "na.pass" (instead of the default, which is "na.omit"),
### so that even study 3 (with a missing log odds ratio) will be shown in the
### forest plot
# options(na.action = "na.pass")

### get the weights and format them as will be used in the forest plot
weights <- paste0(formatC(weights(res), format="f", digits=1), "%")
weights[weights == "NA%"] <- ""

### adjust the margins
par(mar=c(5.8,0,1.3,2.3), mgp=c(3,0.2,0), tcl=-0.2)

pdf("output/revman_DBP.pdf", width = 9.8, height = 7.26, family = "GB1")
### forest plot with extra annotations
sav <- forest(res, at=c(-30,-20,-10,0,10), xlim=c(-160,50),
              xlab="", efac=c(0,4), textpos=c(-160,-31), lty=c(1,1,0), refline=NA,
              ilab=cbind(m1i, sd1i, n1i, m2i, sd2i, n2i, weights),
              ilab.xpos=c(-130,-120,-110,-95,-85,-75,-60), ilab.pos=2,
              cex=0.55, header=c("Author and Year","IV, Random, 95% CI"), mlab="")

### add horizontal line at the top
segments(sav$xlim[1]+0.5, k+1, sav$xlim[2], k+1, lwd=0.8)

### add vertical reference line at 0
segments(0, -4, 0, k+1, lwd=0.8)

### now we add a bunch of text; since some of the text falls outside of the
### plot region, we set xpd=NA so nothing gets clipped
par(xpd=NA)

### adjust cex as used in the forest plot and use a bold font
par(cex=sav$cex, font=2)

text(sav$ilab.xpos, k+2, pos=2, c("Mean", "SD", "Total", "Mean", "SD", "Total","Weight"))
text(c(mean(sav$ilab.xpos[1:3]),mean(sav$ilab.xpos[4:6]))+0.7, k+3, pos=2, c("---试验组---","---对照组---"))
text(sav$textpos[2], k+3, "MD", pos=2)
text(0, k+3, "MD")
text(sav$xlim[2]-2, k+3, "Risk of Bias 2", pos=2)
text(0, k+2, "IV, Random, 95% CI")
text(c(sav$xlim[1],sav$ilab.xpos[c(3,6,7)]), -1, pos=c(4,2,2,2,2),
     c("Total (95% CI)", sum(dat$n1i), sum(dat$n2i), "100.0%"))
text(sav$xlim[1], -7, pos=4, "Risk of bias 2 legend")

### first hide the non-bold summary estimate text and then add it back in bold font
rect(sav$textpos[2], -1.7, sav$ilab.xpos[7], -0.3, col="white", border=NA)
text(sav$textpos[2], -1, paste0(pred[1], " [", pred[2], ",  ", pred[3], "]"), pos=2)

### use a non-bold font for the rest of the text
par(cex=sav$cex, font=1)

### add 'Favours caffeine'/'Favours decaf' text below the x-axis
# text(log(c(0.01, 100)), -4, c("Favours caffeine","Favours decaf"), pos=c(4,2), offset=-0.5)

### add 'Not estimable' for study with missing log odds ratio
text(sav$textpos[2], k+1-which(is.na(dat$yi)), "Not estimable", pos=2)

### add text for total events
# text(sav$xlim[1], -2, pos=4, "Total events:")
# text(sav$ilab.xpos[c(1,4)], -2, c(sum(dat$m1i),sum(dat$m2i)), pos=2)

### add text with heterogeneity statistics
text(sav$xlim[1], -3, pos=4, bquote(paste("Heterogeneity: ", "Tau"^2, " = ",
                                          .(formatC(res$tau2, digits=2, format="f")), "; ", "Chi"^2, " = ",
                                          .(formatC(res$QE, digits=2, format="f")), ", df = ", .(res$k - res$p),
                                          " (P = ", .(formatC(res$QEp, digits=2, format="f")), "); ", I^2, " = ",
                                          .(formatC(res$I2, digits=0, format="f")), "%")))

### add text for test of overall effect
text(sav$xlim[1], -4.5, pos=4, bquote(paste("Test for overall effect: Z = ",
                                            .(formatC(res$zval, digits=2, format="f")),
                                            " (P = ", .(formatC(res$pval, digits=2, format="f")), ")")))

### add text for test of subgroup differences
text(sav$xlim[1], -5.5, pos=4, bquote(paste("Test for subgroup differences: Not applicable")))

### add risk of bias points and symbols
cols <- c("#00cc00", "#cc0000", "#eeee00")
syms <- levels(dat$rb.a)
pos  <- seq(sav$xlim[2]-38,sav$xlim[2]-1,length=6)
for (i in 1:6) {
  points(rep(pos[i],k), k:1, pch=19, col=cols[dat[[8+i]]], cex=2)
  text(pos[i], k:1, syms[dat[[8+i]]], font=2)
}
text(pos, k+2, c("A","B","C","D","E","F"), font=2)

### add risk of bias legend
text(sav$xlim[1], -8:-13, pos=4, c(
  "(A) Randomization process",
  "(B) Deviations from intended interventions",
  "(C) Mising outcome data",
  "(D) Measurement of the outcome",
  "(E) Selection of the reported result",
  "(F) Overall Bias"))
dev.off()

# 降压有效率 -------------------------------------------------------------------

outcomes <- readxl::read_xlsx(path = 'data/meta回归数据.xlsx', sheet = 3)
rob2 <- readxl::read_xlsx(path = 'data/meta回归数据 (2).xlsx', sheet = 7)
outcomes <- outcomes[,c(-7,-8,-9)]
colnames(rob2) <- c("author_year", paste0("rb",".", letters[1:6]))
outcomes <- outcomes %>% mutate(author_year = paste0(author, year))
outcomes <-  left_join(outcomes, rob2, by = "author_year")
outcomes <- select(outcomes, -author_year)
dat <- outcomes


### turn the risk of bias items into factors with levels +, -, and ?
dat[7:12] <- lapply(dat[7:12], factor, levels=c("+", "-", "?"))

### calculate log odds ratios and corresponding sampling variances (and use
### the 'slab' argument to store study labels as part of the data frame)
dat <- escalc(measure="RR", ai=x1i, n1i=n1i, ci=x2i, n2i=n2i, data=dat,
              slab=paste(author, year), drop00 = TRUE)
dat

### note: using drop00=TRUE to leave the log odds ratio missing for the study
### with no events in either group (note the NAs for study 3)

### fit random-effects model (using DL estimator as RevMan does)
res <- rma(yi, vi, data=dat, method="EE")
res

### estimated average odds ratio (and 95% CI/PI)
pred <- predict(res, transf=exp, digits=2) #必须要加exp转换，因为RR默认log
pred

############################################################################

### need the rounded estimate and CI bounds further below
pred <- formatC(c(pred$pred, pred$ci.lb, pred$ci.ub), format="f", digits=2)

### total number of studies
k <- nrow(dat)

### set na.action to "na.pass" (instead of the default, which is "na.omit"),
### so that even study 3 (with a missing log odds ratio) will be shown in the
### forest plot
# options(na.action = "na.pass")

### get the weights and format them as will be used in the forest plot
weights <- paste0(formatC(weights(res), format="f", digits=1), "%")
weights[weights == "NA%"] <- ""

### adjust the margins
par(mar=c(5.8,0,1.3,2.3), mgp=c(3,0.2,0), tcl=-0.2)

pdf("output/revman_rate.pdf", width = 9.13, height = 7.61, family = "GB1")
### forest plot with extra annotations(必须要加exp转换，因为RR默认log)
sav <- forest(res, atransf=exp, at=log(c(.3, 1, 3)), xlim=c(-15,7),
              xlab="", efac=c(0,4), textpos=c(-15,-2), lty=c(1,1,0), refline=NA,
              ilab=cbind(x1i, n1i, x2i, n2i, weights),
              ilab.xpos=c(-11.5,-10,-8,-6.5,-4.5), ilab.pos=2,
              cex=0.55, header=c("Author and Year","IV, Fixed, 95% CI"), mlab="")

### add horizontal line at the top
segments(sav$xlim[1]+0.5, k+1, sav$xlim[2], k+1, lwd=0.8)

### add vertical reference line at 0
segments(0, -4, 0, k+1, lwd=0.8)

### now we add a bunch of text; since some of the text falls outside of the
### plot region, we set xpd=NA so nothing gets clipped
par(xpd=NA)

### adjust cex as used in the forest plot and use a bold font
par(cex=sav$cex, font=2)

text(sav$ilab.xpos, k+2, pos=2, c("Events","Total","Events","Total","Weight"))
text(c(mean(sav$ilab.xpos[1:2]),mean(sav$ilab.xpos[3:4]))+0.2, k+3, pos=2, c("---试验组---","---对照组---"))
text(sav$textpos[2], k+3, "Risk ratio", pos=2)
text(0, k+3, "Risk ratio")
text(sav$xlim[2]-2, k+3, "Risk of Bias 2", pos=2)
text(0, k+2, "IV, Fixed, 95% CI")
text(c(sav$xlim[1],sav$ilab.xpos[c(2,4,5)]), -1, pos=c(4,2,2,2,2),
     c("Total (95% CI)", sum(dat$n1i), sum(dat$n2i), "100.0%"))
text(sav$xlim[1], -7, pos=4, "Risk of bias 2 legend")

### first hide the non-bold summary estimate text and then add it back in bold font
rect(sav$textpos[2], -1.7, sav$ilab.xpos[5], -0.5, col="white", border=NA)
text(sav$textpos[2], -1, paste0(pred[1], " [", pred[2], ",  ", pred[3], "]"), pos=2)

### use a non-bold font for the rest of the text
par(cex=sav$cex, font=1)

### add 'Favours caffeine'/'Favours decaf' text below the x-axis
text(log(1), -8, "Risk ratio (log scale)", pos=3, offset=0.5)

### add 'Not estimable' for study with missing log odds ratio
text(sav$textpos[2], k+1-which(is.na(dat$yi)), "Not estimable", pos=2)

### add text for total events
text(sav$xlim[1], -2, pos=4, "Total events:")
text(sav$ilab.xpos[c(1,3)], -2.2, c(sum(dat$x1i),sum(dat$x2i)), pos=2)

### add text with heterogeneity statistics
text(sav$xlim[1], -3, pos=4, bquote(paste("Heterogeneity: ", "Tau"^2, " = ",
                                          .(formatC(res$tau2, digits=2, format="f")), "; ", "Chi"^2, " = ",
                                          .(formatC(res$QE, digits=2, format="f")), ", df = ", .(res$k - res$p),
                                          " (P = ", .(formatC(res$QEp, digits=2, format="f")), "); ", I^2, " = ",
                                          .(formatC(res$I2, digits=0, format="f")), "%")))

### add text for test of overall effect
text(sav$xlim[1], -4.5, pos=4, bquote(paste("Test for overall effect: Z = ",
                                            .(formatC(res$zval, digits=2, format="f")),
                                            " (P = ", .(formatC(res$pval, digits=2, format="f")), ")")))

### add text for test of subgroup differences
text(sav$xlim[1], -5.5, pos=4, bquote(paste("Test for subgroup differences: Not applicable")))

### add risk of bias points and symbols
cols <- c("#00cc00", "#cc0000", "#eeee00")
syms <- levels(dat$rb.a)
pos  <- seq(sav$xlim[2]-5,sav$xlim[2]-1,length=6)
for (i in 1:6) {
  points(rep(pos[i],k), k:1, pch=19, col=cols[dat[[6+i]]], cex=2)
  text(pos[i], k:1, syms[dat[[6+i]]], font=2)
}
text(pos, k+2, c("A","B","C","D","E","F"), font=2)

### add risk of bias legend
text(sav$xlim[1], -8:-13, pos=4, c(
  "(A) Randomization process",
  "(B) Deviations from intended interventions",
  "(C) Mising outcome data",
  "(D) Measurement of the outcome",
  "(E) Selection of the reported result",
  "(F) Overall Bias"))
dev.off()

# adverse -----------------------------------------------------------------
outcomes <- readxl::read_xlsx(path = 'data/meta回归数据.xlsx', sheet = 4)
rob2 <- readxl::read_xlsx(path = 'data/meta回归数据 (2).xlsx', sheet = 7)
outcomes <- outcomes[,c(-7,-8,-9)]
colnames(rob2) <- c("author_year", paste0("rb",".", letters[1:6]))
outcomes <- outcomes %>% mutate(author_year = paste0(author, year))
outcomes <-  left_join(outcomes, rob2, by = "author_year")
outcomes <- select(outcomes, -author_year)
dat <- outcomes


### turn the risk of bias items into factors with levels +, -, and ?
dat[7:12] <- lapply(dat[7:12], factor, levels=c("+", "-", "?"))

### calculate log odds ratios and corresponding sampling variances (and use
### the 'slab' argument to store study labels as part of the data frame)
dat <- escalc(measure="RR", ai=x1i, n1i=n1i, ci=x2i, n2i=n2i, data=dat,
              slab=paste(author, year), drop00 = TRUE)
dat

### note: using drop00=TRUE to leave the log odds ratio missing for the study
### with no events in either group (note the NAs for study 3)

### fit random-effects model (using DL estimator as RevMan does)
res <- rma(yi, vi, data=dat, method="EE")
res

### estimated average odds ratio (and 95% CI/PI)
pred <- predict(res, transf=exp, digits=2) #必须要加exp转换，因为RR默认log
pred

############################################################################

### need the rounded estimate and CI bounds further below
pred <- formatC(c(pred$pred, pred$ci.lb, pred$ci.ub), format="f", digits=2)

### total number of studies
k <- nrow(dat)

### set na.action to "na.pass" (instead of the default, which is "na.omit"),
### so that even study 3 (with a missing log odds ratio) will be shown in the
### forest plot
options(na.action = "na.pass")

### get the weights and format them as will be used in the forest plot
weights <- paste0(formatC(weights(res), format="f", digits=1), "%")
weights[weights == "NA%"] <- ""

### adjust the margins
par(mar=c(10,0,1.3,2.3), mgp=c(3,0.2,0), tcl=-0.2)

pdf("output/revman_adverse.pdf", width = 8.64, height = 5.84, family = "GB1")
### forest plot with extra annotations(必须要加exp转换，因为RR默认log)
sav <- forest(res, atransf=exp, at=log(c(.01,.1, 1, 4)), xlim=c(-20,7),
              xlab="", efac=c(0,4), textpos=c(-20,-5), lty=c(1,1,0), refline=NA,
              ilab=cbind(x1i, n1i, x2i, n2i, weights),
              ilab.xpos=c(-16,-14.5,-12.5,-11,-9), ilab.pos=2,
              cex=0.7, header=c("Author and Year","IV, Fixed, 95% CI"), mlab="")

### add horizontal line at the top
segments(sav$xlim[1]+0.5, k+1, sav$xlim[2], k+1, lwd=0.8)

### add vertical reference line at 0
segments(0, -4, 0, k+1, lwd=0.8)

### now we add a bunch of text; since some of the text falls outside of the
### plot region, we set xpd=NA so nothing gets clipped
par(xpd=NA)

### adjust cex as used in the forest plot and use a bold font
par(cex=sav$cex-0.04, font=2)

text(sav$ilab.xpos, k+2, pos=2, c("Events","Total","Events","Total","Weight"))
text(c(mean(sav$ilab.xpos[1:2]),mean(sav$ilab.xpos[3:4]))+0.2, k+3, pos=2, c("---试验组---","---对照组---"))
text(sav$textpos[2], k+3, "Risk ratio", pos=2)
text(0, k+3, "Risk ratio")
text(sav$xlim[2]-2, k+3, "Risk of Bias 2", pos=2)
text(0, k+2, "IV, Fixed, 95% CI")
text(c(sav$xlim[1],sav$ilab.xpos[c(2,4,5)]), -1, pos=c(4,2,2,2,2),
     c("Total (95% CI)", sum(dat$n1i), sum(dat$n2i), "100.0%"))
text(sav$xlim[1], -7, pos=4, "Risk of bias 2 legend")

### first hide the non-bold summary estimate text and then add it back in bold font
rect(sav$textpos[2], -1.7, sav$ilab.xpos[5], -0.5, col="white", border=NA)
text(sav$textpos[2], -1, paste0(pred[1], " [", pred[2], ",  ", pred[3], "]"), pos=2)

### use a non-bold font for the rest of the text
par(cex=sav$cex-0.04, font=1)

### add 'Favours caffeine'/'Favours decaf' text below the x-axis
text(log(1), -8, "Risk ratio (log scale)", pos=3, offset=0.5)

### add 'Not estimable' for study with missing log odds ratio
text(sav$textpos[2], k+1-which(is.na(dat$yi)), "Not estimable", pos=2)

### add text for total events
text(sav$xlim[1], -2, pos=4, "Total events:")
text(sav$ilab.xpos[c(1,3)], -2.2, c(sum(dat$x1i),sum(dat$x2i)), pos=2)

### add text with heterogeneity statistics
text(sav$xlim[1], -3, pos=4, bquote(paste("Heterogeneity: ", "Tau"^2, " = ",
                                          .(formatC(res$tau2, digits=2, format="f")), "; ", "Chi"^2, " = ",
                                          .(formatC(res$QE, digits=2, format="f")), ", df = ", .(res$k - res$p),
                                          " (P = ", .(formatC(res$QEp, digits=2, format="f")), "); ", I^2, " = ",
                                          .(formatC(res$I2, digits=0, format="f")), "%")))

### add text for test of overall effect
text(sav$xlim[1], -4.5, pos=4, bquote(paste("Test for overall effect: Z = ",
                                            .(formatC(res$zval, digits=2, format="f")),
                                            " (P = ", .(formatC(res$pval, digits=2, format="f")), ")")))

### add text for test of subgroup differences
text(sav$xlim[1], -5.5, pos=4, bquote(paste("Test for subgroup differences: Not applicable")))

### add risk of bias points and symbols
cols <- c("#00cc00", "#cc0000", "#eeee00")
syms <- levels(dat$rb.a)
pos  <- seq(sav$xlim[2]-5,sav$xlim[2]-1,length=6)
for (i in 1:6) {
  points(rep(pos[i],k), k:1, pch=19, col=cols[dat[[6+i]]], cex=2)
  text(pos[i], k:1, syms[dat[[6+i]]], font=2)
}
text(pos, k+2, c("A","B","C","D","E","F"), font=2)

### add risk of bias legend
text(sav$xlim[1], -8:-13, pos=4, c(
  "(A) Randomization process",
  "(B) Deviations from intended interventions",
  "(C) Mising outcome data",
  "(D) Measurement of the outcome",
  "(E) Selection of the reported result",
  "(F) Overall Bias"))
dev.off()







