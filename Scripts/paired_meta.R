library(tidyverse)
library(metafor)
library(forplo)

outcomes <- readxl::read_xlsx(path = 'meta回归数据.xlsx', sheet = 4)
outcomes <- outcomes |> mutate(sample_size = n1i+n2i)


# 连续变量 --------------------------------------------------------------------
dat <- escalc(measure="MD", n1i=n1i, n2i=n2i, m1i = m1i, m2i = m2i,
              sd1i = sd1i, sd2i = sd2i, data=outcomes, slab=paste(" ", author, year))
dat

# 二分类-------------------------------------------------------------------------
dat <- escalc(measure="RR", ai=x1i, n1i=n1i, ci=x2i, n2i=n2i, data=outcomes,
              slab=paste(" ", author, year))
dat

# 调试-------------------------------------------------------------------------
res <- rma(yi, vi, mods = ~ year, data=dat)
res

### draw plot
regplot(res, mod="year", xlab="Time of publication")

### adjust x-axis limits and back-transform to risk ratios
regplot(res, mod="year", xlab="Time of publication", xlim=c(2005,2022))

### add the prediction interval to the plot, add a reference line at 1, and add a legend
regplot(res, mod="year", pi=TRUE, xlab="Time of publication",
        xlim=c(2005,2022), predlim=c(2005,2022), refline=0, legend=TRUE)

### label points outside of the prediction interval
regplot(res, mod="year", pi=TRUE, xlab="Time of publication",
        xlim=c(2005,2022), ylim=c(-40,30), predlim=c(2005,2022), refline=0, legend=T,
        label="piout", labsize=0.8)
### meta-regression #########################################################################

res2 <- rma(yi, vi, mods = ~ year + day + sample_size + CPM + WM, data=dat)
res2

# SBP-------------------------------------------------------------------------
outcomes <- readxl::read_xlsx(path = 'data/meta回归数据.xlsx', sheet = 1)

dat <- escalc(measure="SMD", n1i=n1i, n2i=n2i, m1i = m1i, m2i = m2i,
              sd1i = sd1i, sd2i = sd2i, data=outcomes, slab=paste(author, year))
dat

res <- rma(yi, vi, data=dat)
res
funnel(res, main="Standard Error")
regtest(res, model = "rma")
# ranktest(res)


### cumulative meta-analysis (in the order of publication year)
tmp <- cumul(res, order=year)

### cumulative forest plot
forest(tmp, xlim=c(-3,1), at=c(-2,-1,0,1),
       digits=c(2L,3L), cex=0.8, header="Author and Year")

### forest plot with extra annotations
pdf("SBP.pdf", width = 9, height = 7, family = "GB1")
forest(res,xlim=c(-13,3.5), at=c(-6,-4,-2,0,1), ylim = c(-0,55), order = yi,
       ilab=cbind(m1i, sd1i, n1i, m2i, sd2i, n2i), ilab.xpos=c(-10.7,-9.9,-9.1,-8.3,-7.5,-6.7),
       cex=.55, header="Author and Year", mlab="", showweights = TRUE)
op <- par(cex=.55, font=2)
text(c(-10.7,-9.9,-9.1,-8.3,-7.5,-6.7,1), res$k+2,
     c("Mean", "SD", "Total", "Mean", "SD", "Total", "Weight"))
text(c(-10.4,-8), res$k+4, c("————试验组————", "————对照组————"))
par(op)

### add text with Q-value, dfs, p-value, and I^2 statistic
text(-13, -1, pos=4, cex=0.6, bquote(paste("RE Model (Q = ",
                                            .(formatC(res$QE, digits=2, format="f")), ", df = ", .(res$k - res$p),
                                            ", p = ", .(formatC(res$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                            .(formatC(res$I2, digits=1, format="f")), "%)")))
dev.off()

# DBP ---------------------------------------------------------------------
outcomes <- readxl::read_xlsx(path = 'data/meta回归数据.xlsx', sheet = 2)


dat <- escalc(measure="SMD", n1i=n1i, n2i=n2i, m1i = m1i, m2i = m2i,
              sd1i = sd1i, sd2i = sd2i, data=outcomes, slab=paste(author, year, sep=", "))
dat

res <- rma(yi, vi, data=dat)
res
funnel(res, main="Standard Error")
regtest(res, model = "rma")


### forest plot with extra annotations
pdf("DBP.pdf", width = 9, height = 7, family = "GB1")
forest(res,xlim=c(-13,3.5), at=c(-6,-4,-2,0,1), ylim = c(-0,55), order = yi,
       ilab=cbind(m1i, sd1i, n1i, m2i, sd2i, n2i), ilab.xpos=c(-10.7,-9.9,-9.1,-8.3,-7.5,-6.7),
       cex=.55, header="Author and Year", mlab="", showweights = TRUE)
op <- par(cex=.55, font=2)
text(c(-10.7,-9.9,-9.1,-8.3,-7.5,-6.7,1), res$k+2,
     c("Mean", "SD", "Total", "Mean", "SD", "Total", "Weight"))
text(c(-10.4,-8), res$k+4, c("————试验组————", "————对照组————"))
par(op)

### add text with Q-value, dfs, p-value, and I^2 statistic
text(-13, -1, pos=4, cex=0.6, bquote(paste("RE Model (Q = ",
                                           .(formatC(res$QE, digits=2, format="f")), ", df = ", .(res$k - res$p),
                                           ", p = ", .(formatC(res$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                           .(formatC(res$I2, digits=1, format="f")), "%)")))
dev.off()

# 有效率 ---------------------------------------------------------------------
outcomes <- readxl::read_xlsx(path = 'data/meta回归数据.xlsx', sheet = 3)

dat <- escalc(measure="RR", ai=x1i, n1i=n1i, ci=x2i, n2i=n2i, data=outcomes, slab=paste(author, year, sep=", "))
dat

res <- rma(yi, vi, data=dat)
res
funnel(res, main="Standard Error")
regtest(res, model = "rma")


### forest plot with extra annotations
pdf("降压有效率.pdf", width = 7, height = 7, family = "GB1")

forest(res, atransf=exp, at=log(c(.3, 1, 3)), xlim=c(-8,3),
       ilab=cbind(x1i, n1i, x2i, n2i), ilab.xpos=c(-6,-5,-4,-3),
       cex=.55, header="Author and Year", mlab="", showweights = TRUE, order = -yi)

op <- par(cex=.55, font=2)
text(c(-6,-5,-4,-3,1), res$k+2,
     c("Event", "Total", "Event", "Total", "Weight"))
text(c(-5.7,-3.7), res$k+4, c("———试验组———", "———对照组———"))
par(op)

### add text with Q-value, dfs, p-value, and I^2 statistic
text(-8, -1, pos=4, cex=0.6, bquote(paste("RE Model (Q = ",
                                           .(formatC(res$QE, digits=2, format="f")), ", df = ", .(res$k - res$p),
                                           ", p = ", .(formatC(res$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                           .(formatC(res$I2, digits=1, format="f")), "%)")))
dev.off()

# adverse -----------------------------------------------------------------
outcomes <- readxl::read_xlsx(path = 'data/meta回归数据.xlsx', sheet = 4)

dat <- escalc(measure="RR", ai=x1i, n1i=n1i, ci=x2i, n2i=n2i, data=outcomes, slab=paste(author, year, sep=", "))
dat

res <- rma(yi, vi, data=dat)
res
funnel(res, main="Standard Error")
regtest(res)


### forest plot with extra annotations
pdf("不良反应.pdf", width = 9, height = 7, family = "GB1")

forest(res, atransf=exp, xlim=c(-7,3),at=log(c(.1, 1, 4)),
       ilab=cbind(x1i, n1i, x2i, n2i), ilab.xpos=c(-5.8,-5,-4.2,-3.4),
       cex=.6, header="Author and Year", mlab="", showweights = TRUE, order = yi)

op <- par(cex=.6, font=2)
text(c(-5.8,-5,-4.2,-3.4,1.6), res$k+2,
     c("Event", "Total", "Event", "Total", "Weight"))
text(c(-5.5,-3.9), res$k+3, c("———试验组———", "———对照组———"))
par(op)

### add text with Q-value, dfs, p-value, and I^2 statistic
text(-7, -1, pos=4, cex=0.7, bquote(paste("RE Model (Q = ",
                                          .(formatC(res$QE, digits=2, format="f")), ", df = ", .(res$k - res$p),
                                          ", p = ", .(formatC(res$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                          .(formatC(res$I2, digits=1, format="f")), "%)")))
dev.off()

# 敏感性分析 留一法-------------------------------------------------------------------------
library(forplo)
mydat <- leave1out(res)
mydat <- as.data.frame(mydat)
mydat$weight <- round((dat$n1i+dat$n2i)/sum(dat$n1i+dat$n2i), 2)

forplo(mydat[,c(1,5,6)],
       linreg = FALSE,
       em = "RR",
       left.align = TRUE,
       add.columns=round(mydat[,12], 2),
       add.colnames="Weight", col = 2,
       ci.edge = FALSE,
       # scaledot.by= mydat$weight,
       shade.every=1,
       margin.right =10,
       margin.left = 10,
       sort = FALSE,
       horiz.bar = TRUE,
       xlim = c(-1,0.05))
# segments(-0.6,-10,-0.6,500,lwd=0.8)

# SBP-----------

outcomes <- readxl::read_xlsx(path = 'data/meta回归数据.xlsx', sheet = 1)

dat <- escalc(measure="SMD", n1i=n1i, n2i=n2i, m1i = m1i, m2i = m2i,
              sd1i = sd1i, sd2i = sd2i, data=outcomes, slab=paste(author, year, sep=", "))
dat

res <- rma(yi, vi, data=dat)
res
mydat <- leave1out(res)
mydat <- as.data.frame(mydat)
write.csv(mydat, file = "output/SBP_sensitivity.csv")
mydat$weight <- round((dat$n1i+dat$n2i)/sum(dat$n1i+dat$n2i), 2)
# 
forplo(mydat[,c(1,5,6)],
       linreg = FALSE,
       em = "SMD",
       left.align = TRUE,
       add.columns=round(mydat[,12], 2),
       add.colnames="Weight", col = 2,
       ci.edge = FALSE,
       scaledot.by= mydat$weight,
       shade.every=1,
       margin.right =10,
       margin.left = 10,
       sort = FALSE,
       horiz.bar = TRUE,
       xlim = c(-1.5,0.05))

# ---------------
outcomes <- readxl::read_xlsx(path = 'data/meta回归数据.xlsx', sheet = 2)

dat <- escalc(measure="SMD", n1i=n1i, n2i=n2i, m1i = m1i, m2i = m2i,
              sd1i = sd1i, sd2i = sd2i, data=outcomes, slab=paste(author, year, sep=", "))
dat

res <- rma(yi, vi, data=dat)
res
mydat <- leave1out(res)
mydat <- as.data.frame(mydat)
write.csv(mydat, file = "output/DBP_sensitivity.csv")





# ----------

outcomes <- readxl::read_xlsx(path = 'data/meta回归数据.xlsx', sheet = 3)

dat <- escalc(measure="RR", ai=x1i, n1i=n1i, ci=x2i, n2i=n2i, data=outcomes, slab=paste(author, year, sep=", "))
dat

res <- rma(yi, vi, data=dat)
res
mydat <- leave1out(res)
mydat <- as.data.frame(mydat)
write.csv(mydat, file = "output/rate_sensitivity.csv")
# --------------

outcomes <- readxl::read_xlsx(path = 'data/meta回归数据.xlsx', sheet = 4)

dat <- escalc(measure="RR", ai=x1i, n1i=n1i, ci=x2i, n2i=n2i, data=outcomes, slab=paste(author, year, sep=", "))
dat

res <- rma(yi, vi, data=dat)
res
mydat <- leave1out(res)
mydat <- as.data.frame(mydat)
write.csv(mydat, file = "output/adverse_sensitivity.csv")









