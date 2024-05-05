library(metafor)
library(forplo)
library(tidyverse)

##########下面为漏斗图分析###########
# SBP-------------------------------------------------------------------------

outcomes <- read.csv(file = "outputdata/1.csv")
table(outcomes$CPM)
######################
dat1 <- outcomes %>% 
  select(-X) %>% 
  filter(CPM == "SLXM")

dat1 <- escalc(measure="MD", n1i=n1i, n2i=n2i, m1i = m1i, m2i = m2i,
              sd1i = sd1i, sd2i = sd2i, data=dat1, slab=paste(author, year))

res1 <- rma(yi, vi, data=dat1, method="DL")
res1
###########################
dat2 <- outcomes %>% 
  select(-X) %>% 
  filter(CPM == "TMGT")

dat2 <- escalc(measure="MD", n1i=n1i, n2i=n2i, m1i = m1i, m2i = m2i,
               sd1i = sd1i, sd2i = sd2i, data=dat2, slab=paste(author, year))

res2 <- rma(yi, vi, data=dat2, method="DL")
res2
#####################
dat3 <- outcomes %>% 
  select(-X) %>% 
  filter(CPM == "XMT")

dat3 <- escalc(measure="MD", n1i=n1i, n2i=n2i, m1i = m1i, m2i = m2i,
               sd1i = sd1i, sd2i = sd2i, data=dat3, slab=paste(author, year))

res3 <- rma(yi, vi, data=dat3, method="DL")
res3
########################

tiff(file = "outputdata/funnel_SBP.tiff", compression = 'none',
     width = 1500, height = 500, res = 120)
par(mfrow=c(1,3))

funnel(res1, back = "white")
a1 <- regtest(res1, model = "rma")
legend("topright", inset=.02, cex=0.9, bg="white", bty = "n",
       legend=paste0("SLXM ", "p = ", round(a1$pval, digits = 3)))

funnel(res2, back = "white")
a2 <- regtest(res2, model = "rma")
legend("topright", inset=.02, cex=0.9, bg="white", bty = "n",
       legend=paste0("TMGT ", "p = ", round(a2$pval, digits = 3)))

funnel(res3, back = "white")
a3 <- regtest(res3, model = "rma")
legend("topright", inset=.02, cex=0.9, bg="white", bty = "n",
       legend=paste0("XMT ", "p = ", round(a3$pval, digits = 3)))
dev.off()

# DBP ---------------------------------------------------------------------

outcomes <- read.csv(file = "outputdata/2.csv")
table(outcomes$CPM)
#######################
dat1 <- outcomes %>% 
  select(-X) %>% 
  filter(CPM == "SLXM")

dat1 <- escalc(measure="MD", n1i=n1i, n2i=n2i, m1i = m1i, m2i = m2i,
               sd1i = sd1i, sd2i = sd2i, data=dat1, slab=paste(author, year))

res1 <- rma(yi, vi, data=dat1, method="DL")
res1
###########################
dat2 <- outcomes %>% 
  select(-X) %>% 
  filter(CPM == "TMGT")

dat2 <- escalc(measure="MD", n1i=n1i, n2i=n2i, m1i = m1i, m2i = m2i,
               sd1i = sd1i, sd2i = sd2i, data=dat2, slab=paste(author, year))

res2 <- rma(yi, vi, data=dat2, method="DL")
res2
#####################
dat3 <- outcomes %>% 
  select(-X) %>% 
  filter(CPM == "XMT")

dat3 <- escalc(measure="MD", n1i=n1i, n2i=n2i, m1i = m1i, m2i = m2i,
               sd1i = sd1i, sd2i = sd2i, data=dat3, slab=paste(author, year))

res3 <- rma(yi, vi, data=dat3, method="DL")
res3
########################

tiff(file = "outputdata/funnel_DBP.tiff", compression = 'none',
     width = 1500, height = 500, res = 120)
par(mfrow=c(1,3))

funnel(res1, back = "white")
a1 <- regtest(res1, model = "rma")
legend("topright", inset=.02, cex=0.9, bg="white", bty = "n",
       legend=paste0("SLXM ", "p = ", round(a1$pval, digits = 3)))

funnel(res2, back = "white")
a2 <- regtest(res2, model = "rma")
legend("topright", inset=.02, cex=0.9, bg="white", bty = "n",
       legend=paste0("TMGT ", "p = ", round(a2$pval, digits = 3)))

funnel(res3, back = "white")
a3 <- regtest(res3, model = "rma")
legend("topright", inset=.02, cex=0.9, bg="white", bty = "n",
       legend=paste0("XMT ", "p = ", round(a3$pval, digits = 3)))
dev.off()

# 降压有效率 -------------------------------------------------------------------
outcomes <- read.csv(file = "outputdata/3.csv")
table(outcomes$CPM)
##########################
dat1 <- outcomes %>% 
  select(-X) %>% 
  filter(CPM == "SLXM")

dat1 <- escalc(measure="RR", ai=x1i, n1i=n1i, ci=x2i, n2i=n2i, data=dat1,
              slab=paste(author, year))

res1 <- rma(yi, vi, data=dat1, method="EE")
res1
#####################
dat2 <- outcomes %>% 
  select(-X) %>% 
  filter(CPM == "TMGT")

dat2 <- escalc(measure="RR", ai=x1i, n1i=n1i, ci=x2i, n2i=n2i, data=dat2,
               slab=paste(author, year))

res2 <- rma(yi, vi, data=dat2, method="EE")
res2
#############################
dat3 <- outcomes %>% 
  select(-X) %>% 
  filter(CPM == "XMT")

dat3 <- escalc(measure="RR", ai=x1i, n1i=n1i, ci=x2i, n2i=n2i, data=dat3,
               slab=paste(author, year))

res3 <- rma(yi, vi, data=dat3, method="EE")
res3
#########################
tiff(file = "outputdata/funnel_rate.tiff", compression = 'none',
     width = 1500, height = 500, res = 120)
par(mfrow=c(1,3))

funnel(res1, back = "white")
a1 <- regtest(res1, model = "rma")
legend("topright", inset=.02, cex=0.9, bg="white", bty = "n",
       legend=paste0("SLXM ", "p = ", round(a1$pval, digits = 3)))

funnel(res2, back = "white")
a2 <- regtest(res2, model = "rma")
legend("topright", inset=.02, cex=0.9, bg="white", bty = "n",
       legend=paste0("TMGT ", "p = ", round(a2$pval, digits = 3)))

funnel(res3, back = "white")
a3 <- regtest(res3, model = "rma")
legend("topright", inset=.02, cex=0.9, bg="white", bty = "n",
       legend=paste0("XMT ", "p = ", round(a3$pval, digits = 3)))
dev.off()

# 药物不良反应（其他结局指标分药都不够10个研究） ------------------------------------------------------------------
outcomes <- read.csv(file = "outputdata/9.csv")
table(outcomes$CPM)
##################################the END##########################################



# 敏感性分析 -------------------------------------------------------------------
###################
###SBP
outcomes <- read.csv(file = "outputdata/18.csv")
table(outcomes$CPM)
######################为写函数做准备
dat1 <- outcomes %>% 
  select(-X) %>% 
  filter(CPM == "TMGT")

dat1 <- escalc(measure="RR", ai=x1i, n1i=n1i, ci=x2i, n2i=n2i, data=dat1,
              slab=paste(author, year))

res1 <- rma(yi, vi, data=dat1, method="EE")
res1

sen_dat1 <- as.data.frame(leave1out(res1))

min.ci.lb = floor(min(sen_dat1$ci.lb))
max.ci.ub = ceiling(max(sen_dat1$ci.ub))


forplo(sen_dat1[,c(1,5,6)],
       xlim = c(min.ci.lb,max.ci.ub),
       em = "MD",
       ci.edge = FALSE,
       sort = FALSE,
       horiz.bar = TRUE,
       col = 2,
       margin.right = 12,
       size = 3.5,
       title = "QLDX")

###################################
##############写函数##############
con_sensi_plot <- function(i, CP){
  outcomes <- read.csv(file = paste0("outputdata/", i, ".csv"))
  table(outcomes$CPM)
  ######################
  dat <- outcomes %>% 
    select(-X) %>% 
    filter(CPM == CP)
  
  dat <- escalc(measure="MD", n1i=n1i, n2i=n2i, m1i = m1i, m2i = m2i,
                 sd1i = sd1i, sd2i = sd2i, data=dat, slab=paste(author, year))
  
  res <- rma(yi, vi, data=dat, method="DL")
  res
  
  sen_dat <- as.data.frame(leave1out(res))
  
  k <- nrow(sen_dat)
  min.ci.lb = floor(min(sen_dat$ci.lb))
  max.ci.ub = ceiling(max(sen_dat$ci.ub))
  
  forplo(sen_dat[,c(1,5,6)],
         xlim = c(min.ci.lb, max.ci.ub),
         em = "MD",
         ci.edge = TRUE,
         sort = FALSE,
         horiz.bar = TRUE,
         col = 2,
         margin.right = 12,
         size = 3.5,
         title = CP,
         save = T,
         save.path = "outputdata/",
         save.name = paste0(i, "_", CP),
         save.width = 8.5,
         save.height = 0.17*k+2.33)
}

bin_sensi_plot <- function(i, CP){
  outcomes <- read.csv(file = paste0("outputdata/", i, ".csv"))
  table(outcomes$CPM)
  ######################
  dat <- outcomes %>% 
    select(-X) %>% 
    filter(CPM == CP)
  
  dat <- escalc(measure="RR", ai=x1i, n1i=n1i, ci=x2i, n2i=n2i,
                data=dat, slab=paste(author, year))
  
  res <- rma(yi, vi, data=dat, method="EE")
  res
  
  sen_dat <- as.data.frame(leave1out(res))
  
  k <- nrow(sen_dat)
  min.ci.lb = floor(min(sen_dat$ci.lb))
  max.ci.ub = ceiling(max(sen_dat$ci.ub))
  
  forplo(sen_dat[,c(1,5,6)],
         xlim = c(min.ci.lb, max.ci.ub),
         em = "RR",
         ci.edge = TRUE,
         sort = FALSE,
         horiz.bar = TRUE,
         col = 2,
         margin.right = 12,
         size = 3.5,
         title = CP,
         save = T,
         save.path = "outputdata/",
         save.name = paste0(i, "_", CP),
         save.width = 8.5,
         save.height = 0.17*k+2.33)
}
#####################################
con_sensi_plot(1,"QLDX")
con_sensi_plot(1,"SLXM")
con_sensi_plot(1,"TMGT")
con_sensi_plot(1,"XMT")

for (j in c("QLDX","SLXM","TMGT","XMT")) {
  con_sensi_plot(1,j)
}

for (j in c("QLDX","SLXM","TMGT","XMT")) {
  con_sensi_plot(2,j)
}

for (j in c("QLDX","SLXM","TMGT","XMT")) {
  bin_sensi_plot(3,j)
}

for (j in c("SLXM","TMGT","XMT")) {
  bin_sensi_plot(9,j)
}

con_sensi_plot(18,"TMGT")

bin_sensi_plot(9, "SLXM")
