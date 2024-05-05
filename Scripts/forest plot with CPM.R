library(metafor)
library(tidyverse)

# SBP ---------------------------------------------------------------------

outcomes <- readxl::read_xlsx(path = 'data/meta subCPM.xlsx', sheet = 1)

outcomes <- outcomes %>% filter(CPM == "TMGT")

dat <- outcomes

k <- nrow(dat)

dat <- escalc(measure="MD", n1i=n1i, n2i=n2i, m1i = m1i, m2i = m2i,
              sd1i = sd1i, sd2i = sd2i, data=dat, slab=paste(author, year))
dat

res <- rma(yi, vi, data=dat, method="DL")
res
pdf("SBP_TMGT.pdf", width = 9, height = 4.75, family = "GB1")


forest(res, at=c(-50,-40,-30,-20,-10,0,10), xlim=c(-140,30),
       ilab=cbind(m1i, sd1i, n1i, m2i, sd2i, n2i), ilab.xpos=c(-110,-100,-90,-75,-65,-55),
       cex=.75, header="Author(s) and Year", mlab="")

op <- par(cex=0.7, font=2)

text(c(-106,-96,-86,-71,-61,-51), k+2, pos=2, c("Mean", "SD", "Total", "Mean", "SD", "Total"))
text(c(-90,-59), k+2.7, pos=2, c("天麻钩藤颗粒+西药","西药"))

### switch to bold italic font
par(font=4)

text(-140, -1, pos=4, cex=0.9, bquote(paste("DL Model (Q = ",
                                              .(formatC(res$QE, digits=2, format="f")), ", df = ", .(res$k - res$p),
                                              ", p = ", .(formatC(res$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                              .(formatC(res$I2, digits=1, format="f")), "%)")))
dev.off()




###################################### SLXM
outcomes <- readxl::read_xlsx(path = 'data/meta subCPM.xlsx', sheet = 1)

outcomes <- outcomes %>% filter(CPM == "SLXM")

dat <- outcomes

k <- nrow(dat)

dat <- escalc(measure="MD", n1i=n1i, n2i=n2i, m1i = m1i, m2i = m2i,
              sd1i = sd1i, sd2i = sd2i, data=dat, slab=paste(author, year))
dat

res <- rma(yi, vi, data=dat, method="DL")
res
pdf("output1/SBP_SLXM.pdf", width = 9, height = 5.5, family = "GB1")
forest(res, at=c(-50,-40,-30,-20,-10,0,10), xlim=c(-140,30),
       ilab=cbind(m1i, sd1i, n1i, m2i, sd2i, n2i), ilab.xpos=c(-110,-100,-90,-75,-65,-55),
       cex=.75, header="Author(s) and Year", mlab="")

op <- par(cex=0.7, font=2)

text(c(-106,-96,-86,-71,-61,-51), k+2, pos=2, c("Mean", "SD", "Total", "Mean", "SD", "Total"))
text(c(-90,-59), k+3, pos=2, c("松龄血脉康胶囊+西药","西药"))

### switch to bold italic font
par(font=4)

text(-140, -1, pos=4, cex=0.9, bquote(paste("DL Model (Q = ",
                                            .(formatC(res$QE, digits=2, format="f")), ", df = ", .(res$k - res$p),
                                            ", p = ", .(formatC(res$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                            .(formatC(res$I2, digits=1, format="f")), "%)")))
dev.off()
#########################################QLDX
outcomes <- readxl::read_xlsx(path = 'data/meta subCPM.xlsx', sheet = 1)

outcomes <- outcomes %>% filter(CPM == "QLDX")

dat <- outcomes

k <- nrow(dat)

dat <- escalc(measure="MD", n1i=n1i, n2i=n2i, m1i = m1i, m2i = m2i,
              sd1i = sd1i, sd2i = sd2i, data=dat, slab=paste(author, year))
dat

res <- rma(yi, vi, data=dat, method="DL")
res
pdf("output1/SBP_QLDX.pdf", width = 8.97, height = 3, family = "GB1")
forest(res, at=c(-50,-40,-30,-20,-10,0,10), xlim=c(-140,30),
       ilab=cbind(m1i, sd1i, n1i, m2i, sd2i, n2i), ilab.xpos=c(-110,-100,-90,-75,-65,-55),
       cex=.75, header="Author(s) and Year", mlab="")

op <- par(cex=0.7, font=2)

text(c(-106,-96,-86,-71,-61,-51), k+2, pos=2, c("Mean", "SD", "Total", "Mean", "SD", "Total"))
text(c(-90,-59), k+3, pos=2, c("强力定眩片+西药","西药"))

### switch to bold italic font
par(font=4)

text(-140, -1, pos=4, cex=0.9, bquote(paste("DL Model (Q = ",
                                            .(formatC(res$QE, digits=2, format="f")), ", df = ", .(res$k - res$p),
                                            ", p = ", .(formatC(res$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                            .(formatC(res$I2, digits=1, format="f")), "%)")))
dev.off()
##################################XMT
outcomes <- readxl::read_xlsx(path = 'data/meta subCPM.xlsx', sheet = 1)

outcomes <- outcomes %>% filter(CPM == "XMT")

dat <- outcomes

k <- nrow(dat)

dat <- escalc(measure="MD", n1i=n1i, n2i=n2i, m1i = m1i, m2i = m2i,
              sd1i = sd1i, sd2i = sd2i, data=dat, slab=paste(author, year))
dat

res <- rma(yi, vi, data=dat, method="DL")
res
pdf("output1/SBP_XMT.pdf", width = 8.97, height = 4, family = "GB1")
forest(res, at=c(-50,-40,-30,-20,-10,0,10), xlim=c(-140,30),
       ilab=cbind(m1i, sd1i, n1i, m2i, sd2i, n2i), ilab.xpos=c(-110,-100,-90,-75,-65,-55),
       cex=.75, header="Author(s) and Year", mlab="")

op <- par(cex=0.7, font=2)

text(c(-106,-96,-86,-71,-61,-51), k+2, pos=2, c("Mean", "SD", "Total", "Mean", "SD", "Total"))
text(c(-90,-59), k+3, pos=2, c("心脉通胶囊+西药","西药"))

### switch to bold italic font
par(font=4)

text(-140, -1, pos=4, cex=0.9, bquote(paste("DL Model (Q = ",
                                            .(formatC(res$QE, digits=2, format="f")), ", df = ", .(res$k - res$p),
                                            ", p = ", .(formatC(res$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                            .(formatC(res$I2, digits=1, format="f")), "%)")))
dev.off()
############################################QGJY
outcomes <- readxl::read_xlsx(path = 'data/meta subCPM.xlsx', sheet = 1)

outcomes <- outcomes %>% filter(CPM == "QGJY")

dat <- outcomes

k <- nrow(dat)

dat <- escalc(measure="MD", n1i=n1i, n2i=n2i, m1i = m1i, m2i = m2i,
              sd1i = sd1i, sd2i = sd2i, data=dat, slab=paste(author, year))
dat

res <- rma(yi, vi, data=dat, method="DL")
res
pdf("output1/SBP_QGJY.pdf", width = 8.97, height = 2.5, family = "GB1")
forest(res, at=c(-50,-40,-30,-20,-10,0,10), xlim=c(-140,30),
       ilab=cbind(m1i, sd1i, n1i, m2i, sd2i, n2i), ilab.xpos=c(-110,-100,-90,-75,-65,-55),
       cex=.75, header="Author(s) and Year", mlab="")

op <- par(cex=0.7, font=2)

text(c(-106,-96,-86,-71,-61,-51), k+2, pos=2, c("Mean", "SD", "Total", "Mean", "SD", "Total"))
text(c(-90,-59), k+3, pos=2, c("清肝降压胶囊+西药","西药"))

### switch to bold italic font
par(font=4)

text(-140, -1, pos=4, cex=0.9, bquote(paste("DL Model (Q = ",
                                            .(formatC(res$QE, digits=2, format="f")), ", df = ", .(res$k - res$p),
                                            ", p = ", .(formatC(res$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                            .(formatC(res$I2, digits=1, format="f")), "%)")))
dev.off()
########################QJDH
outcomes <- readxl::read_xlsx(path = 'data/meta subCPM.xlsx', sheet = 1)

outcomes <- outcomes %>% filter(CPM == "QJDH")

dat <- outcomes

k <- nrow(dat)

dat <- escalc(measure="MD", n1i=n1i, n2i=n2i, m1i = m1i, m2i = m2i,
              sd1i = sd1i, sd2i = sd2i, data=dat, slab=paste(author, year))
dat

res <- rma(yi, vi, data=dat, method="DL")
res
pdf("output1/SBP_QJDH.pdf", width = 8.97, height = 3.1, family = "GB1")
forest(res, at=c(-50,-40,-30,-20,-10,0,10), xlim=c(-140,30),
       ilab=cbind(m1i, sd1i, n1i, m2i, sd2i, n2i), ilab.xpos=c(-110,-100,-90,-75,-65,-55),
       cex=.75, header="Author(s) and Year", mlab="")

op <- par(cex=0.7, font=2)

text(c(-106,-96,-86,-71,-61,-51), k+2, pos=2, c("Mean", "SD", "Total", "Mean", "SD", "Total"))
text(c(-90,-59), k+3, pos=2, c("杞菊地黄丸+西药","西药"))

### switch to bold italic font
par(font=4)

text(-140, -1, pos=4, cex=0.9, bquote(paste("DL Model (Q = ",
                                            .(formatC(res$QE, digits=2, format="f")), ", df = ", .(res$k - res$p),
                                            ", p = ", .(formatC(res$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                            .(formatC(res$I2, digits=1, format="f")), "%)")))
dev.off()

# name-------------------------------------------------------------------------

nameCH <- c("松龄血脉康胶囊+西药",
            "强力定眩片+西药药",
            "天麻钩藤颗粒+西药",
            "心脉通胶囊+西药",
            "清肝降压胶囊+西药",
            "杞菊地黄丸+西药")
nameEN <- c("SLXM","QLDX","TMGT","XMT","QGJY","QJDH")

# function SBP ----------------------------------------------------------------

forest_plot_CPM <- function(i){
  
  outcomes <- readxl::read_xlsx(path = 'data/meta subCPM.xlsx', sheet = 1)
  outcomes <- outcomes %>% filter(CPM == nameEN[i])
  dat <- outcomes
  
  k <- nrow(dat)
  dat <- escalc(measure="MD", n1i=n1i, n2i=n2i, m1i = m1i, m2i = m2i,
                sd1i = sd1i, sd2i = sd2i, data=dat, slab=paste(author, year))
  dat
  
  res <- rma(yi, vi, data=dat, method="DL")
  res
  
  pdf(paste0("output1/SBP_", nameEN[i], ".pdf"), width = 8.97, height = 0.17*k+2.33, family = "GB1")
  forest(res, at=c(-50,-40,-30,-20,-10,0,10), xlim=c(-140,30),
         ilab=cbind(m1i, sd1i, n1i, m2i, sd2i, n2i), ilab.xpos=c(-110,-100,-90,-75,-65,-55),
         cex=.75, header="Author(s) and Year", mlab="")
  
  op <- par(cex=0.7, font=2)
  text(c(-106,-96,-86,-71,-61,-51), k+2, pos=2, c("Mean", "SD", "Total", "Mean", "SD", "Total"))
  text(c(-90,-59), k+3, pos=2, c(nameCH[i],"西药"))
  
  ### switch to bold italic font
  par(font=4)
  text(-140, -1, pos=4, cex=0.9, bquote(paste("DL Model (Q = ",
                                              .(formatC(res$QE, digits=2, format="f")), ", df = ", .(res$k - res$p),
                                              ", p = ", .(formatC(res$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                              .(formatC(res$I2, digits=1, format="f")), "%)")))
  dev.off()
}

# -------------------------------------------------------------------------
for (i in 1:6) {
  forest_plot_CPM(i)
}

# function DBP-------------------------------------------------------------------------
forest_plot_CPM <- function(i){
  
  outcomes <- readxl::read_xlsx(path = 'data/meta subCPM.xlsx', sheet = 2)
  outcomes <- outcomes %>% filter(CPM == nameEN[i])
  dat <- outcomes
  
  k <- nrow(dat)
  dat <- escalc(measure="MD", n1i=n1i, n2i=n2i, m1i = m1i, m2i = m2i,
                sd1i = sd1i, sd2i = sd2i, data=dat, slab=paste(author, year))
  dat
  
  res <- rma(yi, vi, data=dat, method="DL")
  res
  
  pdf(paste0("output1/DBP_", nameEN[i], ".pdf"), width = 8.97, height = 0.17*k+2.33, family = "GB1")
  forest(res, at=c(-30,-20,-10,0,10), xlim=c(-110,25),
         ilab=cbind(m1i, sd1i, n1i, m2i, sd2i, n2i), ilab.xpos=c(-83,-73,-63,-53,-43,-35),
         cex=.75, header="Author(s) and Year", mlab="")
  
  op <- par(cex=0.7, font=2)
  text(c(-79,-69,-59,-49,-39,-31), k+2, pos=2, c("Mean", "SD", "Total", "Mean", "SD", "Total"))
  text(c(-67,-37), k+3, pos=2, c(nameCH[i],"西药"))
  
  ### switch to bold italic font
  par(font=4)
  text(-110, -1, pos=4, cex=0.9, bquote(paste("DL Model (Q = ",
                                              .(formatC(res$QE, digits=2, format="f")), ", df = ", .(res$k - res$p),
                                              ", p = ", .(formatC(res$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                              .(formatC(res$I2, digits=1, format="f")), "%)")))
  dev.off()
}

# -------------------------------------------------------------------------
for (i in 1:6) {
  forest_plot_CPM(i)
}

# function rate -----------------------------------------------------------
forest_plot_CPM <- function(i){
  
  outcomes <- readxl::read_xlsx(path = 'data/meta subCPM.xlsx', sheet = 3)
  outcomes <- outcomes %>% filter(CPM == nameEN[i])
  dat <- outcomes
  
  k <- nrow(dat)
  dat <- escalc(measure="RR", ai=x1i, n1i=n1i, ci=x2i, n2i=n2i, data=dat,
                slab=paste(author, year))
  dat
  
  res <- rma(yi, vi, data=dat, method="EE")
  res
  
  pdf(paste0("output1/rate_", nameEN[i], ".pdf"), width = 8.97, height = 0.17*k+2.33, family = "GB1")
  forest(res, at=log(c(.3, 1, 3)), xlim=c(-5.5,2),atransf=exp,
         ilab=cbind(x1i, n1i, x2i, n2i), ilab.xpos=c(-3.9,-3.1,-2.3,-1.5),
         cex=.75, header="Author(s) and Year", mlab="")
  
  op <- par(cex=0.7, font=2)
  
  text(c(-3.9,-3.1,-2.3,-1.5), k+2, pos=2, c("Events","Total","Events","Total"))
  text(c(-3.5,-1.9), k+3, pos=2, c(nameCH[i],"西药"))
  
  ### switch to bold italic font
  par(font=4)
  text(-5.5, -1, pos=4, cex=0.9, bquote(paste("EE Model (Q = ",
                                              .(formatC(res$QE, digits=2, format="f")), ", df = ", .(res$k - res$p),
                                              ", p = ", .(formatC(res$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                              .(formatC(res$I2, digits=1, format="f")), "%)")))
  dev.off()
}

# -------------------------------------------------------------------------

for (i in 1:6) {
  forest_plot_CPM(i)
}

# function adverse --------------------------------------------------------
nameCH <- c("松龄血脉康胶囊+西药",
            "强力定眩片+西药药",
            "天麻钩藤颗粒+西药",
            "心脉通胶囊+西药")
nameEN <- c("SLXM","QLDX","TMGT","XMT")

forest_plot_CPM <- function(i){
  
  outcomes <- readxl::read_xlsx(path = 'data/meta subCPM.xlsx', sheet = 4)
  outcomes <- outcomes %>% filter(CPM == nameEN[i])
  dat <- outcomes
  
  k <- nrow(dat)
  dat <- escalc(measure="RR", ai=x1i, n1i=n1i, ci=x2i, n2i=n2i, data=dat,
                slab=paste(author, year), drop00 = TRUE)
  dat
  
  res <- rma(yi, vi, data=dat, method="EE")
  res
  
  options(na.action = "na.pass")
  
  pdf(paste0("output1/adverse_", nameEN[i], ".pdf"), width = 8.97, height = 0.17*k+2.33, family = "GB1")
  forest(res, at=log(c(.01,.1, 1, 4)), xlim=c(-10.5,3.5),atransf=exp,
         ilab=cbind(x1i, n1i, x2i, n2i), ilab.xpos=c(-8,-7,-6,-5),
         cex=.75, header="Author(s) and Year", mlab="")
  
  op <- par(cex=0.7, font=2)
  
  text(c(-8,-7,-6,-5), k+1, pos=3, c("Events","Total","Events","Total"))
  text(c(-7.5,-5.5), k+2, pos=3, c(nameCH[i],"西药"))
  # text(3.3, 7, "Not estimable", pos=2)
  ### switch to bold italic font
  par(font=4)
  text(-10.5, -1, pos=4, cex=0.9, bquote(paste("EE Model (Q = ",
                                              .(formatC(res$QE, digits=2, format="f")), ", df = ", .(res$k - res$p),
                                              ", p = ", .(formatC(res$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                              .(formatC(res$I2, digits=1, format="f")), "%)")))
  dev.off()
}

forest_plot_CPM(1) #7
forest_plot_CPM(3) #1

forest_plot_CPM(2)
forest_plot_CPM(4)

# 中医证候积分 ------------------------------------------------------------------
outcomes <- read.csv(file = "outputdata/4.csv")

dat <- outcomes %>% 
  select(-X) %>% 
  filter(CPM == "SLXM")

k <- nrow(dat)

dat <- escalc(measure="MD", n1i=n1i, n2i=n2i, m1i = m1i, m2i = m2i,
              sd1i = sd1i, sd2i = sd2i, data=dat, slab=paste(author, year))
dat

res <- rma(yi, vi, data=dat, method="DL")
res

pdf("outputdata/synscore_SLXM.pdf", width = 8.97, height = 3.92, family = "GB1")
forest(res, at=c(-5,-4,-3,-2,-1,0,1), xlim=c(-19.5,6),
       ilab=cbind(m1i, sd1i, n1i, m2i, sd2i, n2i), ilab.xpos=c(-14.5,-13,-11.5,-9.5,-8,-6.5),
       cex=.75, header="Author(s) and Year", mlab="")

op <- par(cex=0.7, font=2)

text(c(-14.5,-13,-11.5,-9.5,-8,-6.5), k+2, c("Mean", "SD", "Total", "Mean", "SD", "Total"))
text(c(-13,-8), k+2.7, c("松龄血脉康胶囊+西药","西药"))

### switch to bold italic font
par(font=4)

text(-19.5, -1, cex=0.9, pos=4, bquote(paste("DL Model (Q = ",
                                            .(formatC(res$QE, digits=2, format="f")), ", df = ", .(res$k - res$p),
                                            ", p = ", .(formatC(res$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                            .(formatC(res$I2, digits=1, format="f")), "%)")))
dev.off()
###########################
outcomes <- read.csv(file = "outputdata/4.csv")

dat <- outcomes %>% 
  select(-X) %>% 
  filter(CPM == "TMGT")

k <- nrow(dat)

dat <- escalc(measure="MD", n1i=n1i, n2i=n2i, m1i = m1i, m2i = m2i,
              sd1i = sd1i, sd2i = sd2i, data=dat, slab=paste(author, year))
dat

res <- rma(yi, vi, data=dat, method="EE")
res

pdf("outputdata/synscore_TMGT.pdf", width = 8.97, height = 3.6, family = "GB1")
forest(res, at=c(-5,-4,-3,-2,-1,0,1), xlim=c(-19.5,5),
       ilab=cbind(m1i, sd1i, n1i, m2i, sd2i, n2i), ilab.xpos=c(-14.5,-13,-11.5,-9.5,-8,-6.5),
       cex=.75, header="Author(s) and Year", mlab="")

op <- par(cex=0.7, font=2)

text(c(-14.5,-13,-11.5,-9.5,-8,-6.5), k+2, c("Mean", "SD", "Total", "Mean", "SD", "Total"))
text(c(-13,-8), k+2.7, c("天麻钩藤颗粒+西药","西药"))

### switch to bold italic font
par(font=4)

text(-19.5, -1, cex=0.9, pos=4, bquote(paste("EE Model (Q = ",
                                             .(formatC(res$QE, digits=2, format="f")), ", df = ", .(res$k - res$p),
                                             ", p = ", .(formatC(res$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                             .(formatC(res$I2, digits=1, format="f")), "%)")))
dev.off()

# 血压变异性DBP ----------------------------------------------------------------
outcomes <- read.csv(file = "outputdata/11.csv")

dat <- outcomes %>% 
  select(-X) %>% 
  filter(CPM == "SLXM")

k <- nrow(dat)

dat <- escalc(measure="MD", n1i=n1i, n2i=n2i, m1i = m1i, m2i = m2i,
              sd1i = sd1i, sd2i = sd2i, data=dat, slab=paste(author, year))
dat

res <- rma(yi, vi, data=dat, method="DL")
res

pdf("outputdata/DBPV_SLXM.pdf", width = 8.97, height = 3.92, family = "GB1")
forest(res, at=c(-1,0,1,2,3,4,5), xlim=c(-15,9),
       ilab=cbind(m1i, sd1i, n1i, m2i, sd2i, n2i), ilab.xpos=c(-10,-8.5,-7,-5.5,-4,-2.5),
       cex=.75, header="Author(s) and Year", mlab="")

op <- par(cex=0.7, font=2)

text(c(-10,-8.5,-7,-5.5,-4,-2.5), k+2, c("Mean", "SD", "Total", "Mean", "SD", "Total"))
text(c(-8.5,-4), k+2.7, c("松龄血脉康胶囊+西药","西药"))

### switch to bold italic font
par(font=4)

text(-15, -1, cex=0.9, pos=4, bquote(paste("DL Model (Q = ",
                                             .(formatC(res$QE, digits=2, format="f")), ", df = ", .(res$k - res$p),
                                             ", p = ", .(formatC(res$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                             .(formatC(res$I2, digits=1, format="f")), "%)")))
dev.off()

# ET1 ---------------------------------------------------------------------
outcomes <- read.csv(file = "outputdata/18.csv")

dat <- outcomes %>% 
  select(-X) %>% 
  filter(CPM == "TMGT")

k <- nrow(dat)

dat <- escalc(measure="MD", n1i=n1i, n2i=n2i, m1i = m1i, m2i = m2i,
              sd1i = sd1i, sd2i = sd2i, data=dat, slab=paste(author, year))
dat

res <- rma(yi, vi, data=dat, method="DL")
res

pdf("outputdata/ET1_TMGT.pdf", width = 9.88, height = 3.28, family = "GB1")
forest(res, at=c(-25,-20,-15,-10,-5,0), xlim=c(-55,11),
       ilab=cbind(m1i, sd1i, n1i, m2i, sd2i, n2i), ilab.xpos=c(-43,-40,-37,-33,-30,-27),
       cex=.75, header="Author(s) and Year", mlab="")

op <- par(cex=0.7, font=2)

text(c(-43,-40,-37,-33,-30,-27), k+2, c("Mean", "SD", "Total", "Mean", "SD", "Total"))
text(c(-40,-30), k+2.7, c("天麻钩藤颗粒+西药","西药"))

### switch to bold italic font
par(font=4)

text(-55, -1, cex=0.9, pos=4, bquote(paste("DL Model (Q = ",
                                           .(formatC(res$QE, digits=2, format="f")), ", df = ", .(res$k - res$p),
                                           ", p = ", .(formatC(res$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                           .(formatC(res$I2, digits=1, format="f")), "%)")))
dev.off()
#########################
outcomes <- read.csv(file = "outputdata/18.csv")

dat <- outcomes %>% 
  select(-X) %>% 
  filter(CPM == "XMT")

k <- nrow(dat)

dat <- escalc(measure="MD", n1i=n1i, n2i=n2i, m1i = m1i, m2i = m2i,
              sd1i = sd1i, sd2i = sd2i, data=dat, slab=paste(author, year))
dat

res <- rma(yi, vi, data=dat, method="EE")
res

pdf("outputdata/ET1_XMT.pdf", width = 9.88, height = 3.04, family = "GB1")
forest(res, at=c(-9,-6,-3,0), xlim=c(-21,4),
       ilab=cbind(m1i, sd1i, n1i, m2i, sd2i, n2i), ilab.xpos=c(-16,-15,-14,-12,-11,-10),
       cex=.75, header="Author(s) and Year", mlab="")

op <- par(cex=0.7, font=2)

text(c(-16,-15,-14,-12,-11,-10), k+2, c("Mean", "SD", "Total", "Mean", "SD", "Total"))
text(c(-15,-11), k+2.7, c("心脉通胶囊+西药","西药"))

### switch to bold italic font
par(font=4)

text(-21, -1, cex=0.9, pos=4, bquote(paste("EE Model (Q = ",
                                           .(formatC(res$QE, digits=2, format="f")), ", df = ", .(res$k - res$p),
                                           ", p = ", .(formatC(res$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                           .(formatC(res$I2, digits=1, format="f")), "%)")))
dev.off()

# SBPV --------------------------------------------------------------------
outcomes <- read.csv(file = "outputdata/20.csv")

dat <- outcomes %>% 
  select(-X) %>% 
  filter(CPM == "SLXM")

k <- nrow(dat)

dat <- escalc(measure="MD", n1i=n1i, n2i=n2i, m1i = m1i, m2i = m2i,
              sd1i = sd1i, sd2i = sd2i, data=dat, slab=paste(author, year))
dat

res <- rma(yi, vi, data=dat, method="DL")
res

pdf("outputdata/SBPV_SLXM.pdf", width = 9.08, height = 3.04, family = "GB1")
forest(res, at=c(-1,0,1,2,3,4,5), xlim=c(-15,9),
       ilab=cbind(m1i, sd1i, n1i, m2i, sd2i, n2i), ilab.xpos=c(-10,-8.5,-7,-5.5,-4,-2.5),
       cex=.75, header="Author(s) and Year", mlab="")

op <- par(cex=0.7, font=2)

text(c(-10,-8.5,-7,-5.5,-4,-2.5), k+2, c("Mean", "SD", "Total", "Mean", "SD", "Total"))
text(c(-8.5,-4), k+2.7, c("松龄血脉康胶囊+西药","西药"))

### switch to bold italic font
par(font=4)

text(-15, -1, cex=0.9, pos=4, bquote(paste("DL Model (Q = ",
                                           .(formatC(res$QE, digits=2, format="f")), ", df = ", .(res$k - res$p),
                                           ", p = ", .(formatC(res$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                           .(formatC(res$I2, digits=1, format="f")), "%)")))
dev.off()

# -------------------------------------------------------------------------
source(file = "scripts/auto_meta_forest_plot.R") #函数还是不完善，尤其是二分类的at范围

autometa_forest_plot(2,"XMT","心脉通胶囊")

autometa_forest_plot(3,"SLXM","松龄血脉康胶囊")
autometa_forest_plot(4,"SLXM","松龄血脉康胶囊")
autometa_forest_plot(9,"SLXM","松龄血脉康胶囊")






