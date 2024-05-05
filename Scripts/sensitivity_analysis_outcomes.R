library(tidyverse)
library(metafor)
library(forplo)

# SBP ---------------------------------------------------------------------

outcomes <- readxl::read_xlsx(path = 'data/meta回归数据.xlsx', sheet = 1)

dat <- escalc(measure="MD", n1i=n1i, n2i=n2i, m1i = m1i, m2i = m2i,
              sd1i = sd1i, sd2i = sd2i, data=outcomes, slab=paste(author, year, sep=", "))
dat

res <- rma(yi, vi, data=dat, method="DL")
res
mydat <- leave1out(res)
mydat <- as.data.frame(mydat)

tiff(file = "output/SBP_sensitivity.tiff", compression = 'none', width = 800, height = 1200)
forplo(mydat[,c(1,5,6)],
       xlim = c(-13.5,-8),
       em = "MD",
       ci.edge = FALSE,
       sort = FALSE,
       horiz.bar = TRUE,
       col = 2,
       margin.right = 12,
       size = 3.5)
dev.off()

# DBP ---------------------------------------------------------------------

outcomes <- readxl::read_xlsx(path = 'data/meta回归数据.xlsx', sheet = 2)

dat <- escalc(measure="MD", n1i=n1i, n2i=n2i, m1i = m1i, m2i = m2i,
              sd1i = sd1i, sd2i = sd2i, data=outcomes, slab=paste(author, year, sep=", "))
dat

res <- rma(yi, vi, data=dat, method="DL")
res
mydat <- leave1out(res)
mydat <- as.data.frame(mydat)

tiff(file = "output/DBP_sensitivity.tiff", compression = 'none', width = 800, height = 1200)
forplo(mydat[,c(1,5,6)],
       xlim = c(-8.5,-5.5),
       em = "MD",
       ci.edge = FALSE,
       sort = FALSE,
       horiz.bar = TRUE,
       col = 2,
       margin.right = 12,
       size = 3.5)
dev.off()

# rate --------------------------------------------------------------------
outcomes <- readxl::read_xlsx(path = 'data/meta回归数据.xlsx', sheet = 3)

dat <- escalc(measure="RR", ai=x1i, n1i=n1i, ci=x2i, n2i=n2i, data=outcomes, slab=paste(author, year, sep=", "))
dat

res <- rma(yi, vi, data=dat, method="EE")
res
mydat <- leave1out(res, transf = exp) #必须要加exp转换，因为RR默认log
mydat <- as.data.frame(mydat)

tiff(file = "output/rate_sensitivity.tiff", compression = 'none', width = 800, height = 1200)
forplo(mydat[,c(1,4,5)],
       xlim = c(1.14,1.2),
       em = "RR",
       ci.edge = FALSE,
       sort = FALSE,
       horiz.bar = TRUE,
       col = 2,
       margin.right = 11,
       size = 3.5)
dev.off()

# adverse -----------------------------------------------------------------

outcomes <- readxl::read_xlsx(path = 'data/meta回归数据.xlsx', sheet = 4)

dat <- escalc(measure="RR", ai=x1i, n1i=n1i, ci=x2i, n2i=n2i, data=outcomes, slab=paste(author, year, sep=", "))
dat

res <- rma(yi, vi, data=dat, method="EE")
res
mydat <- leave1out(res, transf = exp) #必须要加exp转换，因为RR默认log
mydat <- as.data.frame(mydat)

tiff(file = "output/adverse_sensitivity.tiff", compression = 'none', width = 800, height = 800)
forplo(mydat[,c(1,4,5)],
       xlim = c(0.4,0.8),
       em = "RR",
       ci.edge = FALSE,
       sort = FALSE,
       horiz.bar = TRUE,
       col = 2,
       margin.right = 12,
       size = 3.5)
dev.off()
