
autometa_forest_plot <- function(i, CP, CPCH){
  library(metafor)
  library(tidyverse)
  
  outcomes <- read.csv(file = paste0("outputdata/", i, ".csv"))
  
  dat <- outcomes %>% 
    select(-X) %>% 
    filter(CPM == CP)
  
  k <- nrow(dat)
  
  if(ncol(dat) == 7) {
    dat <- escalc(measure="RR", ai=x1i, n1i=n1i, ci=x2i, n2i=n2i, data=dat,
                  slab=paste(author, year))
  } else {
    dat <- escalc(measure="MD", n1i=n1i, n2i=n2i, m1i = m1i, m2i = m2i,
                  sd1i = sd1i, sd2i = sd2i, data=dat, slab=paste(author, year))
  }
  
  #if判断I2是否大于一个值来选择前面的method
  if(rma(yi, vi, data=dat, method="DL")$I2 < 50){
    res <- rma(yi, vi, data=dat, method="EE")
  } else {
    res <- rma(yi, vi, data=dat, method="DL")
  }
  
  min.ci.lb = floor(min(dat$yi - qnorm(0.975) * sqrt(dat$vi))-1)
  max.ci.ub = ceiling(max(dat$yi + qnorm(0.975) * sqrt(dat$vi))+1)
  
  if(ncol(dat) == 11){
    pdf(paste0(CP, "_", i, ".pdf"), width = (max(min.ci.lb - 25, max.ci.ub + 7) - min(min.ci.lb - 25, max.ci.ub + 7))/5, height = 0.17*k+2.33, family = "GB1")
    sav <- forest(res, at=seq(min.ci.lb, max.ci.ub), xlim= c(min.ci.lb - 25, max.ci.ub + 7),
                  ilab=cbind(m1i, sd1i, n1i, m2i, sd2i, n2i), ilab.xpos=c(min.ci.lb-seq(17,2,by=-3)),
                  cex=.75, header="Author(s) and Year", mlab="")
    
    op <- par(cex=0.7, font=2)
    
    text(sav$ilab.xpos, k+2, c("Mean", "SD", "Total", "Mean", "SD", "Total"))
    text(sav$ilab.xpos[c(2,5)], k+3, c(paste0(CPCH, "+西药"),"西药"))
    
    par(font=4)
    
    text(sav$xlim[1], -1, cex=0.9, pos=4,
         bquote(paste(.(formatC(ifelse(rma(yi, vi, data=dat, method="DL")$I2 < 50, "EE", "DL"))),
                      " Model (Q = ",.(formatC(res$QE, digits=2, format="f")), ", df = ", .(res$k - res$p),
                      ", p = ", .(formatC(res$QEp, digits=2, format="f")), "; ", I^2, " = ",
                      .(formatC(res$I2, digits=1, format="f")), "%)")))
    dev.off()
  } else {
    pdf(paste0(CP, "_", i, ".pdf"), width = (max(min.ci.lb - 25, max.ci.ub + 7) - min(min.ci.lb - 25, max.ci.ub + 7))/5, height = 0.17*k+2.33, family = "GB1")
    sav <- forest(res, at=log(c(0.01,0.1,1,3)), xlim=c(min.ci.lb-6,max.ci.ub+2),atransf=exp,
                  ilab=cbind(x1i, n1i, x2i, n2i), ilab.xpos=c(min.ci.lb-3,min.ci.lb-2,min.ci.lb-1,min.ci.lb),
                  cex=.75, header="Author(s) and Year", mlab="")
    
    op <- par(cex=0.7, font=2)
    
    text(sav$ilab.xpos, k+2, c("Events","Total","Events","Total"))
    text(c((sav$ilab.xpos[1]+sav$ilab.xpos[2])/2,(sav$ilab.xpos[3]+sav$ilab.xpos[4])/2), k+3, c(paste0(CPCH, "+西药"),"西药"))
    
    par(font=4)
    
    text(sav$xlim[1], -1, cex=0.9, pos=4,
         bquote(paste(.(formatC(ifelse(rma(yi, vi, data=dat, method="DL")$I2 < 50, "EE", "DL"))),
                      " Model (Q = ",.(formatC(res$QE, digits=2, format="f")), ", df = ", .(res$k - res$p),
                      ", p = ", .(formatC(res$QEp, digits=2, format="f")), "; ", I^2, " = ",
                      .(formatC(res$I2, digits=1, format="f")), "%)")))
    dev.off()
  }
}



