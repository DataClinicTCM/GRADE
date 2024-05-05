outcomes <- read.csv(file = "outputdata/9.csv")
table(outcomes$CPM)
######################
dat <- outcomes %>% 
  select(-X) %>% 
  filter(CPM == "TMGT")

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
       linreg = FALSE)
