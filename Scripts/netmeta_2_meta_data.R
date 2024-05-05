library(tidyverse)
# library(inferregex)

netmeta_dat <- readxl::read_xlsx(path = "data/Outcome_indicators.xlsx", sheet = 8)

# s <- "SLXM_ARB"
# infer_regex(s)$regex

pattern <- "^[A-Z]{3,4}_[A-Z]{3,4}$"
netmeta_dat <- netmeta_dat %>% mutate(group = ifelse(grepl(pattern, treatment),
                                                     "treat", "control"))

meta_dat <- netmeta_dat %>% 
  pivot_wider(id_cols = study, names_from = group,
              values_from = c(mean, std.dev, sampleSize, treatment)) %>% 
  select(-treatment_control) %>% 
  separate(col = study, into = c("author", "year"), sep = -4) %>% 
  mutate(treatment_treat = sub("_.*", "", treatment_treat)) %>% 
  set_names("author", "year", "m2i", "m1i", "sd2i", "sd1i", "n2i", "n1i", "CPM")
  
############################
netmeta_dat <- readxl::read_xlsx(path = "data/Outcome_indicators.xlsx", sheet = 3)

pattern <- "^[A-Z]{3,4}_[A-Z]{3,4}$"
netmeta_dat <- netmeta_dat %>% mutate(group = ifelse(grepl(pattern, treatment),
                                                     "treat", "control"))

meta_dat <- netmeta_dat %>% 
  pivot_wider(id_cols = study, names_from = group,
              values_from = c(responders, sampleSize, treatment)) %>%
  select(-treatment_control) %>% 
  separate(col = study, into = c("author", "year"), sep = -4) %>% 
  mutate(treatment_treat = sub("_.*", "", treatment_treat)) %>% 
  set_names("author", "year", "x2i", "x1i", "n2i", "n1i", "CPM")

# -------------------------------------------------------------------------
netmeta2meta <- function(i){
  netmeta_dat <- readxl::read_xlsx(path = "data/Outcome_indicators.xlsx", sheet = i)
  pattern <- "^[A-Z]{3,4}_[A-Z]{3,4}$"
  netmeta_dat <- netmeta_dat %>% mutate(group = ifelse(grepl(pattern, treatment),
                                                       "treat", "control"))
  if(ncol(netmeta_dat) == 5){
    meta_dat <- netmeta_dat %>% 
      pivot_wider(id_cols = study, names_from = group,
                  values_from = c(responders, sampleSize, treatment)) %>%
      select(-treatment_control) %>% 
      separate(col = study, into = c("author", "year"), sep = -4) %>% 
      mutate(treatment_treat = sub("_.*", "", treatment_treat)) %>% 
      set_names("author", "year", "x2i", "x1i", "n2i", "n1i", "CPM")
  } else {
    meta_dat <- netmeta_dat %>% 
      pivot_wider(id_cols = study, names_from = group,
                  values_from = c(mean, std.dev, sampleSize, treatment)) %>% 
      select(-treatment_control) %>% 
      separate(col = study, into = c("author", "year"), sep = -4) %>% 
      mutate(treatment_treat = sub("_.*", "", treatment_treat)) %>% 
      set_names("author", "year", "m2i", "m1i", "sd2i", "sd1i", "n2i", "n1i", "CPM")
  }
  write.csv(meta_dat, file = paste0("outputdata/", i, ".csv"))
}
# -------------------------------------------------------------------------

for (i in 1:20) {
  netmeta2meta(i)
}
