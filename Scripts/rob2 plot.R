library(robvis) #development version

rob_tools()

rob2dat <- readxl::read_xlsx(path = "data/meta回归数据 (2).xlsx", sheet = 7)

colnames(rob2dat) <- c("Study", "Time", paste0("D", 1:5), "Overall")

rob2dat_withoutQJDH <- rob2dat[-(52:55),]

# overall count
table(rob2dat_withoutQJDH$Overall)/64


#####################################遍历数据框进行条件变换
# for (i in 2:7) {
#   for (j in 1:68) {
#     rob2dat[[j,i]] <- ifelse(rob2dat[[j,i]] == "+", "Low",ifelse(
#       rob2dat[[j,i]] == "-", "High", "Some concerns"))
#   }
# }
# 
# # !!!使用 apply() 函数进行向量化，没想到apply还能同时行+列处理，免于嵌套for循环，同时使用匿名函数嵌套ifelse
# rob2dat[, 2:7] <- apply(rob2dat[, 2:7], c(1, 2), function(x) {
#   ifelse(x == "+", "Low", ifelse(x == "-", "High", "Some concerns"))
# })

rob2dat_withoutQJDH[, 3:8] <- apply(rob2dat_withoutQJDH[, 3:8], c(1, 2), function(x) {
  ifelse(x == "+", "Low", ifelse(x == "-", "High", "Some concerns"))
})

# library(dplyr)
# # 使用 mutate_at() 进行向量化
# rob2dat <- rob2dat %>% 
#   mutate_at(vars(2:7), ~ ifelse(. == "+", "Low", ifelse(. == "-", "High", "Some concerns")))
# 
# # library(dplyr)
# for (i in 2:7) {
#   rob2dat[[i]] <- case_when(
#     rob2dat[[i]] == "+" ~ "Low",
#     rob2dat[[i]] == "-" ~ "High",
#     TRUE ~ "Some concerns"
#   )
# }
# 
# 
# # library(dplyr)  mutate_at结合case_when,避免显示循环，优雅！！
# rob2dat <- rob2dat %>% 
#   mutate_at(vars(2:7), 
#             ~case_when(. == "+" ~ "Low", 
#                        . == "-" ~ "High", 
#                        TRUE ~ "Some concerns"))

#####################################

# p1 <- rob_traffic_light(rob2dat[1:34,], tool = "ROB2", psize = 7)
# p2 <- rob_traffic_light(rob2dat[35:68,], tool = "ROB2", psize = 7)

p1 <- rob_traffic_light(rob2dat_withoutQJDH[1:32,c(1,3:8)], tool = "ROB2", psize = 7)
p2 <- rob_traffic_light(rob2dat_withoutQJDH[33:64,c(1,3:8)], tool = "ROB2", psize = 7)

# rob_save(p1, file = "rob2_figure1.png")
# rob_save(p2, file = "rob2_figure2.png")

rob_save(p1, file = "rob2_withoutQJDH_figure1.png")
rob_save(p2, file = "rob2_withoutQJDH_figure2.png")

# 显示中文
# library(showtext)
# library(ggplot2)
# font_families()
# font_paths()
# font_files()
# font_add(family = "song", regular = "C:\\Windows\\Fonts\\simsun.ttc")
# font_add(family = "heiti", regular = "C:\\Windows\\Fonts\\simhei.ttf")

