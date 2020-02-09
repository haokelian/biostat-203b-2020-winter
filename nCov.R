# remotes::install_github("GuangchuangYu/nCov2019")
library(nCov2019)
x <- get_nCov2019()
lapply(x, head)
#total patients
summary(x)
#today added
summary(x, by = 'today')
#graphs
ggplot(summary(x), aes(as.Date(date,"%m.%d"), as.numeric(confirm))) +
  geom_col(fill='firebrick') + theme_minimal(base_size = 14) +
  xlab("") + ylab("nCov patients") +
  labs(caption = paste("accessed date:", time(x)))
