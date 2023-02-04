library(tidyverse)

## KDE and GP fit
xx = read.table("simulation_results_bk", header=TRUE)
yy = read.table("simulation_results", header=TRUE)
zz = read.table("simulation_results_bk2", header=TRUE)
ww = read.table("simulation_results_bk3", header=TRUE)

xx = rbind(xx, yy, zz, ww)

ss = xx %>% group_by(V1, V2) %>% summarise(
  exp_l2_mean = mean(V4) / 100,
  exp_l2_std = sqrt(var(V4) / n()) / 100,
  kde_l2_mean = mean(V5) / 100,
  kde_l2_std = sqrt(var(V5) / n()) / 100
)
# ss = xx %>% group_by(V1, V2) %>% summarise(
#   exp_l2_mean = median(V4) / 100,
#   exp_l2_std = sqrt(var(V4) / n()) / 100,
#   kde_l2_mean = median(V5) / 100,
#   kde_l2_std = sqrt(var(V5) / n()) / 100
# )

## BDR fit
bb = read.table("BDR/bdr_simulation_results", header=TRUE)
bb2 = read.table("BDR/bdr_simulation_results_bk", header=TRUE)
bb3 = read.table("BDR/bdr_simulation_results_bk2", header=TRUE)

bb = rbind(bb, bb2)
bb = rbind(bb, bb3)

ss2 = bb %>% group_by(V1, V2) %>% summarise(
  bdr_k10_l2_mean = mean(V4) / 100,
  bdr_k10_l2_std = sqrt(var(V4) / n()) / 100,
  bdr_k50_l2_mean = mean(V5) / 100,
  bdr_k50_l2_std = sqrt(var(V5) / n()) / 100
)
# ss2 = bb %>% group_by(V1, V2) %>% summarise(
#   bdr_k10_l2_mean = median(V4) / 100,
#   bdr_k10_l2_std = sqrt(var(V4) / n()) / 100,
#   bdr_k50_l2_mean = median(V5) / 100,
#   bdr_k50_l2_std = sqrt(var(V5) / n()) / 100
# )


## plot
data.frame(
  n = c(ss$V1, ss$V1, ss2$V1, ss2$V1),
  m = c(ss$V2, ss$V2, ss2$V2, ss2$V2),
  method = c(rep("EXP", nrow(ss)), rep("KDE", nrow(ss)), 
             rep("BDR\n (k=10)", nrow(ss2)), rep("BDR\n (k=50)", nrow(ss2))),
  L2 = c(ss$exp_l2_mean, ss$kde_l2_mean, ss2$bdr_k10_l2_mean, ss2$bdr_k50_l2_mean), 
  std = c(ss$exp_l2_std, ss$kde_l2_std, ss2$bdr_k10_l2_std, ss2$bdr_k50_l2_std)
) %>% ggplot(aes(x=m, y=L2, color=method)) +
  geom_line() + 
  geom_errorbar(aes(ymin = L2 - 1.96*std, ymax = L2 + 1.96*std),
                position = "identity", width = 0.05) +
  facet_wrap(vars(n), nrow=2, ncol=3, labeller=label_both) +
  scale_x_log10() + 
  scale_y_log10() +
  ylab("square L2") +
  theme_bw()


p2 = data.frame(
  n = c(ss$V1, ss$V1, ss2$V1, ss2$V1),
  m = c(ss$V2, ss$V2, ss2$V2, ss2$V2),
  method = c(rep("EXP", nrow(ss)), rep("KDE", nrow(ss)), 
             rep("BDR\n(k=10)\n", nrow(ss2)), rep("BDR\n(k=50)\n", nrow(ss2))),
  L2 = c(ss$exp_l2_mean, ss$kde_l2_mean, ss2$bdr_k10_l2_mean, ss2$bdr_k50_l2_mean), 
  std = c(ss$exp_l2_std, ss$kde_l2_std, ss2$bdr_k10_l2_std, ss2$bdr_k50_l2_std)
) %>% dplyr::filter(n < 500) %>% ggplot(aes(x=n, y=L2, color=method)) +
  geom_line(size=0.75) + geom_point(aes(shape=method)) + 
  # geom_errorbar(aes(ymin = L2 - 1.96*std, ymax = L2 + 1.96*std),
  #               position = "identity", width = 0.05) +
  facet_wrap(vars(m), nrow=2, ncol=3, labeller=label_both) +
  scale_x_log10() + 
  scale_y_log10() +
  # scale_x_continuous(limits=c(50, 100, 200, 300, 400)) +
  ylab("Posterior Risk") +
  theme_bw()
  # theme(legend.position = c(0.9, 0.4), 
  #       legend.text = element_text(size=5))

p2

ggsave(filename="plots/risk_by_m.png", p2, width=8, height=6)


# data.frame(
#   n = c(ss$V1, ss$V1),
#   m = c(ss$V2, ss$V2),
#   method = c(rep("EXP", nrow(ss)), rep("KDE", nrow(ss))),
#   L2 = c(ss$exp_l2_mean, ss$kde_l2_mean), 
#   std = c(ss$exp_l2_std, ss$kde_l2_std)
# ) %>% ggplot(aes(x=m, y=L2, color=method)) +
#   geom_line() + 
#   geom_errorbar(aes(ymin = L2 - 1.96*std, ymax = L2 + 1.96*std),
#                 position = "identity", width = 0.05) +
#   facet_wrap(vars(n), nrow=2, ncol=3, labeller=label_both) +
#   scale_x_log10() + 
#   scale_y_log10() +
#   ylab("square L2") +
#   theme_bw()
# 
# 
# data.frame(
#   n = c(ss$V1, ss$V1),
#   m = c(ss$V2, ss$V2),
#   method = c(rep("EXP", nrow(ss)), rep("KDE", nrow(ss))),
#   L2 = c(ss$exp_l2_mean, ss$kde_l2_mean), 
#   std = c(ss$exp_l2_std, ss$kde_l2_std)
# ) %>% ggplot(aes(x=n, y=L2, color=method)) +
#   geom_line() + 
#   geom_errorbar(aes(ymin = L2 - 1.96*std, ymax = L2 + 1.96*std),
#                 position = "identity", width = 0.05) +
#   facet_wrap(vars(m), nrow=2, ncol=3, labeller=label_both) +
#   scale_x_log10() + 
#   scale_y_log10() +
#   ylab("square L2") +
#   theme_bw()


