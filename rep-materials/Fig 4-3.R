
# Figure 4.3 -------------------------------------------------------------

x1 <- rlogis(10000, location = 4)
x2 <- rlogis(10000, location = 2)
x3 <- rlogis(10000, location = 0)

df1 <- data.frame(x1,x2, x3)
df1 <- melt(df1)

theme_set(theme_tufte(base_size = 12, base_family = "sans"))

ggplot(df1, aes(x = value, group = variable)) + stat_ecdf(geom = "line") + 
  xlab(expression(x %->%NA)) + ylab(expression(P(Y <= j))) + 
  annotate("text", x = 3.4, y = 0.5, label = "P(Y <= 1)", angle = 50, parse = T) + 
  annotate("text", x = 1.4, y = 0.5, label = "P(Y <= 2)", angle = 50, parse = T) + 
  annotate("text", x = -.6, y = 0.5, label = "P(Y <= 3)", angle = 50, parse = T) + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = "NA") + coord_cartesian(xlim = c(-7, 10))

ggsave(filename = "ordlog.pdf", width = 6, height = 2)



