set.seed(500)

n <- 5000
df <- data.frame(
	value = c(rnorm(n, mean = 0, sd = 1),
						rnorm(n, mean = 3, sd = 1.25),
						rnorm(n, mean = 6, sd = 1.5)),
	dist = factor(rep(c("Normal1", "Normal2", "Normal3"), each = n))
)

fill_colors <- c("Normal1" = "#89ABE3" , "Normal2" = "#c88b8b", "Normal3" ="#1a2b4c")

ggplot(df, aes(x = value, fill = dist)) +
	geom_density(alpha = 0.7, color = NA, adjust = 1.5) +  # adjust >1 smooths more
	scale_fill_manual(values = fill_colors) +
	theme_void() +
	theme(legend.position = "none")