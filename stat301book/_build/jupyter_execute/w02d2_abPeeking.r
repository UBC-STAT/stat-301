library(tidyverse)
library(ggplot2)

options(repr.plot.width = 10, repr.plot.height = 5) # Adjust these numbers so the plot looks good in your desktop.
options(warn=-1)

t_x_axis <- seq(-4, 4, 0.1)
crit_unadj <- qt(1 - 0.1, 1998)
crit_bonferroni <- qt(1 - 0.01, 1998)
t_stat <- 1.8

t_dens_data <- data.frame( x = t_x_axis, 
                            y1 = dt(t_x_axis,1998))

test_plot <- ggplot(t_dens_data, aes(x = t_x_axis)) + 
geom_line( aes( y = y1, colour = 'H0 is true' ), size = 1.2 ) + 
geom_area( aes( y = y1, x = ifelse(x > crit_unadj, t_x_axis, NA)), fill = 'black',alpha=0.3) + 
geom_area( aes( y = y1, x = ifelse(x > t_stat, t_x_axis, NA)), fill = 'red',alpha=0.3) + 
geom_area( aes( y = y1, x = ifelse(x > crit_bonferroni, t_x_axis, NA)), fill = 'blue',alpha=0.6) + 
theme(text = element_text(size = 18),
      legend.title = element_blank() ) +
labs(x = '', y = '' ) + 
geom_point(y=0,x=t_stat,size = 3,shape=19,aes(color = "t-stat"))+
geom_point(y=0,x=crit_unadj,size = 3,shape=19,aes(color = "unadj critical val"))+
geom_point(y=0,x=crit_bonferroni,size = 3,shape=1,stroke = 2, aes(color = "bonferroni critical val"))+
scale_colour_manual( breaks = c("H0 is true", "t-stat","unadj critical val","bonferroni critical val"), 
                    values = c("black", "#f94f21","black","blue"),
                    guide = guide_legend(override.aes = list(
                         linetype =c("solid",rep("blank",3)),
                         shape = c(NA, rep(16, 2),1))))+
scale_x_continuous(breaks = c(0))+
ggtitle("Bonferroni: statistical significance adjustment") 

test_plot

options(repr.plot.width = 10, repr.plot.height = 5) # Adjust these numbers so the plot looks good in your desktop.
options(warn=-1)

t_x_axis <- seq(-4, 4, 0.1)
crit_unadj <- qt(1 - 0.1, 1998)
crit_bonferroni <- qt(1 - 0.01, 1998)
t_stat <- 1.8
adj_t_stat <- qt(1-(1-pt(1.8,1998))*10,1998)

t_dens_data <- data.frame( x = t_x_axis, 
                            y1 = dt(t_x_axis,1998))

test_plot <- ggplot(t_dens_data, aes(x = t_x_axis)) + 
geom_line( aes( y = y1, colour = 'H0 is true' ), size = 1.2 ) + 
geom_area( aes( y = y1, x = ifelse(x > crit_unadj, t_x_axis, NA)), fill = 'black',alpha=0.3) + 
geom_area( aes( y = y1, x = ifelse(x > adj_t_stat, t_x_axis, NA)), fill = 'red',alpha=0.3) + 
geom_area( aes( y = y1, x = ifelse(x > t_stat, t_x_axis, NA)), fill = 'red',alpha=0.6) + 
theme(text = element_text(size = 18),
      legend.title = element_blank() ) +
labs(x = '', y = '' ) + 
geom_point(y=0,x=t_stat,size = 3,shape=19,aes(color = "t-stat"))+
geom_point(y=0,x=crit_unadj,size = 3,shape=19,aes(color = "unadj critical val"))+
geom_point(y=0,x=adj_t_stat,size = 3,shape=1,stroke = 2, aes(color = "bonferroni t-stat"))+
scale_colour_manual( breaks = c("H0 is true", "t-stat","unadj critical val","bonferroni t-stat"), 
                    values = c("black", "#f94f21","black","#f94f21"),
                    guide = guide_legend(override.aes = list(
                         linetype =c("solid",rep("blank",3)),
                         shape = c(NA, rep(16, 2),1))))+
scale_x_continuous(breaks = c(0)) +
ggtitle("Bonferroni: p-value adjustment") 
test_plot

options(warn=-1)

# Two-sample t-test with tracking sequential statistic and p-values by incremental sample sizes until getting to n in each group.

# @param n (numeric): Initially planned sample size for each group (for simplicity, n needs to be a multiple of sample_increase_step).
# @param d_0 (numeric): effect size.
# @param mean_current (numeric): Population mean for control variation.
# @param sd_current (numeric): Population standard deviation for current variation.
# @param sd_new (numeric): Population standard deviation for new variation.
# @param sample_increase_step (numeric): Sample size increment.

# @return p.value.df: A tibble that has 3 columns:
# inc_sample_size, statistic, and p_value 

incremental_t_test <- function(n, d_0, mean_current, sd_current, sd_new, sample_increase_step) {
  sample_current <- rnorm(n, mean = mean_current, sd = sd_current)
  sample_new <- rnorm(n, mean = mean_current + d_0, sd = sd_new)

  p.value.df <- tibble(
    inc_sample_size = rep(0, n / sample_increase_step),
    statistic = rep(0, n / sample_increase_step),
    p_value = rep(0, n / sample_increase_step)
  )

  current_sample_size <- sample_increase_step
  
  for (i in 1:nrow(p.value.df))
  {
    t_test_results <- t.test(sample_new[1:current_sample_size], sample_current[1:current_sample_size],
      var.equal = TRUE,
      alternative = "greater"                      
    )
    p.value.df[i, "statistic"] <- as_tibble(t_test_results$statistic)
    p.value.df[i, "p_value"] <- as_tibble(t_test_results$p.value)
    p.value.df[i, "inc_sample_size"] <- current_sample_size
    current_sample_size <- current_sample_size + sample_increase_step
  }

  return(p.value.df)
}

options(repr.plot.width = 10, repr.plot.height = 5) # Adjust these numbers so the plot looks good in your desktop.

set.seed(301)
answer2.1 <- 
    incremental_t_test(n = 1000, d_0 = 0, sample_increase_step = 50, mean_current = 200, sd_current = 50, sd_new = 50)


crit_unadj <- qt(1 - 0.05, 1998)
crit_bonferroni <- qt(1 - 0.0025, 1998)

sequential_stat <- 
  answer2.1 %>%
  ggplot() +
  geom_line(aes(x = inc_sample_size, y = statistic)) +
  geom_point(aes(x = inc_sample_size, y = statistic,colour ="#f94f21"), size = 2) +
#  geom_hline(yintercept = crit_pocock, colour = "red", linetype = "twodash") +
#  geom_point(aes(x = inc_sample_size, y = crit_pocock), colour = "red") +
#  geom_text(x=850, y=crit_pocock + 0.15, size=6, label="Pocock",colour = "red") +
  geom_hline(yintercept = crit_bonferroni, colour = "blue", linetype = "twodash") +
  geom_point(aes(x = inc_sample_size, y = rep(crit_bonferroni, 20)), colour = "blue") +
  geom_text(x=850, y=crit_bonferroni + 0.15, size=6, label="Bonferroni",colour = "blue") +
  geom_hline(yintercept = crit_unadj, linetype = "twodash") +
  geom_point(aes(x = inc_sample_size, y = rep(crit_unadj, 20))) +
  geom_text(x=850, y=crit_unadj + 0.15, size=6, label="Unadjusted") +
  theme(
    text = element_text(size = 18),
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    legend.position="none") +
  ggtitle("Critical values in Sequential Designs") +
  ylab("Statistic") +
  xlab("Sample Size") +
  coord_cartesian(ylim = c(-1, 3)) +
  scale_y_continuous(breaks = seq(-1, 3, by = 0.5))
sequential_stat

# Run this cell to get a Pocock design!
library(gsDesign)

design_pocock <- gsDesign(k = 20, #number of interim analysis planned
                          test.type = 1, # for one-sided tests
                          delta = 0, # default effect size
                          alpha = 0.05, #type II error rate
                          beta = 0.2, # type II error rate
                          sfu = 'Pocock')
                          
crit_pocock <- design_pocock$upper$bound

options(repr.plot.width = 10, repr.plot.height = 5) # Adjust these numbers so the plot looks good in your desktop.

set.seed(301)
answer2.1 <- 
    incremental_t_test(n = 1000, d_0 = 0, sample_increase_step = 50, mean_current = 200, sd_current = 50, sd_new = 50)


crit_unadj <- qt(1 - 0.05, 1998)
crit_bonferroni <- qt(1 - 0.0025, 1998)

sequential_stat <- 
  answer2.1 %>%
  ggplot() +
  geom_line(aes(x = inc_sample_size, y = statistic)) +
  geom_point(aes(x = inc_sample_size, y = statistic,colour ="#f94f21"), size = 2) +
  geom_hline(yintercept = crit_pocock, colour = "#ff33ff", linetype = "twodash") +
  geom_point(aes(x = inc_sample_size, y = crit_pocock), colour = "#ff33ff") +
  geom_text(x=850, y=crit_pocock + 0.15, size=6, label="Pocock",colour = "#ff33ff") +
  geom_hline(yintercept = crit_bonferroni, colour = "blue", linetype = "twodash") +
  geom_point(aes(x = inc_sample_size, y = rep(crit_bonferroni, 20)), colour = "blue") +
  geom_text(x=850, y=crit_bonferroni + 0.15, size=6, label="Bonferroni",colour = "blue") +
  geom_hline(yintercept = crit_unadj, linetype = "twodash") +
  geom_point(aes(x = inc_sample_size, y = rep(crit_unadj, 20))) +
  geom_text(x=850, y=crit_unadj + 0.15, size=6, label="Unadjusted") +
  theme(
    text = element_text(size = 18),
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    legend.position="none") +
  ggtitle("Critical values in Sequential Designs") +
  ylab("Statistic") +
  xlab("Sample Size") +
  coord_cartesian(ylim = c(-1, 3)) +
  scale_y_continuous(breaks = seq(-1, 3, by = 0.5))
sequential_stat 

design_of <- gsDesign(k = 20, #number of interim analysis planned
                          test.type = 1, # for one-sided tests
                          delta = 0, # default effect size
                          alpha = 0.05, #type I error rate
                          beta = 0.2, # type II error rate
                          sfu = 'OF')
                          
crit_of <- design_of$upper$bound

options(repr.plot.width = 10, repr.plot.height = 7) # Adjust these numbers so the plot looks good in your desktop.

set.seed(301)
answer2.1 <- 
    incremental_t_test(n = 1000, d_0 = 0, sample_increase_step = 50, mean_current = 200, sd_current = 50, sd_new = 50)


crit_unadj <- qt(1 - 0.05, 1998)
crit_bonferroni <- qt(1 - 0.0025, 1998)

sequential_stat <- 
  answer2.1 %>%
  ggplot() +
  geom_line(aes(x = inc_sample_size, y = statistic)) +
  geom_point(aes(x = inc_sample_size, y = statistic,colour ="#f94f21"), size = 2) +
  geom_hline(yintercept = crit_pocock, colour = "#ff33ff", linetype = "twodash") +
  geom_line(aes(x = inc_sample_size, y = crit_of),colour = 3, linetype = "twodash") +
  geom_point(aes(x = inc_sample_size, y = crit_of), colour = 3) +
  geom_text(x=160, y= crit_of[1] - 0.15, size=6, label="O'Brien-Fleming",colour = 3) +
  geom_point(aes(x = inc_sample_size, y = crit_pocock), colour = "#ff33ff") +
  geom_text(x=850, y=crit_pocock + 0.15, size=6, label="Pocock",colour = "#ff33ff") +
  geom_hline(yintercept = crit_bonferroni, colour = "blue", linetype = "twodash") +
  geom_point(aes(x = inc_sample_size, y = rep(crit_bonferroni, 20)), colour = "blue") +
  geom_text(x=850, y=crit_bonferroni + 0.15, size=6, label="Bonferroni",colour = "blue") +
  geom_hline(yintercept = crit_unadj, linetype = "twodash") +
  geom_point(aes(x = inc_sample_size, y = rep(crit_unadj, 20))) +
  geom_text(x=850, y=crit_unadj + 0.15, size=6, label="Unadjusted") +
  theme(
    text = element_text(size = 18),
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    legend.position="none") +
  ggtitle("Critical values in Sequential Designs") +
  ylab("Statistic") +
  xlab("Sample Size") +
  coord_cartesian(ylim = c(-1, 8.5)) +
  scale_y_continuous(breaks = seq(-1, 8.5, by = 0.5))
sequential_stat 

set.seed(120)

### Run this before continuing
multiple_times_sequential_tests <- tibble(experiment = 1:100) %>% 
    mutate(seq_test = map(.x = experiment, 
                          .f = function(x) incremental_t_test(n = 1000, d_0 = 0, sample_increase_step = 50, 
                              mean_current = 200, sd_current = 50, sd_new = 50)))

typeI_rate  <- multiple_times_sequential_tests %>% 
    mutate(reject = map_dbl(.x = seq_test, .f = function(x) sum(x$p_value<0.05) > 0)) %>%
    mutate(n_reject_bonferroni = map_dbl(.x = seq_test, .f = function(x) sum(x$p_value < 0.05/20) > 0)) %>%
    mutate(n_reject_pocock = map_dbl(.x = seq_test, .f = function(x) sum(x$statistic > crit_pocock) > 0)) %>%
    mutate(n_reject_OF = map_dbl(.x = seq_test, .f = function(x) sum(x$statistic > crit_of) > 0)) %>%
    summarise(Unadjusted = sum(reject),
              Bonferroni= sum(n_reject_bonferroni),
              Pocock = sum(n_reject_pocock),
              OBrienFleming = sum(n_reject_OF),
              expected_n_rejections = 5)
typeI_rate
