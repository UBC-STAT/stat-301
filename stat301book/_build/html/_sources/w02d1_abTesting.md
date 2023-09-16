---
jupytext:
  cell_metadata_filter: -all
  formats: md:myst
  text_representation:
    extension: .md
    format_name: myst
    format_version: 0.13
    jupytext_version: 1.10.3
kernelspec:
  display_name: R
  language: R
  name: ir
---

# A/B Testing

<img src="https://github.com/UBC-STAT/stat-301/blob/master/supplementary-material/img/ABTesting.png?raw=true" width=700>

*Photos/Images by [Optimizely](https://www.optimizely.com/insights/blog/how-obama-raised-60-million-by-running-a-simple-experiment/)*

## Overview
This chapter provides an introduction to A/B testing and the early peeking problem. The goal is that you understand the main principles of A/B testing that will allow you to properly design, run, and analyze data from A/B tests. While you probably know the main concepts behind A/B testing, the usage of online platforms to collect, store and analyze data from A/B tests open new challenges. For example, people get tempted to peek at results before the A/B test ended and take actions usually result in false discoveries. 

+++

A/B Testing is a technique used to compare two variations of a product or service: control (A) and variation (B) 

- A/B testing became very popular in the context of updating and improving websites. 
    
It's main concepts are founded in the context of hypothesis testing and inference to compare population quantities from 2 distributions

- for example: comparison of two population means or population proportions

+++

## Case study

### Obama's 60 million dollar experiment

In 2008, Obama's campaign was looking to increase the total amount of donations to the campaign. Organizers run an experiment to compare visitors' responses to different versions of the website.

<img src="https://github.com/UBC-STAT/stat-301/blob/master/supplementary-material/img/ABTesting.png?raw=true" width=700>

*A full description of this case study can be found [here](https://www.optimizely.com/insights/blog/how-obama-raised-60-million-by-running-a-simple-experiment/).*

## Experimental Design

- post the *question(s)* you want to answer using data

- *design* the experiment to address your question(s)

- identify appropriate methodologies to analyze the data

- run the experiment to collect data 

- analyze the data according to the experimental design and make decisions

### Question:

> Does visitors of the new website contribute with larger donations for the political campaign?  

### Design:
Different experimental designs will be used depending on the population and the problem we are analyzing. A common choice is a ***randomized controlled experiment***: 

> *Randomly* allocate 1000 visitors to each website.

### Method:

A classical hypothesis test can be used to run the analysis. In general, size will be large enough to rely on the CLT results: 

 > run a 2-sample t-test, compute $p$-values and confidence intervals

## A/B testing: what is new about it?

New platforms have been developed to assist companies to analyze, report and visualize the results of their experiments **in real time**

<img src="https://miro.medium.com/max/1400/1*W8mUB5A96ufsbMWLqScVOA.png" width=600>

<font color=grey>Figure by D. Meisner in Towards Data Science </font>

## Early stopping
> **In classical hypothesis testing theory, the sample size must be fixed in advance when the experiment is designed!!**

These platforms allow the users to *continuously monitor* the $p$-values and confidence intervals in order to re-adjust their experiment dynamically. 

> **Is it ok to peek at results before all the data are collected??**

**Early stopping** refers to ending the experiment earlier than originally designed.

### A/A Testing
To examine the problem of early stopping, you will simulate data for which $H_0$ is true (i.e., there is no effect)
  - we can think of a scenario where both groups are equal (aka A/A testing)
  
  - in this scenario, we know that claiming a significant result is a false discovery
  
<img src="https://github.com/UBC-STAT/stat-301/blob/master/supplementary-material/img/aa-Obama.png?raw=true" width=600>

### Test experiment

- run a balanced experiment with a *pre-set* sample size of 1000 visitors per variation (total sample size of 2000) 

- **sequentially collect** the data in batches of 50 visitors per group

- **sequentially analyze** the data using two-sample $t$-tests

- **sequentially compute and monitor** (raw) $p$-values 

> ***Can we stop or re-design the experiment earlier if we have supporting evidence to do so??***

```{r, code-cell}
library(tidyverse)

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
```

```{r, echo=FALSE, message=FALSE, warning=FALSE}
set.seed(301)
answer2.1 <- 
    incremental_t_test(n = 1000, d_0 = 0, sample_increase_step = 50, mean_current = 200, sd_current = 50, sd_new = 50)

sequential_pvalue <- 
  answer2.1 %>%
  ggplot() +
  geom_line(aes(x = inc_sample_size, y = p_value)) +
  theme(
    text = element_text(size = 18),
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold")
  ) + 
  geom_point(aes(x = inc_sample_size, y = p_value)) +
  ggtitle("Evolution of p-values in Experiment 1") +
  ylab("p-value") +
  xlab("Sample Size") +
  geom_hline(
    yintercept = 0.05,
    colour = "red",
    linetype = "twodash"
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.05))
```

```{r, echo=FALSE, message=FALSE, warning=FALSE}
options(repr.plot.width = 15, repr.plot.height = 9) 
sequential_pvalue
```

### Interpretation

The organizers would have made a mistake if they stopped the experiment the first time the p-value dropped below 0.05!

- changing the website is costly and may not really increase the size of the donations as expected

But, how do we know if this mistake was not due to *randomness*

- the test was planned so that the probability to falsely rejecting $H_0$ is 5% 

### Simulation

To know if this mistake occurs only 5% of the times, we need to run *many* of these experiments!!

The figure below shows the p-value trajectory of 100 experiments. We that the p-values of more than 5% of the experiments is below the significance level. 

+++

<center>
<img src="https://github.com/UBC-STAT/stat-301/blob/master/supplementary-material/img/aa-Obama-pval100.png?raw=true" width=900>
</center>

## Conclusion 

- One may be tempted to peek at results of A/B tests as data are being collected

- Stopping an experiment and rejecting $H_0$ as soon as the $p$-value is below the specified significance level can drastically inflate the type I error rate

- Controlling the risk of wrongly rejecting the null hypothesis is not an easy task in A/B testing if peeking and early stops are allowed

**Interesting note**:
> It can be proved, mathematically, that under the null hypothesis, the classical $p$-value will *always* cross $\alpha$ if the experimenter waits long enough$^{*}$. This means that with increasing data, the probability of falsely rejecting a true $H_0$ approaches to 1!

[*] David Siegmund. 1985. Sequential analysis: tests and confidence intervals.
Springer.
