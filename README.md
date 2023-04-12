# easydox
When you analyze and visualize your experiments, are you always bored with creating nice, concise, and easy-to-understand visualizations? From assumption checking to effects visualizations, everything can be done in one line with easydox, whether it's three-way ANOVA, block design, or split-plot design!

# Installation
```
# install.packages("devtools")
# install.packages("gridExtra")
devtools::install_github("HuandongChang/easydox")
```

# Motivation
When reviewing the packages in R we find that currently there are few user-friendly packages whose interface designs match the way researchers and students cognitively design experiments (Tanaka & Amaliah, 2022). They also do not typically provide meaningful graphics options that address all types of variabilities. Without easily accessible graphics, it becomes challenging to validate the experiment assumptions and statistically analyze the experiment results efficiently, comprehensively, and rigorously. easyExp provides simple but powerful commands for users so that beginners like students can easily gain hands-on experiences and can focus more on experimental designs rather than commands. 


# Functions
**Checking ANOVA Assumptions**


1) equal variances within groups
```
# barplot
dox_boxplot = function(y~x, dataset, color=NULL, facet = NULL)

# scatterplot
dox_scatterplot = function(y~x, dataset, color=NULL, facet = NULL, jitter = FALSE)

# interactive standard deviation table
dox_table = function(y~x1+x2+x3+x4, dataset)
```


2) normally distributed residuals \& independent and identically distributed observations
```
dox_resid = function(anova_formula, dataset, plot = "all", bins = 30)

# or you can pick one plot
dox_resid = function(anova_formula, dataset, plot = 1, bins = 30)
```

3) missing data or balanced design
```
dox_sumstats = function(y~x1+x2+x3+x4, dataset)
```

**Main Effect and Interaction Effect Plots**
```
# Main Effect Plots
dox_main = function(y~x1+x2+x3+x4, dataset, label="Mean", ylim(optional), text_size=12)

# Interaction Effect Plots
dox_inter = function(y~x1+x2, dataset, facet = NULL, label="Mean", text_size=12)
```

**ANOVA \& PostHoc**
```
# ANOVA Table with Total DF \& SS
dox_aov = function(anova_formula, dataset)

# Confidence Intervals using Fisher Least Siginificant Difference (LSD), 
# Bonferroni Significant Difference (BSD), and Tukey Honest Siginificant Difference (HSD)
dox_pairs = function(y~x, dataset, alpha = 0.05, method = "ALL")
```


# Datasets Available in the Package
```
# one-way/two-way/three-way anova
Bacteria
Cholesterol
Cups
MemoryA
Movies
Popcorn
Soda
Towels2

# block design/split-plot design
C5Popcorn
Colors
Colors2
Cookies
Cookies2
Corn
Flower
Football
Handwash
Memory
Music
Tennis
```


# Example
You can find a complete example
[here](http://htmlpreview.github.io/?https://github.com/HuandongChang/easydox/blob/main/vignettes/introduction.html). Some highlights:

![](figure/vartable.png)

![](figure/residual_all.png)

![](figure/interactionEffect.png)

# References
Tanaka, Emi, and Dewi Amaliah. "Current state and prospects of R-packages for the design of experiments." arXiv preprint arXiv:2206.07532 (2022).


