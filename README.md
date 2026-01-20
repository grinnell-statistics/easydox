# easydox
When you analyze and visualize your experiments, are you always bored with creating nice, concise, and easy-to-understand visualizations? From assumption checking to effects visualizations, everything can be done in one line with easydox, whether it's three-way ANOVA, block design, or split-plot design!

# Installation
```
# install.packages("devtools")
# install.packages("gridExtra")
devtools::install_github("grinnell-statistics/easydox")
```

# Motivation
When reviewing the available packages in R, we found that there are currently few user-friendly packages where interface designs match how researchers and students cognitively design experiments. They also do not typically provide basic commands for graphic options that are particularly meaningful to researchers. Without easily accessible graphics, it becomes challenging to validate the experiment assumptions and statistically analyze the experiment. Our new R Package, easydox, provides simple yet powerful commands for users to verify model assumptions, visualize main and interaction effects, and conduct post hoc analysis. With easydox, we provide a format for teaching the design of experiments that encourages conceptual understanding and practical examples rather than coding details.


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
dox_resid = function(anova_formula, dataset, plot = "All", bins = 30)

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
dox_inter = function(y~x1+x2, dataset, label="Mean", text_size=12)
```

**ANOVA \& PostHoc**
```
# ANOVA Table with Total DF \& SS
dox_aov = function(anova_formula, dataset)

# Confidence Intervals using Fisher Least Significant Difference (LSD), 
# Bonferroni Significant Difference (BSD), and Tukey Honest Significant Difference (HSD)
dox_pairs = function(y~x, dataset, alpha = 0.05, method = "All")
```


# Datasets Available in the Package

**View All Datasets**
```
data(package = "easydox")
```

**Balanced Factorial Design**
```
Bacteria
Cholesterol
MemoryA
Movies
Popcorn
Soda
Towels2
```

**Fractional Factorial Design**
```
Cups
```

**Block Design/Split-Plot Design**
```
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

# Shiny App
We also build a [Shiny App](https://huandongchang.shinyapps.io/ExperimentalAnalysis/) to help users without any R programming background. It provides two examples and also allows users to upload their own dataset.

# Example
You can find a complete example
[here](http://htmlpreview.github.io/?https://github.com/HuandongChang/easydox/blob/main/vignettes/introduction.html). Some highlights:

![](figure/vartable.png)

![](figure/residual_all.png)

![](figure/interactionEffect.png)

# References
Tanaka, Emi, and Dewi Amaliah. "Current state and prospects of R-packages for the design of experiments." arXiv preprint arXiv:2206.07532 (2022).

# Developmental Credits
Huangdong Chang, Shonda Kuiper, Ankur Roy

