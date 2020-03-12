#### ---- Task: Survival Analysis ----
#### ---- Subtask: Handling time dependent covariate and RE ----
#### ---- Tools: GAM models using pammtools ----
#### ---- By: Steve & BB ----
#### ---- Date: 2020 Mar 09 (Mon) ----

library(tidyr)
library(dplyr)
library(ggplot2); theme_set(theme_bw() + 
theme(panel.spacing = grid::unit(0,"lines"), legend.position = "bottom"))

load("timedependTidy.rda")

## Some terms smoothed in gam?

pos <- ggstance::position_dodgev(height=0.5)

p1 <- (ggplot(extract_coefs_df, aes(x = coef, y = variable, colour = Method, lty = model))
	+ geom_point(position = pos)
	+ ggstance::geom_linerangeh(aes(xmin = ci_lower, xmax = ci_upper), position = pos)
	+ scale_colour_manual(name = "Data"
		, values=c("#E41A1C", "#000000")
		, guide = guide_legend(reverse = TRUE)
	)
	+ geom_vline(xintercept=0,lty=2)
	+ labs(x = "Estimate"
		, y = ""
		, colour = "Data"
		, lty = "Model"
	)
	+ theme(legend.position = "bottom")
)
print(p1)

