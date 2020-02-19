library(survival)
library(ggplot2)
library(dplyr)

df <- veteran

## Proportion surviving

prop_surv_plot <- (ggplot(df, aes(x = time, group = as.factor(status), fill = as.factor(status)))
	+ geom_histogram(alpha = 0.3)
	+ labs(x = "Days"
		, y = "Count"
		, fill = "Survival status"
	)
	+ scale_fill_brewer(palette = "Dark2")
	+ theme_bw()
	+ theme(legend.position = "bottom")
)
print(prop_surv_plot)

## Category plots

catPlots <- function(df, xvar, fillvar, alpha = 0.3){
	df <- (df
		%>% mutate_at(fillvar, as.factor)
	)
	p1 <- (ggplot(df, aes_string(x = xvar, group = fillvar, fill = fillvar))
		+ geom_bar(stat = "count", position = position_dodge(), alpha = alpha)
		+ scale_fill_brewer(palette = "Dark2")
		+ theme_bw()
		+ theme(legend.position = "bottom")
	)
	return(p1)
}

celltype_plot <- catPlots(df, "celltype", "status")
