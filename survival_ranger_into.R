#### ---- Introduction to Survival Analtsy ----
#### ---- Task: Learning survival analysis ----
#### ---- Date: 2019 Oct 03 (Thu)          ----
#### ---- Author: Steve Bicko              ----

library(survival)
library(ranger)

library(ggplot2)
theme_set(theme_bw() +
	theme(panel.spacing=grid::unit(0,"lines")))

library(dplyr)
library(ggfortify)


#### ---- Data ----

## Using the veteran data

df <- veteran

# Kaplan Meier Analysis

km <- with(df, Surv(time, status))

# Note that a “+” after the time in the print out of km indicates censoring.

print(km)
plot(km)

# Estimate the probability of survival

km_fit <- survfit(Surv(time, status) ~ 1, data = df)
summary(km_fit)

km_fit_plot <- (km_fit
	%>% fortify()
	%>% ggplot(aes(x = time, y = surv))
		+ geom_line(colour = "red", alpha = 0.5)
		+ geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightgreen", color = NA, alpha = 0.3)
		+ labs(x = "Time", y = "Survival probabilities")
)
print(km_fit_plot)

## Group by age

df <- (df
	%>% mutate(age_group = ifelse(age >= 60, "50 and above", "Under 50"))
)

km_fit2 <- survfit(Surv(time, status) ~ age_group, data = df)
summary(km_fit2)

km_fit_plot2 <- (km_fit2
	%>% fortify()
	%>% ggplot(aes(x = time, y = surv, colour = strata))
		+ geom_line(alpha = 0.5)
		+ geom_ribbon(aes(ymin = lower, ymax = upper, fill = strata), color = NA, alpha = 0.3)
		+ labs(x = "Time", y = "Survival probabilities")
)
print(km_fit_plot2)
