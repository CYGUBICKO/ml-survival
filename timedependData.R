#### ---- Task: Survival Analysis ----
#### ---- Subtask: Handling time dependent covariate and RE ----
#### ---- Tools: GAM models using pammtools ----
#### ---- By: Steve & BB ----
#### ---- Date: 2020 Mar 09 (Mon) ----

library(tidyr)
library(dplyr)

source("funs/downloadDf.R")

#### ---- 1. Time-dependent covariates

## Uses recidivism dataset

df_url <- "http://math.unm.edu/~james/Rossi.txt"
working_df <- downloadDf(filename = "recidivism"
	, filetype = "csv"
	, df_url = df_url
	, sep = ""
)

head(working_df)

## The trick is to take the time varying covariates
## and convert them to long format
working_df <- (working_df
	%>% mutate(sid = row_number())
	%>% gather(calendar.week, employed, emp1:emp52)
	%>% filter(!is.na(employed))
)

head(working_df)

## Generate observation start-stop. Though not sure why they have to use the row numbers: Seems like discrete time?
working_df <- (working_df
	%>% group_by(sid)
	%>% mutate(start = row_number()-1
		, stop = row_number()
		, arrest = ifelse(stop == last(stop) & arrest == 1, 1, 0) # Event occurs at the end of the observation per subject
		,  offset = log(stop - start) # Not sure what this does?
	)
)

print(working_df %>% filter(sid==1), n = 10, width = Inf)

## Unlagged
working_df_unlagged <- (working_df
	%>% arrange(sid, stop)
	%>% select(sid, start, stop, offset, arrest, employed, fin:educ)
	%>% ungroup()
)

## Lagged employment
working_df_lagged <- (working_df
	%>% arrange(sid, stop)
	%>% select(sid, start, stop, offset, arrest, employed, fin:educ)
	%>% mutate(employed.lag1 = lag(employed, default = 0)) # Not sure why this is lagged
	%>% slice(-1) # Drop the first week lagged observation?
	%>% ungroup()
)


save(file = "timedependData.rda"
	, working_df_unlagged
	, working_df_lagged
)
