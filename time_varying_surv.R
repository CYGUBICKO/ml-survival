library(survival)

set.seed(1953) # a good year
nvisit <- floor(pmin(lung$time/30.5, 12))

## Merging survival data to create followup times

df <- cgd0
head(df)

newcgd <- tmerge(data1=df[, 1:13], data2=df, id=id, tstop=futime)

