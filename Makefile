# Pipeline for understanding Survival in ML world

current: target
-include target.mk

######################################################################

ms = makestuff
Sources += $(wildcard *.R *.Rmd)
Sources += Makefile rmd.mk

######################################################################

# Clone global function 
## git clone https://github.com/CYGUBICKO/funs.git

## Create symbolic link relative
## ln -s ../funs .

######################################################################

## Time varying covariate data
timedependData.Rout: timedependData.R

## pammtools
timedependPammtools.Rout: timedependPammtools.R

## COX PH
timedependCoxph.Rout: timedependCoxph.R

## Compare estimates
timedependTidy.Rout: timedependTidy.R
timedependEffectsizes.Rout: timedependEffectsizes.R
timedependPredeffects.Rout: timedependPredeffects.R
timedependSurvcurves.Rout: timedependSurvcurves.R

######################################################################

clean: 
	rm -f *Rout.*  *.Rout .*.RData .*.Rout.* .*.wrapR.* .*.Rlog *.RData *.wrapR.* *.Rlog

######################################################################

### Makestuff

Ignore += makestuff
msrepo = https://github.com/dushoff
Makefile: makestuff/Makefile
makestuff/Makefile:
	git clone $(msrepo)/makestuff
	ls $@

include rmd.mk
-include makestuff/os.mk
-include makestuff/visual.mk
-include makestuff/projdir.mk
-include makestuff/texdeps.mk
-include makestuff/pandoc.mk
-include makestuff/stepR.mk
-include makestuff/git.mk

