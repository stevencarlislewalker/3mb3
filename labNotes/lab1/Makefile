## To copy this makefile into each subdirectory type cts into a
## command line.  For a specific directory, type ct dir.
##
## set environment variable S2K if Sweave2Knitr should be run first
## (can be done by setting -f flag to the cts function)

RNWsrc := $(shell basename $$(find . -name '*.Rnw') .Rnw)
PDFs = $(RNWsrc:=.pdf)

ifdef S2K
	S2K = "Sweave2knitr(\"$(RNWsrc).Rnw\", output = \"$(RNWsrc).Rnw\")"
endif
ifndef S2K
	S2K = "NULL"
endif

all: $(PDFs)

%.pdf: sober.sty knitrScript.R %.Rnw
	cp $(RNWsrc).Rnw tmp.Rnw
	cat ../labskel.tex $(RNWsrc).Rnw > tmp2.Rnw
	mv tmp2.Rnw $(RNWsrc).Rnw
	Rscript --no-save knitrScript.R
	mv tmp.Rnw $(RNWsrc).Rnw
	make clean

sober.sty: ../../styleFiles/sober.sty
	cp ../../styleFiles/$@ $@

knitrScript.R:
	RNWsrc=$(ls | "grep .Rnw")
	touch knitrScript.R
	echo "library(knitr)" >> knitrScript.R
	echo $(S2K) >> knitrScript.R
	echo "try(knit2pdf(\"$(RNWsrc).Rnw\"))" >> knitrScript.R
	cat knitrScript.R

clean:
	rm -f *.aux *.log *.out *.bbl *.blg knitrScript.R sober.sty .DS_Store tmp.Rnw
