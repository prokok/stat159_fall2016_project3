# Stat 159 Fall 2016 Project 3

###Authors: Stacy Chang, Phil Hoon Oh, James de Lorimier, Tim Mcginley

###Date: December 5th, 2016

![Stat 159 Class Logo](https://raw.githubusercontent.com/ucb-stat159/stat159-fall-2016/master/projects/proj01/images/stat159-logo.png)

This repository holds the third and last project for Statistics 159 Fall 2016 at UC Berkeley.

The repository structure:
```
code/
	functions/
		black-ridge.R
		hispanic-ridge.R
		ols-black.R
		ols-hispanic.R
	scripts/
		analysis.R
		eda-scripts.R
		pred-scaling-script.R
		session-info-script.R
		states-abbrev.R
	README.md
data/
	aux-data.csv
	eda-output.txt
	ols-black-model.RData
	ols-hisp-model.RData
	ranked-ols-black.csv
	ranked-ols.hisp.csv
	ranked-ridge-black.csv
	ranked-ridge-hispanic.csv
	ridge-black-model.RData
	ridge-hisp-model.RData
	scaled-predictors.csv
	subset-data.csv
	subset-data.RData
images/
	README.md
	*.png
report/
  report.Rnw
  report.pdf
slides/
  slides.html
  slides.Rmd
 shiny/
 	app.R
.gitignore
LICENSE
Makefile
README.md
session-info.txt
```

In order to effectively reproduce this project, clone the repository to your local machine. Then run 'make' in your terminal. Make sure all the necessary R packages are installed prior to running the .R scripts. The necessary packages can be found in the session-info.txt. After reading the result of the analysis run 'make clean' to delete the report Rnw file and report PDF file.

In MakeFile here is a list of '.Phony' targets you can run through MakeFile.

* all

	```
		#reproducing the results
		make all
	```
* eda

	```
		#generating eda-output.txt and related plots
		make slides
	```
* regressions

	```
		#generating regression-analysis and related Rdata.
		make regressions
	```
* scaling

	```
		#generating scaled dataset.
		make slides
	```
	
* b-ridge, b-ols, h-ridge, h-ols
	```
		#generating each regression analysis
		make b-ridge
		make b-ols
		make h-ridge
		make h-ols
	```
* analysis

	```
		#generating all regression analysis
		make analysis
	```
* slides

	```
		#generating slides
		make slides
	```
* session

	```
		#generating session info
		make session
	```
* clean

	```
		#deleting report
		make clean
	```
	
	
	
<a rel="license" href="http://creativecommons.org/licenses/by/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by/4.0/88x31.png" /></a><br />All media content (e.g. paper/report, and images) licensed under a <a rel="license" href="http://creativecommons.org/licenses/by/4.0/">Creative Commons Attribution 4.0 International License</a>.

All code licensed under MIT License






