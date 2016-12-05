# files in report directory
report_rnw = report/report.Rnw
report_pdf = report/report.pdf

# files in data directory
subset_data = data/subset-data.csv
aux_data = data/aux-data.csv
scaled_data = data/scaled-predictors.csv
rdata = data/*.RData
ranked_ridge_black = data/ranked-ridge-black.csv
ranked_ridge_hisp = data/ranked-ridge-hispanic.csv

# files in scripts directory
eda = eda-scripts.R
pred_scaling = pred-scaling-script.R
session = session-info-script.R
analysis = analysis.R

# files in function directory
black_ridge = black-ridge.R
black_ols = ols-black.R
hisp_ridge = hispanic-ridge.R
hisp_ols = ols-hispanic.R

# files in the images directory
images = images/*.png

# files in the shiny directory
shiny = shiny/app.R

.PHONY: all eda regressions scaling report b-ridge b-ols h-ridge h-ols analysis slides session shiny clean

all: 
	eda scaling regressions report

# make the eda scripts and all the images
eda: code/scripts/$(eda) $(subset_data)
	cd code/scripts; Rscript $(<F)

# make the scaling test and train sets
scaling: code/scripts/$(pred_scaling) $(subset_data) $(aux_data)
	cd code/scripts; Rscript $(<F)

# make all the ols and ridge regressions of African Americans and Hispanic groups
regressions: 
	make b-ridge && make b-ols && h-ridge && make h-ols

# make the African American ridge
b-ridge:  code/functions/$(black_ridge) $(scaled_data)
	cd code/functions; Rscript $(<F)

# make the African American ols
b-ols: code/functions/$(black_ols) $(scaled_data)
	cd code/functions; Rscript $(<F)

# make the Hispanic ridge
h-ridge: code/functions/$(hisp_ridge) $(scaled_data)
	cd code/functions; Rscript $(<F)

# make the Hispanic ols
h-ols: code/functions/$(hisp_ols) $(scaled_data)
	cd code/functions; Rscript $(<F)
	
# make the analysis scripts for the data
analysis: code/scripts/$(analysis) $(aux_data) $(ranked-ridge-hispanic) $(ranked-ridge-black)
	cd code/scripts; Rscript $(<F)
	
# make report.pdf from report.Rnw
report: $(report_rnw) $(images) $(rdata)
	Rscript -e "library(rsweave); render('report.Rnw', 'pdf_document')"

# make slides.html
slides: slides/slides.Rmd
	Rscript -e "library(rmarkdown); render('slides/slides.Rmd', 'html_document')"

# making session.info.txt
session: 
	bash session.sh	

# make the shiny app and run the ui
shiny: $(shiny)
	Rscript $(<F)

# remove the report.pdf, report.Rnw
clean:
	rm -f $(report_pdf)
	rm -f $(report_rnw)






