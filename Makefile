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


.PHONY: all eda regressions scaling report b-ridge b-ols h-ridge h-ols analysis slides session clean
 

all: 
	eda scaling regressions report

eda: code/scripts/$(eda) $(subset_data)
	cd code/scripts; Rscript $(eda) $(subset_data)

scaling: code/scripts/$(pred_scaling) $(subset_data) $(aux_data)
	cd code/scripts; Rscript $(pred_scaling) $(subset_data) $(aux_data)

regressions: 
	make b-ridge && make b-ols && h-ridge && make h-ols

b-ridge: $(scaled_data) $(black_ridge)
	cd code/functions; Rscript $(black_ridge) $(scaled_data)

b-ols: $(scaled_data) $(black_ols)
	cd code/functions; Rscript $(black_ols) $(scaled_data)

h-ridge: $(scaled_data) $(hisp_ridge)
	cd code/functions; Rscript $(hisp_ridge) $(scaled_data)

h-ols: $(scaled_data) $(hisp_ols)
	cd code/functions; Rscript $(hisp_ols) $(scaled_data)

analysis: code/scripts/$(analysis) $(aux_data) $(ranked-ridge-hispanic) $(ranked-ridge-black)
	cd code/scripts; Rscript $(analysis) $(aux_data) $(ranked-ridge-black) $(ranked-ridge-hispanic)

report: $(report_rnw) $(images) $(rdata)
	Rscript -e "library(rsweave); render('report.Rnw', 'pdf_document')"

slides: slides/slides.Rmd
	Rscript -e "library(rmarkdown); render('slides/slides.Rmd', 'html_document')"

session: 
	cd code/scripts; Rscript $(session)

clean:
	clean $(report_pdf)
	clean $(report_rnw)






