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
	cd code/scripts; Rscript $(<F)

scaling: code/scripts/$(pred_scaling) $(subset_data) $(aux_data)
	cd code/scripts; Rscript $(<F)

regressions: 
	make b-ridge && make b-ols && h-ridge && make h-ols

b-ridge:  code/functions/$(black_ridge) $(scaled_data)
	cd code/functions; Rscript $(<F)

b-ols: code/functions/$(black_ols) $(scaled_data)
	cd code/functions; Rscript $(<F)

h-ridge: code/functions/$(hisp_ridge) $(scaled_data)
	cd code/functions; Rscript $(<F)

h-ols: code/functions/$(hisp_ols) $(scaled_data)
	cd code/functions; Rscript $(<F)

analysis: code/scripts/$(analysis) $(aux_data) $(ranked-ridge-hispanic) $(ranked-ridge-black)
	cd code/scripts; Rscript $(analysis) $(aux_data) $(ranked-ridge-black) $(ranked-ridge-hispanic)

report: $(report_rnw) $(images) $(rdata)
	Rscript -e "library(rsweave); render('report.Rnw', 'pdf_document')"

slides: slides/slides.Rmd
	Rscript -e "library(rmarkdown); render('slides/slides.Rmd', 'html_document')"

#making session.info.txt
session: 
	bash session.sh	

#remove the report.pdf, report.Rnw
clean:
	rm -f $(report_pdf)
	rm -f $(report_rnw)






