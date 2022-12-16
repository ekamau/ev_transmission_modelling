.PHONY: all

# this is a list of all the things that you want your make file to try to produce
# note that I need only to state the ultimate outputs I want, not all the intermediary ones
# since make realises that the dependencies need be run to produce a given output
all: figures/Figure1.pdf, figures/Figure2.pdf, figures/Figure3.pdf, figures/Figure4.pdf,
figures/Figure5.pdf, figures/FigureS1.pdf, figures/FigureS2.pdf, figures/FigureS3.pdf,
figures/FigureS4.pdf, figures/FigureS5.pdf, figures/FigureS6.pdf, figures/FigureS7.pdf,
figures/FigureS8.pdf, figures/FigureS9.pdf, figures/Figure10.pdf, figures/Figure11.pdf,
figures/Figure12.pdf, figures/Figure13.pdf

# this is a recipe to make ".rds" with a dependency for this file is src/r/prior_predictive_simulations.R
# Rscript $< runs the file. The $< is a shorthand for referring to the first dependency,
# so make interprets this as Rscript src/r/prior_predictive_simulations.R
results/EVA71/: src/r/prior_predictive_simulations.R
	Rscript $<
	
# this recipe shows that results/prior_predictive_dilutions.pdf depends on two things:
# src/r/prior_predictive_plot.R and data/processed/prior_predictive_dose_response.rds
# the \ at then end of the first line just allows us to list dependencies across
# lines rather than a long list
results/prior_predictive_dilutions.pdf: src/r/prior_predictive_plot.R\
	data/processed/prior_predictive_dose_response.rds
	Rscript $<

# this is a recipe to make data/processed/prior_predictive_dose_response.rds
# the only dependency for this file is src/r/prior_predictive_simulations.R
# Rscript $< runs the file. The $< is a shorthand for referring to the first dependency,
# so make interprets this as Rscript src/r/prior_predictive_simulations.R
data/processed/prior_predictive_dose_response.rds: src/r/prior_predictive_simulations.R
	Rscript $<
	
# this recipe shows that results/prior_predictive_dilutions.pdf depends on two things:
# src/r/prior_predictive_plot.R and data/processed/prior_predictive_dose_response.rds
# the \ at then end of the first line just allows us to list dependencies across
# lines rather than a long list
results/prior_predictive_dilutions.pdf: src/r/prior_predictive_plot.R\
	data/processed/prior_predictive_dose_response.rds
	Rscript $<

results/real_data_CVA6/uncertainty_ppc.pdf: src/r/fit_model_real_data_CA6_ppc.R\
	data/raw/CA6_raw_well_observations.csv
	Rscript $<
results/real_data_CVA6/uncertainty_ppc.pdf: src/r/fit_model_real_data_CA6_ppc.R\
	data/raw/CA6_raw_well_observations.csv
	Rscript $<

