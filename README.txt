Ths package contains code for generating the results in: Association of Vitamin D level and maternal gut microbiome during pregnancy: Findings from a randomized controlled trial of antenatal vitamin D supplementation

authors: Andrea Aparicio, Diane R. Gold, Scott T. Weiss,  Augusto A. Litonjua, Kathleen Lee-Sarwar, Yang-Yu Liu

code written by: Andrea Aparicio
——————————————————————————————

The project was developed in R 4.2.1

List of required packages 
-tableone
-phyloseq
-dplyr
-reshape
-vegan
-dplyr
-ANCOMBC
-ALDEx2
-tibble
-ggplot2
-Maaslin2
-rbiom
-stringr
-biomformat
-gtools
-cowplot
-ggpubr

——————————————————————————————

Usage

The ‘main.R’ file contains the code to execute all the necessary scripts to perform the statistical analysis and create the visualizations.

The ‘main.R’ file is organized in sections 0-7, each one performing one part of the analysis. Section 0 must be run always at the beginning of the session because all other sections depend on its outputs; sections 1-7 can run independently from each other. 

-section 0: executes the scripts that load the necessary dependencies, establish the paths to the data location and to the location where to store the figures, and import and filter the data. 

-section 1: builds Tables 1, 2, S1, and S2
	+to build a table for a categorical variable execute ‘table1.R’ and run the function:
	table1_fun(var_g, dataM, variabsall, var_cat, wr)
	+to build a table for a categorical variable execute ‘table1.R’ and run the function:
	table1_cont_fun(var_g, dataM, variabsall, var_cat, wr)
		*inputs (both functions)
		var_g: stratifying variable
		dataM: dataframe containing metadata
		variabxsall: vector of all variables to include
		var_cat: vector of all categorical variables in variabsall
		wr: boolean, T writes the resulting table in the data/ folder
		*output: data frame containing the table

-section 2: performs alpha diversity analysis
	+to perform the analysis execute ‘alpha_diversity.R’ and run the function: 
	lm_richness_fun(var_g,cova)
		*inputs: 
		var_g: stratifying variable
		cova: vector of covariates
		*output: data frame containing the adjusted and unadjusted linear regression p values for the analyzed alpha diversity indices
	+to build the plots in figures 2.a, 2.c, and S1.b execute ‘process_data.R’ after having run  ‘alpha_diversity.R’
		*outputs: the figures will be saved in the folder figs/

-section 3: performs beta diversity analysis
	+to perform the analysis run execute ‘beta_diversity.R’ and run the function: 
	perma_dist_fun(var_g, cova) 
		*inputs
		var_g: stratifying variable
		cova: vector of covariates
		*output: data frame containing the adjusted permanova p-values for the analyzed beta diversity distances
	+to build the plots in figures 2.b, 2.d, S1.c, S1.d execute ‘plots_beta_diversity.R’ after having run ‘beta_diversity.R’
		*outputs:  the figures will be saved in the folder figs/

-section 4: performs the differential abundance analysis
	+to perform the any differential abundance analysis, first execute ‘differential_abundance.R’
	+to perform an ANCOM-BC analysis, then run the function:
	ancom_difabundance_fun(var_g,cova) 
		*inputs
		var_g: stratifying variable
		cova: vector of covariates
		*output: list with two elements: data frames containing the adjusted and unadjusted ANCOM-BC results for significantly differently abundant taxa 
	+to generate the plot in figure 3.a (corresponding to the change in vitamin D -var_g=vitDch-), after performing ANCOM analysis with var_g=vitDch, run the function:
	ancom_plot_fun(ancomplot_data,ancomplot_cols,ancomplot_labs)
		*inputs
		ancomplot_data: data frame containing the results from ANCOM-BC 
		ancomplot_cols: list of colors for the plot
		ancomplot_labs: list of labels for the plot
		*output:  the figure will be saved in the folder figs/
	+to perform a Maaslin2 analysis, then run the function:
	maaslin_association_fun(var_g,cova,refs_groups)
		*inputs
		var_g: stratifying variable
		cova: vector of covariates
		refs_groups: vector containing the reference groups for variables with more than 2 groups
		*output: data frame containing the Maaslin2 results for significantly differently abundant taxa 

-section 5: performs the statistical analysis related to mothers’ diet and children’s microbiome
	+to perform the analysis execute ‘diet_children.R’
		*input: the script uses as inputs the microbiome data, the  metadata, and the food questionnaire principal component data  generated from the data processing step in section 0
		*output: the results of the statistical analysis are printed out in the console. 

-section 6: perform the rest of the statistical analysis presented in the manuscript
	+to perform the analysis execute ‘other_statistics.R’
		*input: the script uses as inputs the microbiome data and the metadata generated from the data processing step in section 0
		*output: the results of the statistical analysis are printed out in the console. 

-section 7: produces the plots in figures 1a-d, 3b-c, S1a, S2, S3a-b
	+to perform the analysis execute ‘other_plots.R’
		*input: the script uses as inputs the microbiome data and the metadata generated from the data processing step in section 0
		*output: the figures will be saved in the folder figs/

——————————————————————————————

List of script files:
-alpha_diversity.R
-beta_diversity.R
-compile_metadata.R
-dependencies.R
-diet_children.R
-differential_abundance.R
-import_data.R
-main.R
-other_plots.R
-other_statistics.R
-plots_alpha_diversity.R
-plots_beta_diversity.R
-process_data.R
-table1.R




