.PHONY clean

clean:
	rm *.png
	mkdir plots
	rm report.html
	
.PHONY: dir

dir:
	mkdir -p plots

plots/top_words_bubble_plot.png: top_words_bubble_plot.R DOHHsummaries.csv,
doses_data.csv, experience_data.csv, experience_urls.csv | dir
	Rscript top_words_bubble_plot.R
	
plots/top_words_bar_plot.png: top_words_bar_graph.R DOHHsummaries.csv,
doses_data.csv, experience_data.csv, experience_urls.csv | dir
	Rscript top_words_bar_graph.R

plots/dosage_scatter_plot_comparison.png: increasing_dosage_comparison.R 
DOHHsummaries.csv, doses_data.csv, experience_data.csv, 
experience_urls.csv | dir
	Rscript increasing_dosage_comparison.R

plots/consumption_method_box_plot.png: consumption_method_comparison.R 
DOHHsummaries.csv, doses_data.csv, experience_data.csv, 
experience_urls.csv | dir
	Rscript consumption_method_comparison.R
	
plots/substance_comparison_box_plot.png: substance_comparison.R
DOHHsummaries.csv, doses_data.csv, experience_data.csv,
experience_urls.csv | dir
	Rscript substance_comparison.R
