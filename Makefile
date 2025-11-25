.PHONY: clean

clean:
	rm -rf plots
	mkdir plots
	rm -f report.html
	
.PHONY: dir

dir:
	mkdir -p plots

report.html: report.Rmd plots/top_words_bubble_plot.png plots/top_words_bar_plot.png\
plots/dosage_scatter_plot_comparison.png plots/consumption_method_box_plot.png\
plots/top_words_bubble_plot_DMT.png plots/top_words_bubble_plot_salvia.png

plots/top_words_bubble_plot.png: top_words_bubble_plot.R summaries.csv\
doses_data.csv experience_data.csv experience_urls.csv | dir
	Rscript top_words_bubble_plot.R
	
plots/top_words_bar_plot.png: top_words_bar_graph.R summaries.csv\
doses_data.csv experience_data.csv experience_urls.csv | dir
	Rscript top_words_bar_graph.R

plots/dosage_scatter_plot_comparison.png: increasing_dosage_comparison.R summaries.csv\
doses_data.csv experience_data.csv experience_urls.csv | dir
	Rscript increasing_dosage_comparison.R

plots/consumption_method_box_plot.png: consumption_method_comparison.R summaries.csv\
doses_data.csv experience_data.csv experience_urls.csv | dir
	Rscript consumption_method_comparison.R
	
plots/top_words_bubble_plot_DMT.png: substance_comparison.R summaries.csv\
doses_data.csv experience_data.csv experience_urls.csv | dir
	Rscript substance_comparison.R
	
plots/top_words_bubble_plot_salvia.png: substance_comparison.R summaries.csv\
doses_data.csv experience_data.csv experience_urls.csv | dir
	Rscript substance_comparison.R
