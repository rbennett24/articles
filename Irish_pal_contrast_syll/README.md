- **raw_EdgeTrak_data_JPhon.RData** contains a dataframe called *raw.EdgeTrak.data* which includes all of our raw EdgeTrak tracings.
- **edgetrak_output_normalized_Irish_JPhon.RData** contains a dataframe called *edgetrak.minmax* which includes all of our raw EdgeTrak tracings, following range normalization.
- **tongue_peaks_Irish_JPhon.RData** contains a dataframe called *Tongue.peaks* which includes backness values for the highest point on the dorsum for each of our raw EdgeTrak tracings.
- **loess_plots_Irish_JPhon.R** generates loess plots, and plots of our raw ultrasound tracings. It needs to be run up to a certain point (around line 366) before running other scripts, because it defines some functions for polar transformation and plotting, imports fonts, etc.
- **rmssd_analysis_Irish_JPhon.R** produces our RMSSD analysis.
- **peak_backness_analysis_Irish_JPhon.R** produces our tongue body peak backness analysis.
- **Irish_pal_contrast_syll_loess_curves_alldata.zip** is a .zip file containing PDF plots of loess-smoothed EdgeTrak tracings.

***Contact***: rbennett@ucsc.edu