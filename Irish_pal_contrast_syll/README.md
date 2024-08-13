- **raw_EdgeTrak_data_JPhon.RData** contains a dataframe called *raw.EdgeTrak.data* which includes all of our raw EdgeTrak tracings.
- **edgetrak_output_normalized_Irish_JPhon.RData** contains a dataframe called *edgetrak.minmax* which includes all of our raw EdgeTrak tracings, following range normalization.
- **tongue_peaks_Irish_JPhon.RData** contains a dataframe called *Tongue.peaks* which includes backness values for the highest point on the dorsum for each of our raw EdgeTrak tracings.
- **loess_plots_Irish_JPhon.R** generates loess plots, and plots of our raw ultrasound tracings. It needs to be run up to a certain point (around line 222) before running other scripts, because it defines some functions for polar transformation and plotting, imports fonts, etc.
- **rmssd_analysis_Irish_JPhon.R** produces our RMSSD analysis.
- **peak_backness_analysis_Irish_JPhon.R** produces our tongue body peak backness analysis.
- **raw_curves_alldata_Irish_JPhon.zip** is a .zip file containing PDF plots of raw EdgeTrak tracings, annotated for the highest point on the tongue surface.
- **Irish_pal_contrast_syll_loess_curves_maincomparisons.zip** is a .zip file containing PDF plots of loess-smoothed EdgeTrak tracings, focusing on comparisons of primary interest in the paper.
- **Irish_pal_contrast_syll_loess_curves_alldata.zip** is a .zip file containing PDF plots of loess-smoothed EdgeTrak tracings for every single segmental landmark and context. It's a large file, and so needs to be downloaded here: [Irish_pal_contrast_syll_loess_curves_alldata.zip](https://www.dropbox.com/s/4z3fbnu7paj28u4/Irish_pal_contrast_syll_loess_curves_alldata.zip?dl=1)
- **extreme_polar_angles.R** finds the point on each tracing which is most distant from the polar origin point estimated for each speaker, following [Mielke (2015)](
https://doi.org/10.1121/1.4919346), and checks for correlations between the backness and angle of that point and the backness value of the highest point on the dorsum (which we use as a dependent measure in our analysis). Corresponding PDF plots can also be examined directly here:
  - *backness_polarangle_corr.pdf*
  - *backness_polar_cart_corr.pdf*
  
***Contact***: rbennett@ucsc.edu
