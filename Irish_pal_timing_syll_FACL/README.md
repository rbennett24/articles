Here are things you're likely to use:
- **tongue_peaks_Irish_FACL.RData** contains a dataframe called *Tongue.peaks* which includes backness values for the highest point on the dorsum for each of our raw EdgeTrak tracings.
- **find_gesture_extrema_FACL.R** produces our timing analysis.
- **Irish_pal_contrast_syll_loess_curves_alldata.zip** is a .zip file containing PDF plots of loess-smoothed EdgeTrak tracings.
- **FastTrack_formant_data.Rdata** contains a dataframe called *formants* which includes raw formant measurements of our data, produced by the [Fast Track Praat plugin](https://github.com/santiagobarreda/FastTrack), as well as log-additive regression normalized formant values.
- **F2_trajectory_analysis_FACL.R** produces our formant plots. It depends on **FastTrack_formant_data.Rdata** to load the *formants* dataframe. 

Here are things you're *less* likely to use:
- **raw_EdgeTrak_data_FACL.RData** contains a dataframe called *raw.EdgeTrak.data* which includes all of our raw EdgeTrak tracings.
- **edgetrak_output_normalized_Irish_FACL.RData** contains a dataframe called *edgetrak.minmax* which includes all of our raw EdgeTrak tracings, following range normalization.
- **FACL_F2_extraction_v3.praat** is a hand-made formant extraction script.
- **FACL_F2_tracks_all_spkrs.txt** is a tab-separated list of measured formant values, created by **FACL_F2_extraction_v3.praat** instead of Fast Track.

Here are things you're *not* likely to use:
- **FastTrack_csvs.zip** contains CSV files produced by the [Fast Track Praat plugin](https://github.com/santiagobarreda/FastTrack). If you want to work with these directly for some reason, instead of loading **FastTrack_formant_data.Rdata**, you'll also need **file_information.csv**.
- **formant_Praat_files.zip** contains the audio files and TextGrids used for our formant analysis. It's a large file, and so needs to be downloaded here: [formant_Praat_files.zip
 ](https://www.dropbox.com/scl/fi/2ktl7bv8itp1b5oqytpd1/FastTrack_csvs.zip?rlkey=ac6enume0fphd0sxmkf0f1s3m&dl=1)

***Contact***: rbennett@ucsc.edu
