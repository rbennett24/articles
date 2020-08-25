# Pitch extractor
# Presumes you're working with a LongSound file.
# Indirectly based off Christian DiCanio's formant extraction scripts.

form arguments

	comment Where are the .wav files located?

		text inDir C:\Users\Tiamat\Dropbox\Research\Mayan\Uspanteko\Uspanteko_NSF_project\Recordings\QA_data_for_analysis\FA_output\FA_textgrids\
		
#	comment Where do you want to save the text file?
#
#		text Folder C:\Users\Tiamat\Dropbox\Research\Mayan\Uspanteko\Uspanteko_NSF_project\Recordings\Scripts\Data analysis\R_scripts\QA_task\Article\
		
	comment What tier lists the target segments?

		integer targseg_tier 2
		
	comment What tier lists the target words?

		integer targwd_tier 4

	comment What tier lists the condition coding?

		integer coding_tier 3

	comment Select pitch range automatically for each subject using the De Looze & Rauzy (2009) parameters?
	comment WARNING: this may crash if used with long sound files. See Evanini et al. (2010).

		boolean Autorange 1

	#comment Manual pitch floor: (~75Hz for men, ~100Hz for women)

	#	integer pitMinManual 75

	#comment Manual pitch ceiling: (~300Hz for men, ~500Hz for women)

	#	integer pitMaxManual 300

	comment Downsample audio to 16 kHz before getting pitch measurements?

		boolean Downsample 1
	
	comment Smooth pitch tracings using Praat?

		boolean smoother 1
		integer smoothWin 15
		
	comment Extract time-normalized pitch tracks?
	
		boolean timeNormalized 1
		
	comment If time-normalized: How many measurements recorded from each pitch trace?
	
		integer measCountNorm 9
		
	#comment If NOT using time-normalization: How often (in ms) should a pitch measurement be recorded from the pitch trace?

	#	integer meas_freq_ms 4

	comment Include a date stamp on the output filename?

		boolean Datestamp 1

endform

folder$ = "'inDir$'Praat_measures"

if fileReadable (folder$)
		# Do nothing
	else
		createDirectory: folder$
endif

if datestamp = 1

	# Get a code for the date if desired.
	date$ = date$ ()
	mon$ = mid$ (date$, 5, 3)
	day$ = mid$ (date$, 9, 2)
	
	outputname$ = "'folder$'\Usp_QA_vowels_pitch_all_spkrs_'mon$''day$'.txt"
else
	outputname$ = "'folder$'\Usp_QA_vowels_pitch_all_spkrs.txt"
endif

#  Delete any pre-existing versions of the text output.
deleteFile (outputname$)

# Append a header of column names to the output file
header_text$ = "segment'tab$'code'tab$'word'tab$'start.time'tab$'token.code'tab$'interval.num'tab$'step'tab$'step.size'tab$'seg.dur'tab$'pitch.Hz'tab$'pitch.ERB'tab$'mean.pitch.Hz'tab$'mean.pitch.ERB'tab$'min.pitch.Hz'tab$'min.pitch.ERB'tab$'min.pitch.loc'tab$'min.pitch.loc.perc'tab$'max.pitch.Hz'tab$'max.pitch.ERB'tab$'max.pitch.loc'tab$'max.pitch.loc.perc'tab$'pitch.range.min'tab$'pitch.range.max'tab$'speaker"

appendFileLine: "'outputname$'", "'header_text$'"
#print 'header_text$'


# Set the pitch floor and pitch ceiling automatically.
if autorange = 1
	outputPitchTable$ = "'inDir$'\Usp_QA_pitch_tracks_spkr_pitch_range.Table"
	if fileReadable (outputPitchTable$)
		Read from file: outputPitchTable$
		Rename: "pitchLims"
	else
		# Get the list of all .wav files in the input directory.
		#
		Create Strings as file list... fileList 'inDir$'\*.wav
		select Strings fileList
		strnum = Get number of strings

		# Create a table that will store the speaker code
		# prefixes used in the list of .wav files.
		#
		Create Table with column names... "speakers" 0 spk

		# Iterate through the .wav files in the input directory.
		#
		counter = 1
		for k from 1 to strnum

			# Get speaker code for the current .wav file.
			# This assumes that the name of the TextGrid begins
			# with a three-symbol code that corresponds to the
			# speaker code.
			#
			select Strings fileList
			fstr$ = Get string... 'k'
			spkcode$ = left$(fstr$, 3)

			# Only store unique speaker codes, i.e. codes
			# that have not yet been entered in the table.
			#
			select Table speakers
			exists = Search column... spk 'spkcode$'
			if exists = 0
				Append row
				Set string value... counter spk 'spkcode$'
				counter +=1
			
			endif
		endfor

		# Deselect all objects.
		selectObject()

		# Remove the list of input .wav files.
		select Strings fileList
		Remove

		# Get the number of unique speaker codes.
		select Table speakers
		spkrnum = Get number of rows

		# Create a table for storing speaker pitch ranges.
		Create Table with column names... "pitchLims" spkrnum spkr pitchmin pitchmax

		# Iterate through speakers, working with their .wav files.
		for k from 1 to spkrnum

			# Get list of the speaker's .wav files, and load
			# them into the object window.
			#
			select Table speakers
			spklabel$ = Get value... k spk
			Create Strings as file list... spkfiles 'inDir$'\'spklabel$'_*.wav
			select Strings spkfiles
			spkrwavnum = Get number of strings

			for j from 1 to spkrwavnum
				select Strings spkfiles
				wavname$ = Get string... 'j'
				Read from file... 'inDir$'\'wavname$'
				rawWav = selected("Sound")
				# Downsample to 11,025 Hz (see Evanini et al. 2010)
				# This point is a weak link in terms of Praat's memory
				# limitations, i.e. it can cause crashing.
				Resample... 11025 50
				resampWav = selected("Sound")
				select rawWav
				Remove
			endfor
		
			# Select all of the speaker's .wav files, then
			# concatenate them (with no intervening silence).
			select all
			soundcount = numberOfSelected("Sound")

			for m from 1 to soundcount
				sound[m] = selected("Sound",m)
			endfor

			selectObject()
			for m from 1 to soundcount
				plus sound[m]
			endfor

			# Concatenate the speaker's .wav files.
			# This point is a weak link in terms of Praat's memory
			# limitations, i.e. it can cause crashing.
			#
			Concatenate
			concat_resamp_ID = selected("Sound")

			# Create a pitch object for the speaker's sound file.
			# Uses baseline range of 75-600Hz.
			#
			To Pitch... 0 75 600
			pitch_full_ID = selected("Pitch")

			# Get 15th and 65th quantiles for pitch for
			# that speaker.
			#
			qthirtyfive = Get quantile... 0 0 0.35 Hertz
			qsixtyfive = Get quantile... 0 0 0.65 Hertz

			# Determine pitch floor and pitch ceiling.
			pfloor = qthirtyfive * 0.72 - 10
			pceiling = qsixtyfive * 1.9 + 10

			pitMinSpk = pfloor
			pitMaxSpk = pceiling

			select Table pitchLims
			Set string value... k spkr 'spklabel$'
			Set string value... k pitchmin 'pitMinSpk'
			Set string value... k pitchmax 'pitMaxSpk'

			#print 'spklabel$'-'pitMinSpk'-'pitMaxSpk''newline$'
		
			# Clean up the files you've created.
			selectObject()
			for m from 1 to soundcount
				plus sound[m]
			endfor
			plus concat_resamp_ID
			plus pitch_full_ID
			Remove

			select Strings spkfiles
			Remove
		endfor

		select Table speakers
		Remove
		select Table pitchLims
		Save as text file: outputPitchTable$
	endif
endif

# Get the number of TextGrids in the directory.
Create Strings as file list... filesWAV 'inDir$'*.wav
Create Strings as file list... filesTextGrid 'inDir$'*.TextGrid
numfile = Get number of strings

# Iterate through each of the TextGrids
for k from 1 to numfile

	# Get the next TextGrid and its name
	select Strings filesTextGrid

	# Select an individual TextGrid to work with
	workingTGname$ = Get string... 'k'
	Read from file... 'inDir$'\'workingTGname$'

	gridname$ = selected$ ("TextGrid")
	select TextGrid 'gridname$'
	
	# Get the identification code for the speaker.
	# This assumes that the name of the TextGrid begins
	# with a three-symbol code that corresponds to the
	# speaker code, and ends with a 
	spkcode$ = left$(gridname$, 3)

	# Break off the filename-ending code which numbers each speaker's textgrid, using regex search
	splitLoc = index_regex(gridname$, "(\d)+$")
	nameend = index_regex(gridname$, "$")
	tgcode$ = mid$(gridname$, splitLoc,nameend)
	
	tokenCode$="'spkcode$'-'tgcode$'"
	

	# Set the pitch floor and pitch ceiling automatically.
	if autorange = 1

		# Selecting pitch values for this speaker from
		# the table of pitch ranges you made earlier.
		select Table pitchLims
		spkrRowIndex = Search column... spkr 'spkcode$'
		pitMin = Get value... spkrRowIndex pitchmin
		pitMax = Get value... spkrRowIndex pitchmax

	elif autorange = 0

		pitMin = pitMinManual
		pitMax = pitMaxManual

	endif


	# Process the individual sound file.
	#

	# Get the label of the corresponding wav file (Sound file).
	Read from file... 'inDir$'\'gridname$'.wav
	input_ID = selected("Sound")	
	select input_ID

	# Select the Sound file associated with the current TextGrid.
	# NOTE: the Sound and TextGrid files must have *exactly* the
	# same name.
	select input_ID


	# For downsampling.
	if downsample  = 1

		# Downsample to 16 kHz
		Resample... 16000 50

		# Get numerical index of the downsample output in object list,
		# so you can select it later if needed.
		resample_ID = selected("Sound")

	else
		select input_ID
		resample_ID = selected("Sound")
	endif



	# Extract pitch from the TextGrid you're working with.
	#
	select resample_ID
	To Pitch... 0 pitMin pitMax
	
	# Smooth the pitch contour of the TextGrid if desired.
	if smoother = 1
		rawPitch = selected("Pitch")
		select rawPitch
		Smooth: smoothWin
		smoothPitch = selected("Pitch")
		select rawPitch
		Remove
		select smoothPitch
	endif

	# Get numerical index of the pitch object in object list,
	# so you can select it later if needed.
	pitch_ID = selected("Pitch")

	# Get the number of intervals on the relevant tier
	select TextGrid 'gridname$'
	numint = Get number of intervals... 'targseg_tier'
	
	# Iterate through intervals, and find the labels you care about.
	for i from 1 to 'numint'

		select TextGrid 'gridname$'

		# Get label of the interval you're working with.
		interval$ = Get label of interval... 'targseg_tier' 'i'
		
		# Get timecodes for beginning and end of the interval.
		start = Get starting point... 'targseg_tier' 'i'
		end = Get end point... 'targseg_tier' 'i'

		# Get the word label
		# For this script, we're actually only getting labels for target words, non-target
		# words should return a null label.
		int_mdpt = start + (end-start)/2
		intWd = Get interval at time... targwd_tier int_mdpt
		wdlab$ = Get label of interval... targwd_tier intWd

		# Ignore all empty intervals.
		if wdlab$ = ""
			# Do nothing
		else

			# Get the annotation label/condition coding.
			intcond = Get interval at time... coding_tier int_mdpt
			condlab$ = Get label of interval... coding_tier intcond

			# Place conditions on labels.
			# Here: we only care about labels that start with vowel symbols
			startChar$ = left$("'interval$'",1)
			if (startChar$ = "A" or startChar$ = "E" or startChar$ = "I" or startChar$ = "O" or startChar$ = "U")

				# Get overall duration of interval, and figure out the durations
				# of the sub-intervals that you're going to analyze.
				# This is where it matters if you choose time-normalization or not.
				int_duration = (end - start)
						
				if timeNormalized = 1
					slice_count = measCountNorm
					sample_step = int_duration/slice_count
				else
					slice_count = floor(int_duration*1000/meas_freq_ms)
					sample_step = meas_freq_ms/1000
				endif

				
				# Get overall pitch for the interval in question, and min/max
				select pitch_ID
				sample_start = start
				sample_end = end
				
				pitch_mean_overall_Hz = Get mean... 'sample_start' 'sample_end' Hertz
				pitch_mean_overall_ERB = Get mean... 'sample_start' 'sample_end' ERB
				
				pitch_min_overall_Hz = Get minimum... 'sample_start' 'sample_end' Hertz Parabolic
				pitch_min_overall_ERB = Get minimum... 'sample_start' 'sample_end' ERB Parabolic

				pitch_max_overall_Hz = Get maximum... 'sample_start' 'sample_end' Hertz Parabolic
				pitch_max_overall_ERB = Get maximum... 'sample_start' 'sample_end' ERB Parabolic
				
				minLocRaw = Get time of minimum: 'sample_start', 'sample_end', "Hertz", "Parabolic"
				minLoc = (minLocRaw-sample_start) * 1000
				minLocPerc = minLoc/(int_duration*1000)
				maxLocRaw = Get time of maximum: 'sample_start', 'sample_end', "Hertz", "Parabolic"
				maxLoc = (maxLocRaw-sample_start) * 1000
				maxLocPerc = maxLoc/(int_duration*1000)

				# Now get pitch measurements, averaged over each sub-interval.
				#
				# Alternatively, you could extract all of the pitch values, and worry about
				# sampling/averaging/etc. in R.
				#
				# The "start+" portion is needed to make sure that you're starting
				# at the beginning of the interval rather than the beginning of the file.
				#
				
				select pitch_ID

				for step_num from 0 to slice_count-1

					sample_start = start+(sample_step * step_num)
					sample_end = start+(sample_step * (step_num+1))
					# The last interval should be computed even if it's smaller than the desired window.
					if sample_end > end
						sample_end_out = end
					else
						sample_end_out = sample_end
					endif
					step_count = step_num+1
					
					step_size = sample_end_out-sample_start
				
					step_pitch_mean_Hz = Get mean... 'sample_start' 'sample_end_out' Hertz
					step_pitch_mean_ERB = Get mean... 'sample_start' 'sample_end_out' ERB

					outDur = 1000*int_duration
					outStart = 1000*start

					pitch_text_raw$ = "'interval$''tab$''condlab$''tab$''wdlab$''tab$''outStart:3''tab$''tokenCode$''tab$''i''tab$''step_count''tab$''step_size:4''tab$''outDur:3''tab$''step_pitch_mean_Hz:3''tab$''step_pitch_mean_ERB:4''tab$''pitch_mean_overall_Hz:3''tab$''pitch_mean_overall_ERB:4''tab$''pitch_min_overall_Hz:3''tab$''pitch_min_overall_ERB:4''tab$''minLoc:3''tab$''minLocPerc:3''tab$''pitch_max_overall_Hz:3''tab$''pitch_max_overall_ERB:4''tab$''maxLoc:3''tab$''maxLocPerc:3''tab$''pitMin:4''tab$''pitMax:4''tab$''spkcode$'"
					
					# Clean up undefined pitch values and apostrophes.
					pitch_text_intermed$ = replace$ ("'pitch_text_raw$'", "--undefined--", "NA", 0)
					pitch_text_intermed$ = replace$ ("'pitch_text_intermed$'", "", "NA", 0)
					pitch_text$ = replace$ ("'pitch_text_intermed$'", "'", "7", 0)

					appendFileLine: "'outputname$'", "'pitch_text$'"
					#print 'pitch_text$'
				
				
				## There was a problem here which WASN'T throwing an error involving the relative order of the following endfor and endif statements. Fixed now, looks better, but be aware of it.
				endfor
			endif
		endif
	endfor

	# Get rid of any sound files or formant files that have been created.
	#
	select TextGrid 'gridname$'
	plus input_ID
	plus pitch_ID
	plus resample_ID
	#plus extract_ID
	Remove

	select Strings filesTextGrid
endfor

select Table pitchLims
plus Strings filesWAV
plus Strings filesTextGrid
#select all
Remove