form arguments

	comment Where are the TextGrids and .wav files located?
		text inDir C:\Users\Tiamat\Dropbox\Research\Irish\Irish_ultrasound_shared\Scripts\Praat scripts\Carnie_volume\Formants_Carnie\Praat_input\

	comment What tier are the target segments on?
		integer seg_tier 3

	comment How many formant samples should be taken from each interval?
		integer form_slice_count 11
	
	comment Window length (in seconds):
		real wlen 0.0475
	
	comment To help measure formants at edges of vowel intervals, add padding (= 2 * window length) 
	comment to each side of extracted interval?
		boolean addpad 1
		
	comment F1 reference value? (550 recommended for adult women)
		integer f1_ref 250
		
	comment F2 reference value? (1650 recommended for adult women)
		integer f2_ref 1400

	comment F3 reference value? (2750 recommended for adult women)
		integer f3_ref 2600

endform

outputname_form$ =  "'inDir$'\FACL_F2_tracks_all_spkrs.txt"

#  Delete any pre-existing versions of the text outputs.
deleteFile (outputname_form$)

# Append headers of column names to the output files
form_header_text$ = "vowel.code'tab$'token.code'tab$'dur'tab$'step'tab$'stepSize'tab$'start.time'tab$'end.time'tab$'
...formant'tab$'freq'tab$'
...speaker"
appendFileLine: outputname_form$, form_header_text$

# Get the list of all .wav files in the input directory.
Create Strings as file list... fileList 'inDir$'\*.wav
select Strings fileList
strnum = Get number of strings

# Create a table that will store the speaker code
# prefixes used in the list of .wav files.
Create Table with column names... "speakers" 0 spk

# Iterate through the .wav files in the input directory.
counter = 1
for k from 1 to strnum

	# Get speaker code for the current .wav file.
	# This assumes that the name of the TextGrid begins
	# with an 11-symbol code that corresponds to the
	# speaker code.
	select Strings fileList
	fstr$ = Get string... 'k'
	spkcodeTMP$ = left$(fstr$, 11)

	# Only store unique speaker codes, i.e. codes
	# that have not yet been entered in the table.
	select Table speakers
	exists = Search column... spk 'spkcodeTMP$'
	if exists = 0
		Append row
		Set string value... counter spk 'spkcodeTMP$'
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

# Iterate through speakers, working with their .wav files.
for snum from 1 to spkrnum

	# Get list of the speaker's .wav files, and load
	# them into the object window.
	select Table speakers
	spkcode$ = Get value... snum spk

	Create Strings as file list... spkfiles 'inDir$'\'spkcode$'_*.wav
	select Strings spkfiles
	spkrwavnum = Get number of strings
	
	# Read .wav files in to the object window.
	for j from 1 to spkrwavnum
		select Strings spkfiles
		wavname$ = Get string... 'j'
		Read from file... 'inDir$'\'wavname$'
	endfor

	# Iterate through each of the TextGrids
	for k from 1 to spkrwavnum
		select Strings spkfiles
		wavname$ = Get string... 'k'
		base$ = replace$(wavname$, ".wav", "", 0)
		tgname$ = "'base$'.TextGrid"
		
		# Select an individual TextGrid to work with
		if fileReadable("'inDir$'\'tgname$'")
			Read from file... 'inDir$'\'tgname$'
			gridname$ = selected$ ("TextGrid")
		else
			writeInfoLine: "File 'tgname$' doesn't exist! You have unpaired .wav files. Quitting script."
			exitScript()
		endif

		# Get the number of intervals on the relevant tier
		select TextGrid 'gridname$'
		numint = Get number of intervals... 'seg_tier'
		select TextGrid 'gridname$'

		# Iterate through intervals, and find the labels you care about.
		for i from 1 to 'numint'
			select TextGrid 'gridname$'
			
			# Get label of the interval you're working with.
			interval$ = Get label of interval... 'seg_tier' 'i'

			# Place conditions on labels.
			# Here: we only care about labels that start with
			# a particular string. We also ignore all empty labels.
			if ((startsWith(interval$,"w") or startsWith(interval$,"j") or startsWith(interval$,"0")))

				# Set token code
				tokenCode$ = "'interval$'-'base$'"

				# Get timecodes for beginning and end of the interval.
				start = Get starting point... 'seg_tier' 'i'
				end = Get end point... 'seg_tier' 'i'
				
				# Get overall duration of interval, and figure out the durations
				# of the sub-intervals that you're going to analyze.
				int_duration = end - start
				int_duration_out = int_duration*1000
				form_sample_step = int_duration / form_slice_count
				
				# Get original .wav file
				select all
				sound[k] = selected("Sound",k)
				select sound[k]
				
				# Extract section of the .wav file consisting of the vowel
				# of interest, as marked on the TextGrid.
				if addpad = 0
					vchunk = Extract part: start, end, "rectangular", 1, "yes"
				elif addpad = 1
					vchunk = Extract part: start - 2 * wlen, end + 2 * wlen, "rectangular", 1, "yes"
				endif
								
				# Create formant object for extracted vowel.
				# Window length and F1-F3 reference levels are set as script parameters.
				# All other settings here are the defaults.
				select vchunk
				# To Formant (burg): time step, max # of formants, formant ceiling, window length, pre-emphasis
				To Formant (burg): 0, 5.5, 4800, wlen, 50
				formants$ = selected$ ("Formant")
				
				# Track: # of tracks, F1 ref, F2 ref, F3 ref, F4 ref, F5 ref, freq cost, bandwidth cost, transition cost
				# Last three have default values of 1; raising them should in principle reduce jump costs
				Track: 3, f1_ref, f2_ref, f3_ref, 3850, 4950, 2, 2, 2
				formantTrack$ = selected$ ("Formant")
				
				# Check that tracks for F1/F2 aren't outside the expected range,
				# and re-run formant analysis with more formants, higher ceiling, and
				# downward adjustment of F1-F3 reference levels.
				select Formant 'formantTrack$'
				f1Max = Get maximum: 1, 0, 0, "hertz", "parabolic"
				f2Max = Get maximum: 2, 0, 0, "hertz", "parabolic"
				# f1Min = Get minimum: 1, 0, 0, "hertz", "parabolic"
				f2Min = Get minimum: 2, 0, 0, "hertz", "parabolic"
				f3Min = Get minimum: 3, 0, 0, "hertz", "parabolic"
				
				if (f2Max >= 2200 or f1Max >= 800 or f2Min <= 900)
					select vchunk
					To Formant (burg): 0, 6.5, 5250, wlen, 50
					formants$ = selected$ ("Formant")

					if (f1Max >= 800)
						Track: 3, f1_ref-100, f2_ref, f3_ref, 3850, 4950, 2, 2, 2
						formantTrack$ = selected$ ("Formant")
					elif (f2Max >= 2200)
						Track: 3, f1_ref-100, f2_ref-200, f3_ref, 3850, 4950, 2, 2, 2
						formantTrack$ = selected$ ("Formant")
					elif (f2Min <= 900)
						Track: 3, f1_ref, f2_ref+200, f3_ref+200, 3850, 4950, 2, 2, 2
						formantTrack$ = selected$ ("Formant")
					else
						Track: 3, f1_ref, f2_ref, f3_ref, 3850, 4950, 2, 2, 2
						formantTrack$ = selected$ ("Formant")
					endif
				endif
				
				# Take formant measurements over small windows of the formant
				# tracks.
				for step_num from 0 to (form_slice_count-1)

					sample_start = start + form_sample_step * step_num
					sample_end = start + form_sample_step * (step_num+1)
					step_count = step_num+1
		
					# Get mean formant values for F1,F2,F3 over each subinterval.
					select Formant 'formantTrack$'
					f1_Hz = Get mean... 1 'sample_start' 'sample_end' Hertz
					f2_Hz = Get mean... 2 'sample_start' 'sample_end' Hertz
					f3_Hz = Get mean... 3 'sample_start' 'sample_end' Hertz
					
					# Prepare formant output text.
					if f1_Hz = undefined
						f1_text$ = "'interval$''tab$''tokenCode$''tab$''int_duration_out:2''tab$''step_count''tab$''form_sample_step:5''tab$''sample_start:3''tab$''sample_end:3''tab$'F1'tab$'NA'tab$''spkcode$'"
					else
						f1_text$ = "'interval$''tab$''tokenCode$''tab$''int_duration_out:2''tab$''step_count''tab$''form_sample_step:5''tab$''sample_start:3''tab$''sample_end:3''tab$'F1'tab$''f1_Hz:2''tab$''spkcode$'"
					endif
				
					appendFileLine: outputname_form$, f1_text$


					if f2_Hz = undefined
						f2_text$ = "'interval$''tab$''tokenCode$''tab$''int_duration_out:2''tab$''step_count''tab$''form_sample_step:5''tab$''sample_start:3''tab$''sample_end:3''tab$'F2'tab$'NA'tab$''spkcode$'"
					else
						f2_text$ = "'interval$''tab$''tokenCode$''tab$''int_duration_out:2''tab$''step_count''tab$''form_sample_step:5''tab$''sample_start:3''tab$''sample_end:3''tab$'F2'tab$''f2_Hz:2''tab$''spkcode$'"
					endif
				
					appendFileLine: outputname_form$, f2_text$

					
					if f3_Hz = undefined
						f3_text$ = "'interval$''tab$''tokenCode$''tab$''int_duration_out:2''tab$''step_count''tab$''form_sample_step:5''tab$''sample_start:3''tab$''sample_end:3''tab$'F3'tab$'NA'tab$''spkcode$'"
					else
						f3_text$ = "'interval$''tab$''tokenCode$''tab$''int_duration_out:2''tab$''step_count''tab$''form_sample_step:5''tab$''sample_start:3''tab$''sample_end:3''tab$'F3'tab$''f3_Hz:2''tab$''spkcode$'"
					endif
				
					appendFileLine: outputname_form$, f3_text$		

				endfor
			endif
		endfor
	endfor

select all
minus Table speakers
Remove
	
endfor

# Clean up
select Table speakers
Remove