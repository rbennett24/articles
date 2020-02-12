######################
# Script for drawing waveforms, spectrograms, pitch, and associated text annotation
# from sound files with TextGrids in the Praat object window.
# Before running this script, you need to manually select the sounds you'd
# like to draw in the object window itself.
#
# The output of the script will be a PNG or EPS file of the waveform corresponding
# to each selected sound (over its entire duration).
#
# Written Jan. 2016 by Ryan Bennett (Yale University)

#Dialogue box
form Where do you want to save the output image files?
comment Folder:
	text Folder C:\Users\Tiamat\Dropbox\Research\Mayan\Uspanteko\Uspanteko_NSF_project\Articles\JIPA_description\wav_files_images_JIPA\
comment Produce an .eps file instead of a .png file?
	boolean eps 0
comment Maximum frequency of the spectrogram?
	positive upperlim 5000
# http://www.fon.hum.uva.nl/praat/manual/Sound__To_Spectrogram___.html
comment Timestep for the spectrogram?
	positive timestep 0.001
comment Windowlength for the spectrogram?
	positive windowSpec 0.0075
# comment Waveform color?
	# text wavCol Grey
#comment Pitch color?
#	text pitchCol Red
comment Draw segment durations?
	boolean segDurs 1
comment Draw pitch?
	boolean drawPitch 0
comment Select pitch range:
	real low_F0 80
	real high_F0 250
endform

#Select each file
n = numberOfSelected ("Sound")

for i to n
    sound'i' = selected ("Sound", i)
endfor

graphbottom = 0
specWavBound = graphbottom+3
specTGBound = 5.25
basexMin = 0
basexMax = 7

for i to n
	# Clear drawing window
	Erase all

	# Set font size. Changing this will make everything messy.
	do ("24")

	# Draw the waveform
	select sound'i'
	Scale peak: 0.99
	name$ = selected$ ("Sound")

	# Set waveform color and line thickness
	do ("Grey")
	Line width: 1

	# Draw waveform
	# Viewport parameters: left right top bottom
	Select outer viewport: basexMin, basexMax, graphbottom, specWavBound
	select Sound 'name$'
	Draw: 0, 0, 0, 0, "no", "Curve"
	
	# Reset color
	do ("Black")


	if drawPitch = 1
		#Extract pitch track
		select Sound 'name$'
		To Pitch... 0 'low_F0' 'high_F0'
		#To Pitch (ac): 0, 'low_F0', 15, "yes", 0.03, 0.75, 0.01, 0.35, 0.14, 'high_F0'
		
		Select outer viewport: basexMin, basexMax, graphbottom, specWavBound
		select Pitch 'name$'

		# Draw pitch
		do ("Line width...", 10)
		Draw... 0 0 low_F0 high_F0 no

		# Draw axes and boxes as needed
		do ("Line width...", 1)
		Select outer viewport: basexMin, basexMax, graphbottom, specWavBound

		#Draw inner box
		Marks left... 5 no yes no
		Marks left... 3 no yes no
		One mark left: high_F0, "no", "yes", "no", string$(high_F0)
		One mark left: (high_F0-low_F0)/2+low_F0, "no", "yes", "no", string$((high_F0-low_F0)/2+low_F0)
		
		#Remove pitchtrack object after drawing.
		select Pitch 'name$'
		Remove
	endif

	# From this point on, the dimensions of the drawing window were basically set by trial and error.
	# It is very puzzling how and why they work the way they do, and why they interact with font size.

	specBottom = specTGBound-0.5
	specTop = specWavBound-1.8
	textBottom = specWavBound+2.25

	# Create spectrogram
	select sound'i'
	To Spectrogram: windowSpec, upperlim, timestep, 20, "Gaussian"
	
	#Draw spectrogram.
	Select outer viewport: basexMin, basexMax, specTop, specBottom
	select Spectrogram 'name$'
	Paint: 0, 0, 0, 0, 80, "yes", 50, 6, 0, "no"

	# Draw text annotations
	#Text left: "no", "Frequency (Hz)"
	One mark left: upperlim, "no", "yes", "no", string$(upperlim)
	One mark left: 0, "no", "yes", "no", "0"
	
	# Draw box around annotations
	Line width: 1
	Select outer viewport: basexMin, basexMax, specTop, textBottom
	Draw inner box

	# Set position of box for annotations (I have no idea why these parameter settings do what they do)
	#Line width: 5
	Select outer viewport: basexMin, basexMax, specWavBound-2.15, textBottom
	select TextGrid 'name$'
	Draw: 0, 0, "no", "no", "no"

	# Draw segment-level boundaries (vertical lines)
	Line width: 10
	Select outer viewport: basexMin, basexMax, -0.1, specBottom
	select TextGrid 'name$'
	# http://www.fon.hum.uva.nl/praat/manual/Script_for_TextGrid_boundary_drawing.html
	#Line width: 50
	intNum = Get number of intervals: 1
	for k to intNum-1
	    #Line width: 50
	    t = Get end point: 1, k
	    One mark bottom: t, "no", "no", "yes", ""
	endfor
	
	# Write segmental durations for non-empty intervals
	if segDurs = 1
	Select outer viewport: basexMin, basexMax, -0.1, textBottom
		for k to intNum
				label$ = Get label of interval... 1 k
				if label$ <> ""
					start = Get starting point... 1 k
					end = Get end point... 1 k
					duration = end - start
					dur_ms = round('duration'*1000)
					dur_label_place = start+(end-start)/2
					Text special... "'dur_label_place'" centre -1.5 top Times 18 0 'dur_ms'
					#Text bottom: "no", "Duration (ms)"
				endif
		endfor
	else
		# Draw 50ms reference lines
		Line width: 1
		Marks bottom every: 0.001, 50, "yes", "yes", "no"
	endif

	# Draw box around annotations
	Line width: 1
	Select outer viewport: basexMin, basexMax, -0.1, specBottom
	Draw inner box
	
	if eps = 1
		Write to EPS file... 'folder$'\'name$'_spec.eps
	else
		Save as 300-dpi PNG file: "'folder$'\'name$'_spec.png"
	endif

	select Spectrogram 'name$'
	Remove

	Erase all
endfor

#Restore selection
if n >= 1
    select sound1
    for i from 2 to n
       plus sound'i'
    endfor
endif
