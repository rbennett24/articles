#Dialogue box
form Where do you want to save the files?
comment Folder:
text Folder C:\Users\Tiamat\Dropbox\Research\Mayan\Uspanteko\Uspanteko_NSF_project\Articles\Intonation\Images\

comment Select pitch range
	real low_F0 45
	real high_F0 175
#	real low_int 30
#	real high_int 90

endform

savepath$ =  "'folder$'"


#Select each file
n = numberOfSelected ("Sound")

for i to n
    sound'i' = selected ("Sound", i)
endfor

for i to n
	select sound'i'
	Scale peak: 0.99
	# Make the waveform larger
	name$ = selected$ ("Sound")

#Extract pitch track
	select Sound 'name$'
	To Pitch... 0 'low_F0' 'high_F0'
	#To Pitch (ac): 0, 'low_F0', 15, "yes", 0.03, 0.75, 0.01, 0.35, 0.14, 'high_F0'


do ("18")
#Draw waveform
#	Select outer viewport... 1 9.5 1.4 3.9
#	Select outer viewport... 1 8.5 0 3.75
	#Select outer viewport... 1 8.5 3 6.75
	Select outer viewport... 1 8.5 2 6.25

	select Sound 'name$'
	plus TextGrid 'name$'

	do ("Grey")

	Draw... 0 0 yes yes no

	do ("Black")





#Draw Pitch Track
	#Select outer viewport... 1 8.5 0 3.75
	#Select outer viewport... 1 8.5 3 6.75
	Select outer viewport... 1 8.5 2 6.25
	select Pitch 'name$'
#	plus TextGrid 'name$'

	do ("Line width...", 10)
#	do ("Grey")
#	do ("Red")
#	do ("Dotted line")
#	do ("Dashed line")

	Draw... 0 0 low_F0 high_F0 no

	do ("Line width...", 1)
#	do ("Grey")
	do ("Black")
	do ("Solid line")

	Draw inner box

#	Draw separately... 0 0 low_F0 high_F0 yes yes yes

	Marks left... 5 no yes no
	Marks left... 3 yes yes no

	Text left... yes Pitch (Hz)
	Text bottom... yes Duration (ms)

	#Remove pitchtrack object after drawing.
	select Pitch 'name$'
	Remove


do ("18")
#Draw vowel duration
select TextGrid 'name$'

# check how many intervals there are in the selected tier:
numberOfIntervals = Get number of intervals... 1

# loop through all the intervals
for interval from 1 to numberOfIntervals

	label$ = Get label of interval... 1 interval

# if the interval has some text as a label, then calculate the duration.

	if label$ <> ""

		start = Get starting point... 1 interval
		end = Get end point... 1 interval
		duration = end - start
		dur_ms = round('duration'*1000)
		dur_label_place = start+(end-start)/2

#		Text... "'dur_label_place'" Centre 0 Top 'dur_ms'ms

#		Text special... "'dur_label_place'" centre -1.5 top Times 14 0 'dur_ms'ms
		Text special... "'dur_label_place'" centre low_F0 top Times 18 0 'dur_ms'

	endif

endfor

#Text bottom... yes Duration (ms)


do ("18")
# Draw TextGrid

	do ("Black")

	#Select outer viewport... 1 8.5 0 3.75
	#Select outer viewport... 1 8.5 3 6.75
	Select outer viewport... 1 8.5 2 6.25

	select TextGrid 'name$'
	Draw... 0 0 yes no no

do ("14")

Save as 600-dpi PNG file: "'folder$'\'name$'.png"

Erase all

endfor

#Restore selection
if n >= 1
    select sound1
    for i from 2 to n
       plus sound'i'
    endfor
endif

