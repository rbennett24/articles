##  This script measures the duration, intensity, and center of gravity of intervals, and appends the
##  results to a text file. That file will be called "release_log.txt".
##  To run this script, you will have to have a bunch of sound files with accompanying text grids. The
##  locations of intervals to be measured must be marked in tier 2 of the textgrid. Anything with a
##  label in that tier will be logged. In order for COG to be measured correctly, the sound file must
##  be sampled at 22050 Hz. It will be stop-band filtered (band from 0 to 1 kHz) to keep voicing
##  our of the calculation.

##  For the FACL release analysis, I don't trust the righthand boundaries, so duration won't be a useful 
##  measure. We're going to focus on burst properties, so this script will work with a window of 15 ms after the release
##  for the purposes of the intensity and COG analyses.

##  Specify the directory containing the sound files and the script log in the next lines:

directory$ = ""
script_directory$ = ""

##  The next line deletes any pre-existing variant of the log:

filedelete 'script_directory$'release_log.txt

##  Now I'm going to make a variable called "header_row$", then write that variable to the log file:

header_row$ = "Filename" + tab$ + "reldur" + tab$ + "relint" + tab$ + "cog" + newline$
header_row$ > 'script_directory$'release_log.txt

##  Now we make a list of all the sound files in the directory we're using, and put the number of
##  filenames into the variable "number_of_files":

Create Strings as file list...  list 'directory$'*.wav
number_files = Get number of strings

# Then we set up a "for" loop that will iterate once for every file in the list:

for j from 1 to number_files

     #    Query the file-list to get the first filename from it, then read that file in:

     select Strings list
     current_token$ = Get string... 'j'
     Read from file... 'directory$''current_token$'

     #    Here we make a variable called "object_name$" that will be equal to the filename minus the ".wav" extension:

     object_name$ = selected$ ("Sound")

     #    Now we do an intensity analysis on the current sound file. The default settings are 100 0, meaning a pitch floor
     #    of 100 Hz and a time step of 0. Setting the pitch floor actually defines the window for the intensity analysis.
     #    Apparently Praat needs three cycles to infer a frequency. 1/100 = 10 ms, times 3 = 30 ms. This is the window size.
     #    Time step = 0 means Praat does the default thing, which is make time steps one quarter the size of the window,
     #    so in this case 7.5 ms. Look at durations in your data and decide what the window size should be. In the 2012
     #    paper we used a pitch floor = 300, meaning a window 10 ms, since our durations were very short, and I see a 
     #    JIPA paper by Malmi et al. (2023) that uses this window size to get release COG.

     To Intensity... 100 0

	 #	  Now we create a filtered sound that we will use for the COG measurements, then create a spectrum object from that sound.

	 select Sound 'object_name$'

	 Filter (stop Hann band)... 0 1000 100

     #    Now we get the corresponding TextGrid and read it in:

     Read from file... 'directory$''object_name$'.TextGrid

     #    Now we query the TextGrid to find out how many intervals there are in tier 2, storing
     #    that number in a variable called "number_of_intervals".  This is used to set up a for loop
     #    that will be used to go through each of the intervals and measure it if its label is "r".

     select TextGrid 'object_name$'
     number_of_intervals = Get number of intervals... 2
     for b from 1 to number_of_intervals
         select TextGrid 'object_name$'
          interval_label$ = Get label of interval... 2 'b'
          if interval_label$ <> ""
               begin = Get starting point... 2 'b'
               end = Get end point... 2 'b'
	           windowend = begin + 0.015 
               select Intensity 'object_name$'
               relint = Get mean... begin windowend
               reldur = (windowend - begin) * 1000

	       # GK: We want a spectrum for the current interval, so let's extract the interval first.
		   # We have to do this because the Get cog command gives you the average for an entire sound;
           # it doesn't allow you to specify the interval.

	       	   select Sound 'object_name$'_band

	       # GK: There is one problem, though: The FFT that calculates the spectrum from the signal
	       # uses a certain window length. The beginning and end of a sound up to the length of the
	       # analysis window are excluded from the calculation. This means, in effect, that if we
	       # extract the exact interval, and the window legnth is set to 0.005 s, the first and
	       # last 0.005 s of the interval are not included in the COG calculation.
	       # This is probably undesired, given that your intervals are fairly short. As a solution
	       # we don't use the exact interval boundaries, but extend the interval into both directions.

		   #JP: I'm not sure about this - I think all of an interval figures in the calculation of the FFT.
		   #I read somewhere that for Gaussian windows Praat goes outside of the window a bit to avoid this
           #issue. Or maybe the use of a rectangular window below obviates this. So for now I'm not using the following.

       		#  my_begin = begin - 0.005
		    #  my_end = windowend + 0.005

	        #  Extract part... 'my_begin' 'my_end' Rectangular 1.0 yes

			   Extract part... 'begin' 'windowend' Rectangular 1.0 yes

	       # GK: Now we can create the Spectrum object for the new sound object and calculate the COG:

	       	   To Spectrum... yes

	           cog = Get centre of gravity... 2

	           fileappend "'script_directory$'release_log.txt" 'object_name$''tab$''reldur:0''tab$''relint:1''tab$''cog:0''newline$'
          endif

     endfor

     #  By this point, we have gone through all the intervals for the current sound object and
     #  textgrid, and written all the appropriate values to our log file.  We are now ready to go on to
     #  the next file, so we close can get rid of any objects we no longer need, and end our for loop

     select all
     minus Strings list
     Remove
endfor

# And at the end, a little bit of clean up and a message to let you know that it's all done.

select all
Remove
clearinfo
print All files have been processed.

## written by Katherine Crosswhite
## crosswhi@ling.rochester.edus

## additions by Jaye Padgett

## GK: Additions by Gero Kunter (kunter@anglistik.uni-siegen.de)
