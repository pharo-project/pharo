documentation
monitor
	^ HelpTopic
		title: 'Monitor & Ombu Files'
		contents: '
MONITOR & OMBU FILES

Epicea Monitor logs code changes and other IDE events. Such data is placed in .ombu files in a local directory. Each time a Pharo image opens, Epicea Monitor logs code changes into a new ombu file. This avoids the risk of loosing the data in an ombu file: two Pharo images will always write into different ombu files. 

Logging the code changes into different files can have a negative consequence: the changes of a single development session might be segmented into several log files. This will happen in the case where the developer had to close and open several times the Pharo image.
'