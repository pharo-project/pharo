accessing
getflag: aString
	"^ (PipeableOSProcess command: 'tput ', aString) output"
	Error signal: 'Cannot interact with tput yet'