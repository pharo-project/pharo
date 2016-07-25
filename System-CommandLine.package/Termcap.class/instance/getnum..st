accessing
getnum: aString
	"^ (PipeableOSProcess command: 'tput ', aString) output asInteger"
	^ Error signal: 'Cannot interact with the terminal yet'.