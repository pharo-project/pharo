accessing
getent: aString
	"The tgetent routine loads the entry for name. 
	It returns 1 on success, 
	0 if there is no such entry,
	 and -1 if the terminfo database could not be found."
	
	"result := (PipeableOSProcess command: 'tput ', aString) output asInteger.
	result = 1 ifTrue: [ ^ self ].
	result = 0 ifTrue: [ Error signal: 'No such tget entry: ', aString aString ].
	result = -1 ifTrue: [ Error signal: 'Could not find terminfo database' ]."
	
	Error signal: 'Cannot interact with tput yet'.
	