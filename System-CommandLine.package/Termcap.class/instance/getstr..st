accessing
getstr: aString
	^ cache
		at: aString 
		ifAbsentPut: [ 
			"(PipeableOSProcess command: 'tput ', aString) output"
			^ Error signal: 'Terminal capability ', aString, ' not found' ]