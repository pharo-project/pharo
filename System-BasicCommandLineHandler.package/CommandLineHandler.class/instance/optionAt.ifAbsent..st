accessing arguments
optionAt: aString ifAbsent: absentBlock
	^ self commandLine 
		optionAt: aString ifAbsent: absentBlock