accessing arguments
optionAt: aString ifPresent: absentBlock
	^ self commandLine 
		optionAt: aString ifPresent: absentBlock