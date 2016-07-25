accessing arguments
optionAt: aString ifPresent: presentBlock ifAbsent: absentBlock
	^ self commandLine 
		optionAt: aString ifPresent: presentBlock ifAbsent: absentBlock