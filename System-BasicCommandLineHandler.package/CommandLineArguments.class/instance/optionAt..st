accessing
optionAt: aString
	^ self optionAt: aString ifAbsent: [
		Error signal: 'Could not find option ', aString ]