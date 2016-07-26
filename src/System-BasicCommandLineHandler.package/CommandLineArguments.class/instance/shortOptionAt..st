testing
shortOptionAt: aString
	^ self shortOptionAt: aString ifAbsent: [
		Error signal: 'Could not find short-form option: ', aString  ]