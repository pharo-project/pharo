testing
longOptionAt: aString
	^ self longOptionAt: aString ifAbsent: [
		Error signal: 'Could not find long-form option: ', aString  ]