interface
includesVersionNamed: aString
	^ dict anySatisfy: [:ea | ea info name = aString]