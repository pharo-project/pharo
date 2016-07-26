testing
matches: aString
	^ self isEmpty not
		and: [ self search = aString ]