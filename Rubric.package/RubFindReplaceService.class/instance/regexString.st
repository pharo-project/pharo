accessing
regexString
	^ self isRegex 
		ifTrue: [self findString]
		ifFalse: [self convertedFindString]
