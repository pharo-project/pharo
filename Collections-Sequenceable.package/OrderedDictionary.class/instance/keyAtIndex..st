accessing
keyAtIndex: anIndex
	^ self keyAtIndex: anIndex ifAbsent: [self errorInvalidIndex: anIndex]