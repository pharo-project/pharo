instance creation
generateBytes: aPlaceHolder forVersion: aVersion
	aVersion = 4 ifTrue: [self generateFieldsVersion4]
		ifFalse: [self error: 'Unsupported version'].
	self placeFields: aPlaceHolder.