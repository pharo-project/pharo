Extension { #name : #EncodedCharSet }

{ #category : #'*Deprecated10' }
EncodedCharSet class >> canBeGlobalVarInitial: char [

	self deprecated: 'Please use #isValidGlobalName on the name of the variable'.
	^ self isUppercase: char
]

{ #category : #'*Deprecated10' }
EncodedCharSet class >> canBeNonGlobalVarInitial: char [

	self deprecated: 'Please use #isValidGlobalName on the name of the variable'.
	^ self isLowercase: char
]
