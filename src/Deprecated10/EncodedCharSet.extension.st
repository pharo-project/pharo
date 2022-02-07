Extension { #name : #EncodedCharSet }

{ #category : #'*Deprecated10' }
EncodedCharSet class >> canBeGlobalVarInitial: char [

	| leadingChar |
	self deprecated: 'Please use #isValidGlobalName on the name of the variable'.
	leadingChar := char leadingChar.

	leadingChar = 0 ifTrue: [^ self isUppercase: char].
	^ self isLetter: char.
]

{ #category : #'*Deprecated10' }
EncodedCharSet class >> canBeNonGlobalVarInitial: char [

	| leadingChar |
	self deprecated: 'Please use #isValidGlobalName on the name of the variable'.
	leadingChar := char leadingChar.

	leadingChar = 0 ifTrue: [^ self isLowercase: char].
	^ self isLetter: char.
]
