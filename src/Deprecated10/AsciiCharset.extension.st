Extension { #name : #AsciiCharset }

{ #category : #'*Deprecated10' }
AsciiCharset class >> canBeGlobalVarInitial: aCharacter [ 
	self deprecated: 'Please use #isValidGlobalName on the name of the variable'.
	^ self isUppercase: aCharacter
]
