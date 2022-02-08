Extension { #name : #Character }

{ #category : #'*Deprecated10' }
Character >> canBeGlobalVarInitial [
	self deprecated: 'Please use #isValidGlobalName on the name of the variable'.
	^ self characterSet canBeGlobalVarInitial: self
]

{ #category : #'*Deprecated10' }
Character >> canBeNonGlobalVarInitial [
	self deprecated: 'Please use #isValidGlobalName on the name of the variable'.
	^ self characterSet canBeNonGlobalVarInitial: self
]

{ #category : #'*Deprecated10' }
Character >> leadingChar [
	^ (self asInteger bitAnd: 1069547520) bitShift: -22
]

{ #category : #'*Deprecated10' }
Character class >> leadingChar: leadChar code: code [

	code >= 16r400000 ifTrue: [
		self error: 'code is out of range'.
	].
	leadChar >= 256 ifTrue: [
		self error: 'lead is out of range'.
	].
	code < 256 ifTrue: [ ^self value: code ].
	^self value: (leadChar bitShift: 22) + code
]

{ #category : #'*Deprecated10' }
Character >> unicodeToMacRoman [
	"Convert the receiver from Unicode to MacRoman encoding."

	^MacRomanTextConverter new byteToUnicode: self
]
