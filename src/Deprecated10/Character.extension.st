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

	self deprecated:
		'Assume all characters return 0: Characters in Pharo are internally unicode code points and the concept of leading char is obsolete and wrong'.
	^ (self asInteger bitAnd: 1069547520) bitShift: -22
]

{ #category : #'*Deprecated10' }
Character >> unicodeToMacRoman [
	self deprecated: 'Not supported anymore, for workaround see FreeTypeGlyphRenderer >> unicodeToMacRoman:'
]
