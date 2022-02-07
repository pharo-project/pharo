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
