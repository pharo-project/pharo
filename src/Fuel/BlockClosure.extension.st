Extension { #name : #BlockClosure }

{ #category : #'*Fuel' }
BlockClosure >> cleanCopy [
	"Answer a copy of myself preserving in outerContext just the receiver and method, which are the only needed by a clean block closure."

	^ self shallowCopy
		cleanOuterContext;
		yourself
]

{ #category : #'*Fuel' }
BlockClosure >> cleanOuterContext [
	"Clean my outerContext preserving just the receiver and method, which are the only needed by a clean block closure."

	outerContext := outerContext cleanCopy
]

{ #category : #'*Fuel' }
BlockClosure >> fuelAccept: aGeneralMapper [

	^ self shouldBeSubstitutedByCleanCopy
		"The 'onRecursionDo:' is just to avoid an infinitive loop for the substitution. The cleanCopy MUST be a clean copy so it can be serialized normally"
		ifTrue: [ aGeneralMapper visitSubstitution: self by: self cleanCopy onRecursionDo: [ aGeneralMapper visitVariableObject: self ]  ]
		ifFalse: [ aGeneralMapper visitVariableObject: self ]
]

{ #category : #'*Fuel' }
BlockClosure >> shouldBeSubstitutedByCleanCopy [
	^ self isClean 
]
