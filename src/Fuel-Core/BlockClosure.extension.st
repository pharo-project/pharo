Extension { #name : 'BlockClosure' }

{ #category : '*Fuel-Core' }
BlockClosure >> cleanCopy [
	"Answer a copy of myself preserving in outerContext just the receiver and method, which are the only needed by a clean block closure."

	^ self shallowCopy
		cleanOuterContext;
		yourself
]

{ #category : '*Fuel-Core' }
BlockClosure >> cleanOuterContext [
	"Clean my outerContext preserving just the receiver and method, which are the only needed by a clean block closure. outerContext can already be nil if the block was compiled without context"

	outerContext ifNotNil: [ outerContext := outerContext cleanCopy ]
]

{ #category : '*Fuel-Core' }
BlockClosure >> fuelAccept: aGeneralMapper [

	^ self shouldBeSubstitutedByMinimalCopy
		"The 'onRecursionDo:' is just to avoid an infinitive loop for the substitution"
		ifTrue: [
			aGeneralMapper
				visitSubstitution: self
				by: self cleanCopy
				onRecursionDo: [ aGeneralMapper visitVariableObject: self ]  ]
		ifFalse: [ aGeneralMapper visitVariableObject: self ]
]

{ #category : '*Fuel-Core' }
BlockClosure >> shouldBeSubstitutedByMinimalCopy [
	"Clean blocks can be made smaller by replacing the outer context.
	Un unclean closure can't be 'cleaned' so it will just be serialized
	as is. This is an optimization."
	^ self isClean
]
