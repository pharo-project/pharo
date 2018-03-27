Extension { #name : #Array }

{ #category : #'*TraitsV2' }
Array >> asTraitComposition [
	^ self isEmpty
		ifTrue: [ TaEmptyComposition new ]
		ifFalse: [ TaSequence withAll: (self collect: #asTraitComposition) ]
]
