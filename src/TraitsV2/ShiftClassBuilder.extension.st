Extension { #name : #ShiftClassBuilder }

{ #category : #'*TraitsV2' }
ShiftClassBuilder >> isTrait [ 
	^ self metaSuperclass isKindOf: Trait class 
]
