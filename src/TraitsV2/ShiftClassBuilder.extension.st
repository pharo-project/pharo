Extension { #name : #ShiftClassBuilder }

{ #category : #'*TraitsV2' }
ShiftClassBuilder >> beTrait [

	self 
		superclass: nil;
		metaSuperclass: Trait;
		metaclassClass: MetaclassForTraits

]

{ #category : #'*TraitsV2' }
ShiftClassBuilder >> isTrait [ 
	^ self metaSuperclass isKindOf: Trait class 
]
