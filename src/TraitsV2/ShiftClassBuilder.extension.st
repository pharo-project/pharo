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

{ #category : #'*TraitsV2' }
ShiftClassBuilder >> traitComposition [
	^ extensibleProperties at: #traitComposition ifAbsent: {}
]

{ #category : #'*TraitsV2' }
ShiftClassBuilder >> traitComposition: aValue [

	| aTraitComposition |
	aTraitComposition := aValue asTraitComposition.

	self classTraitComposition: aTraitComposition classComposition.
	^ extensibleProperties at: #traitComposition put: aTraitComposition
]
