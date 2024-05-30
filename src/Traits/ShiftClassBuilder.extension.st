Extension { #name : 'ShiftClassBuilder' }

{ #category : '*Traits' }
ShiftClassBuilder >> beTrait [

	self
		superclass: nil;
		metaSuperclass: Trait;
		metaclassClass: MetaclassForTraits
]

{ #category : '*Traits' }
ShiftClassBuilder >> classTraitComposition [

	^ extensibleProperties at: #classTraitComposition ifAbsent: [ {  } asTraitComposition ]
]

{ #category : '*Traits' }
ShiftClassBuilder >> classTraitComposition: aValue [
	"The enhander needs to be put to nil in case the trait composition is updated."

	builderEnhancer := nil.
	^ extensibleProperties at: #classTraitComposition put: aValue asTraitComposition
]

{ #category : '*Traits' }
ShiftClassBuilder >> classTraits: aValue [
	"Added during the merge of FluidClassBuilder and ShiftClassBuilder"

	^ self classTraitComposition: aValue
]

{ #category : '*Traits' }
ShiftClassBuilder >> isTrait [
	^ self metaSuperclass isKindOf: Trait class
]

{ #category : '*Traits' }
ShiftClassBuilder >> privateTraitComposition: aValue [
	"The enhander needs to be put to nil in case the trait composition is updated."

	builderEnhancer := nil.
	^ extensibleProperties at: #traitComposition put: aValue asTraitComposition
]

{ #category : '*Traits' }
ShiftClassBuilder >> traitComposition [

	^ extensibleProperties at: #traitComposition ifAbsent: [ {  } asTraitComposition ]
]

{ #category : '*Traits' }
ShiftClassBuilder >> traitComposition: aValue [

	self classTraitComposition: aValue asTraitComposition classComposition.
	"Cyril: The next line is for the case where we update a class that had a trait and we remove the used traits. The metaclass should be updated also.
	But, IMO we could do better. Instead of having a state for the metaclassClass we should resolve it via the enhancer during the building. And someone could set its own using its own enhancer."
	(aValue isEmpty and: [ metaclassClass = TraitedMetaclass ]) ifTrue: [ metaclassClass := Metaclass ].
	^ self privateTraitComposition: aValue
]

{ #category : '*Traits' }
ShiftClassBuilder >> traits: aValue [
	"Added during the merge of FluidClassBuilder and ShiftClassBuilder"

	^ self traitComposition: aValue
]
