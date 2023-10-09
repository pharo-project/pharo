Extension { #name : 'ShiftClassBuilder' }

{ #category : '*TraitsV2' }
ShiftClassBuilder >> beTrait [

	self
		superclass: nil;
		metaSuperclass: Trait;
		metaclassClass: MetaclassForTraits
]

{ #category : '*TraitsV2' }
ShiftClassBuilder >> classTraitComposition [

	^ extensibleProperties at: #classTraitComposition ifAbsent: [ {  } asTraitComposition ]
]

{ #category : '*TraitsV2' }
ShiftClassBuilder >> classTraitComposition: aValue [
	"The enhander needs to be put to nil in case the trait composition is updated."

	builderEnhancer := nil.
	^ extensibleProperties at: #classTraitComposition put: aValue asTraitComposition
]

{ #category : '*TraitsV2' }
ShiftClassBuilder >> classTraits: aValue [
	"Added during the merge of FluidClassBuilder and ShiftClassBuilder"

	^ self classTraitComposition: aValue
]

{ #category : '*TraitsV2' }
ShiftClassBuilder >> isTrait [
	^ self metaSuperclass isKindOf: Trait class
]

{ #category : '*TraitsV2' }
ShiftClassBuilder >> privateTraitComposition: aValue [
	"The enhander needs to be put to nil in case the trait composition is updated."

	builderEnhancer := nil.
	^ extensibleProperties at: #traitComposition put: aValue asTraitComposition
]

{ #category : '*TraitsV2' }
ShiftClassBuilder >> traitComposition [

	^ extensibleProperties at: #traitComposition ifAbsent: [ {  } asTraitComposition ]
]

{ #category : '*TraitsV2' }
ShiftClassBuilder >> traitComposition: aValue [

	self classTraitComposition: aValue asTraitComposition classComposition.
	^ self privateTraitComposition: aValue
]

{ #category : '*TraitsV2' }
ShiftClassBuilder >> traits: aValue [
	"Added during the merge of FluidClassBuilder and ShiftClassBuilder"

	^ self traitComposition: aValue
]
