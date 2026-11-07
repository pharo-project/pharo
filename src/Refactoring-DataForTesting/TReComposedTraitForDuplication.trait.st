Trait {
	#name : 'TReComposedTraitForDuplication',
	#traits : 'TReTraitToBeUsedInDuplication',
	#classTraits : 'TReTraitToBeUsedInDuplication classTrait',
	#category : 'Refactoring-DataForTesting-ForDuplication',
	#package : 'Refactoring-DataForTesting',
	#tag : 'ForDuplication'
}

{ #category : 'testing' }
TReComposedTraitForDuplication classSide >> classSideValue [

	^ 42
]

{ #category : 'testing' }
TReComposedTraitForDuplication >> instanceSideValue [

	^ 7
]
