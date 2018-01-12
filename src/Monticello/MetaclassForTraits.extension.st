Extension { #name : #MetaclassForTraits }

{ #category : #'*Monticello' }
MetaclassForTraits >> asMCDefinition [

	^MCClassTraitDefinition
		baseTraitName: self baseTrait name
		classTraitComposition: self traitCompositionString
		category: self category
			
]
