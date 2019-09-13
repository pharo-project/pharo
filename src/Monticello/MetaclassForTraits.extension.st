Extension { #name : #MetaclassForTraits }

{ #category : #'*Monticello' }
MetaclassForTraits >> asMCDefinition [

	^MCClassTraitDefinition
		baseTraitName: self instanceSide name
		classTraitComposition: self traitCompositionString
		category: self category
			
]
