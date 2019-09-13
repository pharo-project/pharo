Extension { #name : #MCClassTraitDefinition }

{ #category : #'*Ring-Definitions-Monticello' }
MCClassTraitDefinition >> asRingDefinition [

	^(RGMetatraitDefinition named:  baseTrait)
		traitCompositionSource: self classTraitCompositionString;
		definitionSource: self definitionString;
		yourself
]

{ #category : #'*Ring-Definitions-Monticello' }
MCClassTraitDefinition >> classDefinitionString [

	^self definitionString
]
