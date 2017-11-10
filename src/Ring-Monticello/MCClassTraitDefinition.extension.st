Extension { #name : #MCClassTraitDefinition }

{ #category : #'*Ring-Monticello' }
MCClassTraitDefinition >> asRingDefinition [

	^(RGMetatraitDefinition named:  baseTrait)
		traitCompositionSource: self classTraitCompositionString;
		definitionSource: self definitionString;
		yourself
]

{ #category : #'*Ring-Monticello' }
MCClassTraitDefinition >> classDefinitionString [

	^self definitionString
]
