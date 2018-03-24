Extension { #name : #MCClassTraitDefinition }

{ #category : #'*Ring-Deprecated-Monticello' }
MCClassTraitDefinition >> asRingDefinition [

	^(RGMetatraitDefinition named:  baseTrait)
		traitCompositionSource: self classTraitCompositionString;
		definitionSource: self definitionString;
		yourself
]

{ #category : #'*Ring-Deprecated-Monticello' }
MCClassTraitDefinition >> classDefinitionString [

	^self definitionString
]
