Extension { #name : 'LegacyClassDefinitionPrinter' }

{ #category : '*Traits' }
LegacyClassDefinitionPrinter >> traitDefinitionString [

	^ String streamContents: [ :s |
		s
			nextPutAll: 'Trait named: ';
			nextPutAll:	forClass name printString;
			cr; tab;
			nextPutAll: ' uses: ';
			nextPutAll: forClass traitComposition traitCompositionExpression;
			cr;
			tab;
			nextPutAll: ' package: ';
			print: forClass category asString
			"we do not want the # if category returns a symbol"
	]
]

{ #category : '*Traits' }
LegacyClassDefinitionPrinter >> traitedMetaclassDefinitionString [

	^ self metaclassDefinitionString
]
