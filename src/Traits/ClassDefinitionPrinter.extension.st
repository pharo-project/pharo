Extension { #name : 'ClassDefinitionPrinter' }

{ #category : '*Traits' }
ClassDefinitionPrinter >> traitDefinitionString [
	^ self subclassResponsibility
]

{ #category : '*Traits' }
ClassDefinitionPrinter >> traitedMetaclassDefinitionString [
	^ self subclassResponsibility
]
