Extension { #name : #TraitComposition }

{ #category : #'*Ring2-Core' }
TraitComposition >> asRing2MinimalDefinitionIn: aRG2Environment [

	
	^ aRG2Environment backend definitionFor: self ifAbsentRegister: [
		| definingClass |
	
		definingClass := Smalltalk environment allBehaviors detect: [ :b | b traitComposition == self ].

		RG2TraitCompositionDefinition
			parent: (definingClass asRing2MinimalDefinitionIn: aRG2Environment)]
]
