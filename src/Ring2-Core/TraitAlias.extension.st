Extension { #name : #TraitAlias }

{ #category : #'*Ring2-Core' }
TraitAlias >> asRing2MinimalDefinitionIn: aRG2Environment [

	^ aRG2Environment backend definitionFor: self ifAbsentRegister: [
		| definingClass def |
	
		definingClass := Smalltalk environment allBehaviors detect: [ :b | b traitComposition transformations includes: self ].

		def := RG2TraitAliasDefinition
			parent: (definingClass traitComposition asRing2MinimalDefinitionIn: aRG2Environment).
			
		"we need to set real object because we cannot simply identify the real object from the model data"
		def propertyNamed: #realObject put: self.
		def ]
]
