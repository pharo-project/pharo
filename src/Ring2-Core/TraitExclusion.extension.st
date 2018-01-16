Extension { #name : #TraitExclusion }

{ #category : #'*Ring2-Core' }
TraitExclusion >> asRing2MinimalDefinitionIn: aRG2Environment [

	^ aRG2Environment backend definitionFor: self ifAbsentRegister: [
		| definingClass def |
	
		definingClass := Smalltalk environment allBehaviors detect: [ :b | b traitComposition transformations includes: self ].

		def := RG2TraitExclusionDefinition
			parent: (definingClass traitComposition asRing2MinimalDefinitionIn: aRG2Environment).
			
		"we need to set real object because we cannot simply identify the real object from the model data"
		def propertyNamed: #realObject put: self.
		def ]
]
