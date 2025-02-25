Extension { #name : 'TaAliasMethod' }

{ #category : '*Ring-TraitsSupport' }
TaAliasMethod >> asRingMinimalDefinitionIn: anRGEnvironment [
	^ anRGEnvironment backend
		definitionFor: self
		ifAbsentRegister: [ | definingClass def |
			definingClass := Smalltalk environment allBehaviors detect: [ :b | b traitComposition transformations includes: self ].

			def := definingClass traitComposition == self
				ifTrue: [RGTraitAlias parent: (definingClass asRingMinimalDefinitionIn: anRGEnvironment) ]
				ifFalse: [RGTraitAlias parent: (definingClass traitComposition asRingMinimalDefinitionIn: anRGEnvironment) ].


			"we need to set real object because we cannot simply identify the real object from the model data"
			def propertyNamed: #realObject put: self.
			def ]
]
