Extension { #name : #TaCompositionElement }

{ #category : #'*Ring-TraitsV2Support' }
TaCompositionElement >> asRingMinimalDefinitionIn: anRGEnvironment [
	| def |
	^ anRGEnvironment backend
		definitionFor: self
		ifAbsentRegister: [ | definingClass |
			definingClass := Smalltalk environment allBehaviors detect: [ :b | b traitComposition == self or: [b traitComposition includesElement: self]].

			def := RGTraitComposition named: self name parent: (definingClass asRingMinimalDefinitionIn: anRGEnvironment).

			def propertyNamed: #realObject put: self.

			"we need to set real object because we cannot simply identify the real object from the model data"
			def ]
]
