class-loading
executeDefinitionFor: aRFiDefinition

	| definition |
	definition := aRFiDefinition isBehavior ifTrue: [ 
		self classDefinitionFor: aRFiDefinition.
	] ifFalse: [
		aRFiDefinition definition.
	].

	^ builder bootstrapInterpreter evaluateCode: definition.