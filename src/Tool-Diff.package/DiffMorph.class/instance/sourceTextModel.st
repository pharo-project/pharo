instance-creation
sourceTextModel
	^ sourceTextModel ifNil: [ sourceTextModel := RubScrolledTextModel new interactionModel: self ].