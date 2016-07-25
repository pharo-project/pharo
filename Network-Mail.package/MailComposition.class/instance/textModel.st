access
textModel
	^ textModel ifNil: [ textModel := RubScrolledTextModel new interactionModel: self ]