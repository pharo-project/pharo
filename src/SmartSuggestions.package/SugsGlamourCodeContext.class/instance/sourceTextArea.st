refactoring
sourceTextArea 
	^ model dependents ifEmpty: [ RubScrolledTextModel new interactionModel: model  ] ifNotEmpty: [ model dependents at: 1  ]