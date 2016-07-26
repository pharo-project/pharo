callbacks
actOnVariableBindingsUpdated: ann 

	textModel variableBindings: nil.
	self addVariableBindingsOn: textModel from: ann presentation.
	textModel changed: #text. self flag: 'update just the style here'