shout
initialize
	super initialize.
	textModel := RubScrolledTextModel new
		interactionModel: self;
		yourself.
	mustDeclareVariables := false