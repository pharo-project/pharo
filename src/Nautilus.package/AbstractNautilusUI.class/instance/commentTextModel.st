comment pane
commentTextModel
	^ commentTextModel
		ifNil: [ 
			commentTextModel := RubScrolledTextModel new
				interactionModel: self;
				yourself ]