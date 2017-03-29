accessing
textModel: aTextModel
	textModel := aTextModel.
	
	self textEditBrick ifNotNil: [ :aTextEditBrick | aTextEditBrick text: self textModel ]