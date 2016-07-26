accessing
textModel
	^ textModel ifNil: [textModel := RubScrolledTextModel new text: self loremIpsum ]