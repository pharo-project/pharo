tests
testCommentWithoutStyle
	| reader |
	reader := MCStReader on: self commentWithoutStyle readStream.
	self assert: (reader definitions anySatisfy: [:ea | ea isMethodDefinition]).