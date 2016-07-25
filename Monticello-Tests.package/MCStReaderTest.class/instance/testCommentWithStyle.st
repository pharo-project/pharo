tests
testCommentWithStyle
	| reader |
	reader := MCStReader on: self commentWithStyle readStream.
	reader definitions