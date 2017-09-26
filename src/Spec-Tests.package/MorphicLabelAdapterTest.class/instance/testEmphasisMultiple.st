tests
testEmphasisMultiple
	label emphasis: #(bold italic).
	label buildWithSpec.
	self assert: label widget widget font emphasis equals: 3