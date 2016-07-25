tests-selection
testSingleLineSelectionAsString
	
	self newText: 'ABCDEF'.

	selection all.	
	selection start moveRight.
	selection end moveLeft.
	
	self assert: selection asString equals: 'BCDE'.

