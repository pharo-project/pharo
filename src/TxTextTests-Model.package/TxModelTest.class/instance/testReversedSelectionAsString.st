tests-selection
testReversedSelectionAsString
	
	self newText: 'ABCDEF'.

	selection beSelection.
	selection start moveToStart.
	selection end moveToStart.
	selection start moveRight.
	
	self assert: selection asString equals: 'A'.

