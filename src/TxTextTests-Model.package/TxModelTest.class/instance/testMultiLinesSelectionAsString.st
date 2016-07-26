tests-selection
testMultiLinesSelectionAsString
	
	self newText: 'ABC', String cr, 'DEF'.
	
	selection all.
	selection start moveRight.
	selection end moveLeft.
	
	self assert: selection asString equals: 'BC
DE'.

