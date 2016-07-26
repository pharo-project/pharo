tests-selection
testCopyText
	| newText |	
	self newText: 'ABCDEF'.

	selection all.	
	selection start moveRight.
	selection end moveLeft.
	
	newText := selection copyText.
	
	self assert: newText asString equals: 'BCDE'.

