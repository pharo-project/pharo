tests-navigation
testCutMultiline
	
	self newText: 'ABC',String cr,'DEF'.
	selection all.
	selection end moveLeft: 2.
	selection start moveRight: 2.
	
	self assert: selection start lineNumber equals: 1.
	selection cut.

	self assert: text asString equals: 'ABEF'