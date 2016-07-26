tests-navigation
testCutMultiline2

	self newText: 'ABC',String cr, 	'zzzz' , String cr , 'DEF'.
	selection all.
	selection end moveLeft: 2.
	selection start moveRight: 2.
	
	self assert: selection start lineNumber equals: 1.
	selection cut.

	self assert: text asString equals: 'ABEF'