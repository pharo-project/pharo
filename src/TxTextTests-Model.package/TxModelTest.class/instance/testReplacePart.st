tests-navigation
testReplacePart

	self newText: 'ABCDEF'.
	selection all.
	selection end moveLeft: 3.

	selection replaceWith: 'DEF'.

	self assert: text asString equals: 'DEFDEF'