tests-navigation
testReplaceAll
	
	self newText: 'ABC'.

	selection all.	
	selection replaceWith: 'DEF'.

	self assert: text asString equals: 'DEF'