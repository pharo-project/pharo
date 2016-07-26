tests-selection
testSelectionAll
	
	self newText: 'ABC', String cr, 'DEF'.
	
	selection all.
	
	self assert: selection start isAtStart.
	self assert: selection end isAtEnd.