tests-navigation
testMinMax
	| pos1 pos2  | 

	self newText: 'ABC',String cr,'DEF'.

	pos1 := text startPosition moveRight: 2.
	pos2 := text endPosition moveLeft: 2.
	
	
	self assert:  (pos1 min: pos2 ) equals: pos1.
	self assert:  (pos2 min: pos1 ) equals: pos1.

	self assert: (pos1 max: pos2) equals: pos2.
	self assert: (pos2 max: pos1) equals: pos2.