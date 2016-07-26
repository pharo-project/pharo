tests
testTranslateBy
	| rect |
	rect:= (2@2 corner: 6@4).
	
	self assert: (rect translateBy:2) left = 4.
	self assert: (rect translateBy:2) right = 8.
	self assert: (rect translateBy:2) top = 4.
	self assert: (rect translateBy:2) bottom = 6.
	
	self assert: (rect translateBy:(2 negated)) left = 0.
	self assert: (rect translateBy:(2 negated)) right = 4.
	self assert: (rect translateBy:(2 negated)) top = 0.
	self assert: (rect translateBy:(2 negated)) bottom = 2.