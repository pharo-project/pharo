test addition
testAddMethod
	"self debug: #testAddMethod"
	
	self assert: (p1 includesSelector: #methodDefinedInP1 ofClass: a1).
	
	a1 compileSilently: 'zork ^ #methodDefinedInP1'.
	p1 addMethod: (a1>>#zork).
	self assert: (p1 includesSelector: #zork ofClass: a1).
	
	self assert: (p3 includesSelector: #classSideMethodDefinedInP3 ofClass: a2 class)
	