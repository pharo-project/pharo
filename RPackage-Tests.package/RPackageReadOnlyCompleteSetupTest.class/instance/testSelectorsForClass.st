test situation
testSelectorsForClass
	"self debug: #testSelectorsForClass"
	
	self assert: (p1 selectorsForClass: a1) size = 2. 
	self assert: ((p1 selectorsForClass: a1) includes: #methodDefinedInP1).
	self assert: ((p1 selectorsForClass: a1) includes: #anotherMethodDefinedInP1).
	self assert: (p1 selectorsForClass: b1) isEmpty. 
	self assert: (p1 selectorsForClass: Object) isEmpty. 
	self assert: (p1 selectorsForClass: Object class) isEmpty. 

	self assert: (p3 selectorsForClass: a2) size = 1.
	self assert: ((p3 selectorsForClass: a2) includes: #methodDefinedInP3).
	self assert: (p3 selectorsForClass: a2 class) size = 1.
	self assert:  ((p3 selectorsForClass: a2 class) includes: #classSideMethodDefinedInP3)	