test situation
testDefinedSelectorsForClass
	"self debug: #testDefinedSelectorsForClass"
	
	self assert: (p1 definedSelectorsForClass: a1) size = 2. 
	self assert: (p1 definedMethodsForClass: a1) size = 2. 
	self assert: ((p1 definedSelectorsForClass: a1) includes: #methodDefinedInP1).
	self assert: ((p1 definedSelectorsForClass: a1) includes: #anotherMethodDefinedInP1).
	self assert: ((p1 definedMethodsForClass: a1) includes: ((Smalltalk at: #A1DefinedInP1)>>#methodDefinedInP1)).
	self assert: (p1 definedSelectorsForClass: Object) isEmpty. 
	self assert: (p1 definedSelectorsForClass: Object class) isEmpty. 
