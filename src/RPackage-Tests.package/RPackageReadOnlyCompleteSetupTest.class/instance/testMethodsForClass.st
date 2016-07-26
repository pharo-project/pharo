test situation
testMethodsForClass
	"self debug: #testMethodsForClass"
	
	self assert: (p1 methodsForClass: a1) size = 2. 
	self assert: ((p1 methodsForClass: a1) includes: ((Smalltalk at: #A1DefinedInP1)>>#methodDefinedInP1)).
	self assert: ((p1 methodsForClass: a1) includes: ((Smalltalk at: #A1DefinedInP1)>>#anotherMethodDefinedInP1)).
	self assert: (p1 methodsForClass: b1) isEmpty. 
	self assert: (p1 methodsForClass: Object) isEmpty. 
	self assert: (p1 methodsForClass: Object class) isEmpty. 

	self assert: (p3 methodsForClass: a2) size = 1.
	self assert: ((p3 methodsForClass: a2) includes: ((Smalltalk at: #A2DefinedInP2)>>#methodDefinedInP3)).
	self assert: (p3 methodsForClass: a2 class) size = 1.
	self assert:  ((p3 methodsForClass: a2 class) includes: ((Smalltalk at: #A2DefinedInP2) class>>#classSideMethodDefinedInP3))	