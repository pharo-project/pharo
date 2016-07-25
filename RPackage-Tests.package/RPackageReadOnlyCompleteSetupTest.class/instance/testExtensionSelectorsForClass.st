test situation
testExtensionSelectorsForClass
	"self debug: #testExtensionSelectorsForClass"
	
	self assert: (p1 extensionSelectorsForClass: a2) size = 1. 
	self assert: ((p1 extensionSelectorsForClass: a2) includes: #methodDefinedInP1).
	self assert: ((p1 extensionMethodsForClass: a2) includes: (Smalltalk at: #A2DefinedInP2) >> #methodDefinedInP1).
	self assert: (p1 extensionSelectorsForClass: Object) isEmpty.
	self assert: (p1 extensionSelectorsForClass: Object class) isEmpty