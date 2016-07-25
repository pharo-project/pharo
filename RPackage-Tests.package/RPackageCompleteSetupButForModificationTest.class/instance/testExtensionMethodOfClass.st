test starting situation
testExtensionMethodOfClass
	"self debug: #testExtensionMethodOfClass"
	
	"P1 
		A1DefinedInP1>>methodDefinedInP1
		B1DefinedInP1	
		A2DefinedInP2>>methodDefinedInP1  (was *P2)
	
	P2
		A2DefinedInP2>>methodDefinedInP2
		B2DefinedInB2	

	P3
		A3DefinedInP3	
		A2DefinedInP2>>methodDefinedInP3  (was *P2*)"
		
	self assert: a2 selectors size = 3.
	"the class contains methods defined by several packages 
	however when looking with a package view we only see the package 
	extension."
		
	self assert: (p2 extensionMethodsForClass: a2) size = 0.
	self assert: (p1 extensionMethodsForClass: a1) size = 0.
	"show that extensionMethodsForClass: does works for methods of class
	defined in the package"
	
	self assert: (p1 extensionMethodsForClass: a2) size = 1.
	self assert: (p3 extensionMethodsForClass: a2) size = 1.