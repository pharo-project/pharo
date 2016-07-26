test compiled method
testCompiledMethodIsDefinedInPackage
	"self debug: #testCompiledMethodIsDefinedInPackage"
	
	self assert: (((Smalltalk at: #A1DefinedInP1) >> #methodDefinedInP1) isDefinedInPackage: p1).
	self deny: (((Smalltalk at: #A2DefinedInP2) >> #methodDefinedInP1) isDefinedInPackage: p1).
	