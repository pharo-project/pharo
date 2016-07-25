test compiled method
testCompiledMethodIsExtensionInPackage
	"self debug: #testCompiledMethodIsExtensionInPackage"
	
	self deny: (((Smalltalk at: #A1DefinedInP1) >> #methodDefinedInP1) isExtensionInPackage: p1).
	self assert: (((Smalltalk at: #A2DefinedInP2) >> #methodDefinedInP1) isExtensionInPackage: p1).
	