test compiled method
testClassIsExtendedInPackage
	"self debug: #testClassIsExtendedInPackage"
	
	self deny: ((Smalltalk globals at: #A1DefinedInP1) isExtendedInPackage: p1).
	self assert: (p1 includesClass: (Smalltalk globals at: #A1DefinedInP1)).
	self deny: (p1 extendsClass: (Smalltalk globals at: #A1DefinedInP1)).
	self assert: ((Smalltalk globals at: #A2DefinedInP2) isExtendedInPackage: p1).
	self deny: (p1 includesClass: (Smalltalk globals at: #A2DefinedInP2)).
	self assert: (p1 extendsClass: (Smalltalk globals at: #A2DefinedInP2)).
	