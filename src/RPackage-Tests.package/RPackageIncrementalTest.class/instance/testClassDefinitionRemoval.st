test class addition removal
testClassDefinitionRemoval
	| p a1 b1|
	
	p := self createNewPackageNamed: self p1Name.
	a1 := self createNewClassNamed: #A1InPAckageP1 inCategory: self p1Name.
	b1 := self createNewClassNamed: #B1InPAckageP1 inCategory: self p1Name.
	self assert: p definedClasses size = 0.
	
	p addClassDefinition: a1.
	p addClassDefinition: b1.
	self assert: p definedClasses size = 2.
	
	self assert: (p includesClass: a1).
	self assert: (p includesClass: b1).
	
	p removeClassDefinition: a1.
	self assert: p definedClasses size = 1.
	self deny: (p includesClass: a1).
	self assert: (p includesClass: b1).
	
	p removeClassDefinition: b1 class.
	self deny: (p includesClass: b1).
