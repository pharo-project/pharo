test class addition removal
testClassDefinitionRemovalName
	| p a1 b1|
	p := self createNewPackageNamed: self p1Name.
	a1 := self createNewClassNamed: #A1InPAckageP1 inCategory: self p1Name.
	b1 := self createNewClassNamed: #B1InPAckageP1 inCategory: self p1Name.
	self assert: p definedClasses size = 0.
	p addClassDefinitionName: a1 name.
	p addClassDefinitionName: b1 name.
	self assert: p definedClasses size = 2.
	self assert: (p includesClass: a1).
	self assert: (p includesClass: b1).
	p removeClassDefinitionName: a1 name.
	self assert: p definedClasses size = 1.
	self deny: (p includesClass: a1).
	self assert: (p includesClass: b1).
	self should: [p removeClassDefinitionName: b1 class name] raise: Error.
	p removeClassDefinitionName: b1  name.
	self deny: (p includesClass: b1).
