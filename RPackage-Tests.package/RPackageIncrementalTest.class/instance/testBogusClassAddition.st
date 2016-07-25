test class addition removal
testBogusClassAddition
	| p a1 |
	p := self createNewPackageNamed: self p1Name.
	a1 := self createNewClassNamed: #A1InPAckageP1 inCategory: self p1Name.
	self assert: p definedClasses size = 0.
	p addClassDefinitionName: a1 name.
	self assert: p definedClasses size = 1.
	self assert: (p includesClass: a1).
	self should: [p addClassDefinitionName: a1 class name] raise: Error.
