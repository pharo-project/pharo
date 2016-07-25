test class addition removal
testDefinedClassesAndDefinedClassNames
	| p a1 b1 |
	p := self createNewPackageNamed: self p1Name.
	a1 := self createNewClassNamed: #A1InPackageP1 inCategory: self p1Name.
	self assert: p definedClasses size = 0.
	p addClassDefinition: a1.
	self assert: p definedClasses size = 1.
	self assert: (p definedClasses  includes: a1).
	self assert: (p definedClassNames includes: a1 name).
	
	b1 := self createNewClassNamed: #B1InPackageP1 inCategory: self p1Name.
	p addClassDefinition: b1.
	self assert: p definedClasses size = 2.
	self assert: (p definedClasses includes: b1).
	self assert: (p definedClassNames includes: b1 name).
	