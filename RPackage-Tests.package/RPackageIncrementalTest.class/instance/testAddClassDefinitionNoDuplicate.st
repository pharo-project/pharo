test class addition removal
testAddClassDefinitionNoDuplicate
	| p a1 b1 |
	p := self createNewPackageNamed: self p1Name.
	a1 := self createNewClassNamed: #A1InPackageP1 inCategory: self p1Name.
	self assert: p definedClasses size = 0.
	p addClassDefinition: a1.
	self assert: p definedClasses size = 1.
	b1 := self createNewClassNamed: #B1InPackageP1 inCategory: self p1Name.
	p addClassDefinition: a1.
	"adding the same class does not do anything - luckily"
	self assert: p definedClasses size = 1.
	p addClassDefinition: b1.
	self assert: p definedClasses size = 2.
