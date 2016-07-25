test package belonging
testPackageOfClassForDefinedClasses
	| p a1 b1 |
	p := self createNewPackageNamed: 'P1'.
	a1 := self createNewClassNamed: #A1InPAckageP1 inPackage: p.
	b1 := self createNewClassNamed: #B1InPAckageP1 inPackage: p.

	self assert: a1 package = p.
	self assert: b1 package = p.
