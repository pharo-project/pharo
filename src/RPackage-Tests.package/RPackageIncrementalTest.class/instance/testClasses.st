test extension
testClasses	
	| p1 p2 a2  b2 |
	p1 := self createNewPackageNamed: 'P1'.
	p2 := self createNewPackageNamed: 'P2'.

	a2 := self createNewClassNamed: #A2InPackageP2 inPackage: p2.
	b2 := self createNewClassNamed: #B2InPackageP2 inPackage: p2.
	a2 compileSilently: 'methodPackagedInP1 ^ #methodPackagedInP1'.
	p1 addMethod: (a2>>#methodPackagedInP1).
	b2 class compileSilently: 'methodPackagedInP1 ^ #methodPackagedInP1'.
	p1 addMethod: (b2 class>>#methodPackagedInP1).
	
	self assert: p1 classes size = 2.
	self assert: p2 classes size = 2.