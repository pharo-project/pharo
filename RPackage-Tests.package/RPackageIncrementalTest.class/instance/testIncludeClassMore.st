test extension
testIncludeClassMore
	| p1 p2 p3 a2  |
	p1 := self createNewPackageNamed: 'P1'.
	p2 := self createNewPackageNamed: 'P2'.
	p3 := self createNewPackageNamed: 'P3'.
	a2 := self createNewClassNamed: #A2InPackageP2 inPackage: p2.
	a2 compileSilently: 'methodDefinedInP2 ^ #methodDefinedInP2'.
	p2 addMethod: (a2>>#methodDefinedInP2).
	a2 compileSilently: 'methodDefinedInP1 ^ #methodDefinedInP1'.
	p1 addMethod: (a2>>#methodDefinedInP1).
	a2 compileSilently: 'methodDefinedInP3 ^ #methodDefinedInP3'.
	p3 addMethod: (a2>>#methodDefinedInP3).
	
	self assert: (p2 includesClass: a2).
	self deny: (p1 includesClass: a2).
	self deny: (p3 includesClass: a2).