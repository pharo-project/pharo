test method addition removal
testMethodAddition
	| p1 a1 methodRef |
	p1 := self createNewPackageNamed: 'P1'.
	a1 := self createNewClassNamed: #A1DefinedInP1 inPackage: p1.
	a1 compileSilently: 'foo ^ 10'.
	p1 addMethod:  (a1>>#foo).
	self assert: (p1 includesSelector: #foo ofClass: a1).
	