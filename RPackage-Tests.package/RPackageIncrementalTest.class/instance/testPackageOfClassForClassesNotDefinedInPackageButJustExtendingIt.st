test package belonging
testPackageOfClassForClassesNotDefinedInPackageButJustExtendingIt
	| p1 p2 a2  |
	p1 := self createNewPackageNamed: 'P1'.
	p2 := self createNewPackageNamed: 'P2'.

	a2 := self createNewClassNamed: #A2InPackageP2 inPackage: p2.
	a2 compileSilently: 'methodDefinedInP1 ^ #methodDefinedInP1'.
	p1 addMethod: (a2>>#methodDefinedInP1).
	
	self assert: a2 package = p2.
	p1 extensionMethods do: 
		[:each | self deny: each methodClass package = p1 ].
	"the package of a class which is extended inside a package p, is not p
	but the package where the class was defined"