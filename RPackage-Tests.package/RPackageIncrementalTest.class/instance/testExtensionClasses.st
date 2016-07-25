test extension
testExtensionClasses
	| p1 p2 a2  b2 |
	p1 := self createNewPackageNamed: 'P1'.
	p2 := self createNewPackageNamed: 'P2'.

	a2 := self createNewClassNamed: #A2InPackageP2 inPackage: p2.
	b2 := self createNewClassNamed: #B2InPackageP2 inPackage: p2.
	self deny: (p1 includesClass: a2).
	self assert: (p2 includesClass: a2).
	
	a2 compileSilently: 'methodPackagedInP1 ^ #methodPackagedInP1'.
	p1 addMethod: (a2>>#methodPackagedInP1).
	
	self assert: p1 extendedClasses size = 1.
	self assert: (p1 extendedClasses  includes: a2).
	self assert: p1 extendedClassNames size = 1.
	self assert: (p1 extendedClassNames  includes: a2 name).
	
	b2 class compileSilently: 'methodPackagedInP1 ^ #methodPackagedInP1'.
	p1 addMethod: (b2 class>>#methodPackagedInP1).
	
	self assert: p1 extendedClasses size = 2.
	self assert: (p1 extendedClasses  includes: b2 class).
	"extensionClasses returns or metaclasses while extensionClassNames returns class names (but not metaclass names)"

	self assert: p1 extendedClassNames size = 2.
	self assert: (p1 extendedClassNames  includes: b2 name).