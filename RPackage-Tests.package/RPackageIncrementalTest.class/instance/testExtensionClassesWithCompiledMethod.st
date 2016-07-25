test extension
testExtensionClassesWithCompiledMethod
	| p1 p2  a2 b2 |
	p1 := self createNewPackageNamed: 'P1'.
	p2 := self createNewPackageNamed: 'P2'.
	a2 := self createNewClassNamed: #A2InPackageP2 inPackage: p2.
	b2 := self createNewClassNamed: #B2InPackageP2 inPackage: p2.
	self deny: (p1 includesClass: a2).
	self assert: (p2 includesClass: b2).
	self assert: (p2 includesClass: b2).
	
	a2 compileSilently: 'methodDefinedInP1 ^ #methodDefinedInP1'.
	p1 addMethod: (a2>>#methodDefinedInP1).
	
	self assert: p1 extensionMethods size = 1.
	self assert: p1 extensionSelectors size = 1.
	self assert: p1 extendedClasses size = 1.
	self deny: (p1 includesClass: a2).
	"method extension class are not included in packages"
	
	b2 compile: 'firstMethodInB2PackagedInP1 ^ 1'.
	p1 addMethod: (b2>>#firstMethodInB2PackagedInP1).
	
	self assert: p1 extensionSelectors  size = 2.
	self assert: p1 extensionMethods  size = 2.
	self assert: p1 extendedClasses size = 2.
	self deny: (p1 includesClass: b2).
	
	b2 compileSilently: 'secondMethodInB2PackagedInP1 ^ 2'.
	p1 addMethod: (b2>>#secondMethodInB2PackagedInP1).
	
	self assert: p1 extensionSelectors  size = 3.
	self assert: p1 extensionMethods  size = 3.
	self assert: p1 extendedClasses size = 2.
	self deny: (p1 includesClass: b2).