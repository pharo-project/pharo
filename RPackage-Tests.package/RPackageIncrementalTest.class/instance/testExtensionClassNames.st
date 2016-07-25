test extension
testExtensionClassNames
	| p1 p2  a2 b2 |
	p1 := self createNewPackageNamed: 'P1'.
	p2 := self createNewPackageNamed: 'P2'.
	a2 := self createNewClassNamed: #A2InPackageP2 inPackage: p2.
	b2 := self createNewClassNamed: #B2InPackageP2 inPackage: p2.
	self deny: (p1 includesClass: a2).
	self assert: (p2 includesClass: b2).
	self assert: (p2 includesClass: a2).
	
	a2 compileSilently: 'methodDefinedInP1 ^ #methodDefinedInP1'.
	p1 addSelector: #methodDefinedInP1 ofClassName: a2 name.
	
	self assert: p1 extensionSelectors size = 1.
	self assert: p1 extensionMethods size = 1.
	self assert: (p1 extendedClassNames includes: #A2InPackageP2).
	self deny: (p1 includesClass: a2).
	"method extension class are not included in packages"
	
	b2 compile: 'firstMethodInB2PackagedInP1 ^ 1'.
	p1 addSelector: #firstMethodInB2PackagedInP1 ofClassName: b2 name.
	
	self assert: p1 extensionSelectors  size = 2.
	self assert: p1 extensionMethods  size = 2.
	self assert: (p1 extendedClassNames includes: #B2InPackageP2).
	self deny: (p1 includesClass: b2).
	
	b2 compileSilently: 'secondMethodInB2PackagedInP1 ^ 2'.
	p1 addSelector: #secondMethodInB2PackagedInP1 ofClassName: b2 name.
	
	self assert: p1 extensionSelectors  size = 3.
	self assert: p1 extensionMethods  size = 3.
	self assert: p1 extendedClasses size = 2.
	self assert: (p1 extendedClassNames includes: #B2InPackageP2).
	self deny: (p1 includesClass: b2).