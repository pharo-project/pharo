test extension
testIncludesOrTouches
	| p1 p2 a2  |
	p1 := self createNewPackageNamed: 'P1'.
	p2 := self createNewPackageNamed: 'P2'.

	a2 := self createNewClassNamed: #A2InPackageP2 inPackage: p2.
	self deny: (p1 includesClass: a2).
	self assert: (p2 includesClass: a2).
	
	a2 compileSilently: 'methodPackagedInP1 ^ #methodPackagedInP1'.
	p1 addMethod: (a2>>#methodPackagedInP1).
	
	self assert: p1 extensionMethods size = 1.
	self assert: p1 extensionSelectors size = 1.
	self deny: (p1 includesClass: a2).
	self assert: (p1 definesOrExtendsClass: a2).
	self assert: (p2 includesClass: a2).