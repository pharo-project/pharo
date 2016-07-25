test method addition removal
testAddRemoveMethod
	| p1 p2 p3 a2   a2Name |
	a2Name := #A2InPackageP2.
	p1 := self createNewPackageNamed: 'P1'.
	p2 := self createNewPackageNamed: 'P2'.
	p3 := self createNewPackageNamed: 'P3'.
	a2 := self createNewClassNamed: a2Name inPackage: p2.
	a2 compileSilently: 'methodDefinedInP2 ^ #methodDefinedInP2'.
	
	p2 addMethod: (a2>>#methodDefinedInP2).
	a2 compileSilently: 'methodDefinedInP1 ^ #methodDefinedInP1'.
	p1 addMethod: (a2>>#methodDefinedInP1).
	a2 compileSilently: 'methodDefinedInP3 ^ #methodDefinedInP3'.
	p3 addMethod: (a2>>#methodDefinedInP3).
	
	self assert: (p2 includesDefinedSelector: #methodDefinedInP2 ofClassName: a2Name).
	self deny: (p2 includesExtensionSelector: #methodDefinedInP2 ofClassName: a2Name).
	self deny: (p3 includesDefinedSelector: #methodDefinedInP3 ofClassName: a2Name).
	self assert: (p3 includesExtensionSelector: #methodDefinedInP3 ofClassName: a2Name).
	self assert: (p1 includesExtensionSelector: #methodDefinedInP1 ofClassName: a2Name).
	self deny: (p3 includesDefinedSelector: #methodDefinedInP1 ofClassName: a2Name).
	
	"removeMethod by default removes from defined methods and not extension"
	p2 removeMethod: (a2>>#methodDefinedInP2).
	self deny: (p2 includesDefinedSelector: #methodDefinedInP2 ofClassName: a2Name).
	self deny: (p2 includesExtensionSelector: #methodDefinedInP2 ofClassName: a2Name).
	
	p1 removeMethod: (a2>>#methodDefinedInP1).
	self deny: (p1 includesDefinedSelector: #methodDefinedInP3 ofClassName: a2Name).
	self deny: (p1 includesExtensionSelector: #methodDefinedInP3 ofClassName: a2Name).