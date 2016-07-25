test extension
testIncludeSelectorOfClass
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
	
	"includesSelector checks both in defined and extension so we test both"
	self assert: (p2 includesSelector: #methodDefinedInP2 ofClass: a2).
	self assert: (p2 includesDefinedSelector: #methodDefinedInP2 ofClassName: a2 name).
	self deny: (p2 includesExtensionSelector: #methodDefinedInP2 ofClassName: a2 name).
	
	self deny: (p2 includesSelector: #methodDefinedInP3 ofClass: a2).
	self assert: (p3 includesExtensionSelector: #methodDefinedInP3 ofClassName: a2 name).
	self deny: (p3 includesDefinedSelector: #methodDefinedInP3 ofClassName: a2 name).
	
	self deny: (p2 includesSelector: #methodDefinedInP1 ofClass: a2).
	self assert: (p1 includesExtensionSelector: #methodDefinedInP1 ofClassName: a2 name).
	self deny: (p3 includesDefinedSelector: #methodDefinedInP1 ofClassName: a2 name).