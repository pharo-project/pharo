test method addition removal
testAddRemoveSelectorOfMetaclass
	| p1 p2 p3 a2   a2Name a2class |
	a2Name := #A2InPackageP2.
	p1 := self createNewPackageNamed: 'P1'.
	p2 := self createNewPackageNamed: 'P2'.
	p3 := self createNewPackageNamed: 'P3'.
	a2 := self createNewClassNamed: a2Name inPackage: p2.
	a2class := a2 class.
	a2class compileSilently: 'methodDefinedInP2 ^ #methodDefinedInP2'.
	p2 addSelector: #methodDefinedInP2 ofMetaclassName: a2Name.
	a2class compileSilently: 'methodDefinedInP1 ^ #methodDefinedInP1'.
	p1 addSelector: #methodDefinedInP1 ofMetaclassName: a2Name.
	a2class compileSilently: 'methodDefinedInP3 ^ #methodDefinedInP3'.
	p3 addSelector: #methodDefinedInP3 ofMetaclassName: a2Name.
	
	self assert: (p2 includesDefinedSelector: #methodDefinedInP2 ofMetaclassName: a2Name).
	self deny: (p2 includesExtensionSelector: #methodDefinedInP2 ofMetaclassName: a2Name).
	self deny: (p3 includesDefinedSelector: #methodDefinedInP3 ofMetaclassName: a2Name).
	self assert: (p3 includesExtensionSelector: #methodDefinedInP3 ofMetaclassName: a2Name).
	self assert: (p1 includesExtensionSelector: #methodDefinedInP1 ofMetaclassName: a2Name).
	self deny: (p3 includesDefinedSelector: #methodDefinedInP1 ofMetaclassName: a2Name).
	
	p2 removeSelector: #methodDefinedInP2 ofMetaclassName:  a2Name.
	self deny: (p2 includesDefinedSelector: #methodDefinedInP2 ofMetaclassName: a2Name).
	self deny: (p2 includesExtensionSelector: #methodDefinedInP2 ofMetaclassName: a2Name).
	
	p1 removeSelector: #methodDefinedInP1 ofMetaclassName:  a2Name.
	self deny: (p1 includesDefinedSelector: #methodDefinedInP3 ofMetaclassName: a2Name).
	self deny: (p1 includesExtensionSelector: #methodDefinedInP3 ofMetaclassName: a2Name).