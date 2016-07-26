test extension
testIncludesMethodOfClassInPresenceOfOtherPackageExtensions
	| p1 p2 p3 a2  a2name |
	p1 := self createNewPackageNamed: 'P1'.
	p2 := self createNewPackageNamed: 'P2'.
	p3 := self createNewPackageNamed: 'P3'.
	a2name := #A2InPackageP2.
	a2 := self createNewClassNamed: a2name inPackage: p2.
	
	a2 compileSilently: 'methodDefinedInP2 ^ #methodDefinedInP2'.
	p2 addMethod: (a2>>#methodDefinedInP2).
	self assert: (p2 includesDefinedSelector: #methodDefinedInP2 ofClassName: a2name).
	
	a2 compileSilently: 'methodDefinedInP1 ^ #methodDefinedInP1'.
	p1 addMethod: (a2>>#methodDefinedInP1).
	self assert: (p1 includesExtensionSelector: #methodDefinedInP1 ofClassName: a2name).
	
	a2 compileSilently: 'methodDefinedInP3 ^ #methodDefinedInP3'.
	p3 addMethod: (a2>>#methodDefinedInP3).
	self assert: (p3 includesExtensionSelector: #methodDefinedInP3 ofClassName: a2name).
		
	self assert: (p2 includesDefinedSelector: #methodDefinedInP2 ofClassName: a2name).
	self deny: (p2 includesDefinedSelector: #methodDefinedInP3 ofClassName: a2name).
	self deny: (p2 includesExtensionSelector: #methodDefinedInP3 ofClassName: a2name).
	self deny: (p2 includesDefinedSelector: #methodDefinedInP1 ofClassName: a2name).
	self deny: (p2 includesExtensionSelector: #methodDefinedInP1 ofClassName: a2name).
	
	
	