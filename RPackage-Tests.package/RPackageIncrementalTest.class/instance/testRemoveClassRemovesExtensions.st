test class addition removal
testRemoveClassRemovesExtensions
	| p1 p2 a1 b1 |
	p1 := self createNewPackageNamed: self p1Name.
	p2 := self createNewPackageNamed: self p2Name.
	"the class is created but not added to the package for now"
	a1 := self createNewClassNamed: #A1InPackageP1 inCategory: self p1Name.
	p1 addClassDefinition: a1.
	self assert: p1 definedClasses size = 1.
	a1 compileSilently: 'newlyAddedToA1 ^ #methodDefinedInP1'.
	p2 addMethod: (a1>>#newlyAddedToA1).
	
	self assert: (p2 includesSelector: #newlyAddedToA1 ofClass: a1).
	self assert: (p2 includesExtensionSelector: #newlyAddedToA1 ofClass: a1).
	
	a1 removeFromSystem.
	
	self deny: (p2 includesSelector: #newlyAddedToA1 ofClass: a1).
	self deny: (p2 includesExtensionSelector: #newlyAddedToA1 ofClass: a1).
	self deny: (p2 includesDefinedSelector: #newlyAddedToA1 ofClass: a1).