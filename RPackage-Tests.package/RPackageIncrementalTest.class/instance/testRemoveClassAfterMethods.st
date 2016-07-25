test class addition removal
testRemoveClassAfterMethods
	| p1 a1 b1 |
	p1 := self createNewPackageNamed: self p1Name.
	"the class is created but not added to the package for now"
	a1 := self createNewClassNamed: #A1InPackageP1 inCategory: self p1Name.
	p1 addClassDefinition: a1.
	self assert: p1 definedClasses size = 1.
	a1 compileSilently: 'newlyAddedToA1 ^ #methodDefinedInP1'.
	p1 addMethod: (a1>>#newlyAddedToA1).
	
	"now we add a new method to the class and the to the package: it is considered
	as defined"
	self deny: (p1 includesExtensionSelector: #newlyAddedToA1 ofClass: a1).
	self assert: (p1 includesDefinedSelector: #newlyAddedToA1 ofClass: a1).
	
	p1 removeClassDefinition: a1.
	self assert: p1 definedClasses size = 0.
	
	self assert: (p1 includesSelector: #newlyAddedToA1 ofClass: a1).
	self assert: (p1 includesExtensionSelector: #newlyAddedToA1 ofClass: a1).
	self deny: (p1 includesDefinedSelector: #newlyAddedToA1 ofClass: a1).
	
	"Now we add the class and this means that the method is not defined anymore but an extension "
	