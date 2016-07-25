test extension
testIncludeClass
	| p1 p2 a2  |
	p1 := self createNewPackageNamed: 'P1'.
	p2 := self createNewPackageNamed: 'P2'.
	a2 := self createNewClassNamed: #A2InPackageP2 inPackage: p2.
	a2 compileSilently: 'methodPackagedInP1 ^ #methodPackagedInP1'.
	
	self deny: (p1 includesClass: a2).
	p1 addMethod: (a2>>#methodPackagedInP1).
	self deny: (p1 includesClass: a2).
	"We should declare the class explictly. Adding a method does not declare
	the class as defined. The reason is that like that the client controls the granularity 
	and moment of class registration."
	
	p1 addClassDefinition: a2.
	self assert: (p1 includesClass: a2).
	self assert: (p1 includesClassNamed: a2 name).
	
	