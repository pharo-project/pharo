tests
testAccessingPackage
	"self debug: #testAccessingPackage"

	| p1 |
	p1 := self createNewPackageNamed: 'P1'.
	self packageOrganizer registerPackage: p1.
	p1 addClassDefinition: Point.
	p1 addMethod: Point>>#x.
	p1 addMethod: Point class>>#x:y:. 
	self assert: (self packageOrganizer packageNamed: #P1) = p1.
	self should: [(self packageOrganizer packageNamed: #P22)] raise: Error