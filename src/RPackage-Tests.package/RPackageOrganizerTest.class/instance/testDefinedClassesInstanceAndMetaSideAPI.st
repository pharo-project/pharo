tests
testDefinedClassesInstanceAndMetaSideAPI
	"self debug: #testDefinedClassesInstanceAndMetaSideAPI"
	| p1 |
	p1 := self createNewPackageNamed: 'P1'.
	self packageOrganizer registerPackage: p1.
	p1 addClassDefinition: Point.
	p1 addMethod: Point>>#x.
	p1 addMethod: Point class>>#x:y:. 
	self assert: self packageOrganizer packageNames size = 2.
	self assert: self packageOrganizer packages size = 2.
	self assert: (self packageOrganizer packageNamed: #P1) definedClasses size = 1