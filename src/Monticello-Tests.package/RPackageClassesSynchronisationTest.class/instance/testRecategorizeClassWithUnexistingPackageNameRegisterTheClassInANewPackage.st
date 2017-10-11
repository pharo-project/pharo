tests - recategorizing class
testRecategorizeClassWithUnexistingPackageNameRegisterTheClassInANewPackage
	"test that when we recategorize a class in category not yet registered in RPackage, a new rPackage is created with the class inside"
	
	|XPackage YPackage class|
	self addXCategory.
	XPackage := self organizer packageNamed: #XXXXX.
	
	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.
	self assert: (self organizer packageOf: class) = XPackage. 
	
	class category: 'YYYYY'.
	YPackage := self organizer packageNamed: #YYYYY.
	
	self assert: (self organizer packageOf: class) equals: YPackage. 
	self deny: (self organizer packageOf: class) = XPackage.