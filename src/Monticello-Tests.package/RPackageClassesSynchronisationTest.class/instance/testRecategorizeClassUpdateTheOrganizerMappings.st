tests - recategorizing class
testRecategorizeClassUpdateTheOrganizerMappings
	"test that when we recategorize a class, the organizer is updated so that the class name point the the new RPackage"
	
	|XPackage YPackage class|
	self addXCategory.
	self addYCategory.
	XPackage := self organizer packageNamed: #XXXXX.
	YPackage := self organizer packageNamed: #YYYYY.
	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.
	class category: 'YYYYY'.	
	self assert: (self organizer packageOf: class) equals: YPackage		