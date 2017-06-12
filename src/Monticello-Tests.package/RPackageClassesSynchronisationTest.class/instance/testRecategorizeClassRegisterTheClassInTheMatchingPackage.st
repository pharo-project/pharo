tests - recategorizing class
testRecategorizeClassRegisterTheClassInTheMatchingPackage
	"test that when we recategorize a class, the new package in which it is defined include it"
	
	|XPackage YPackage class|
	self addXCategory.
	self addYCategory.
	
	XPackage := self organizer packageNamed: #XXXXX.
	YPackage := self organizer packageNamed: #YYYYY.
	
	class := self createNewClassNamed: 'NewClass' inCategory: 'YYYYY'.
	
	class category: 'XXXXX'.
	self deny: (self organizer packageOf: class) = YPackage. 
	self assert: (self organizer packageOf: class) = XPackage.