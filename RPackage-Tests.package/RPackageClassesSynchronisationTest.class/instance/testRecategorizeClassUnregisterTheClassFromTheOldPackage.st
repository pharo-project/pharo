tests - recategorizing class
testRecategorizeClassUnregisterTheClassFromTheOldPackage
	"test that when we recategorize a class, the old package in which it was defined don't include it anymore"
	

	|XPackage YPackage class|
	self addXCategory.
	self addYCategory.
	XPackage := self organizer packageNamed: #XXXXX.
	YPackage := self organizer packageNamed: #YYYYY.
	
	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.

	class category: 'YYYYY'.
	self deny: (XPackage includesClass: class).