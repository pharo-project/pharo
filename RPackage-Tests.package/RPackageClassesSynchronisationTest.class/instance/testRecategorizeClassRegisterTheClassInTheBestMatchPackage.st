tests - recategorizing class
testRecategorizeClassRegisterTheClassInTheBestMatchPackage
	"test that when we recategorize a class, the new package in which it is defined include it"
	
	|XPackage  YPackage class|
	self addXCategory.
	self addXMatchCategory.
	self addYCategory.
	
	XPackage := self organizer packageNamed: #XXXXX.
	YPackage := self organizer packageNamed: #YYYYY.
	
	class := self createNewClassNamed: 'NewClass' inCategory: 'YYYYY'.
	
	class category: 'XXXXX-YYYY'.
	self assert: (self organizer packageOf: class) = XPackage. 
	self deny: (self organizer packageOf: class) = YPackage.
