tests - recategorizing class
testRecategorizeClassUnregisterTheClassMethodsFromTheOldPackage
	"test that when we recategorize a class, the old package in which it was defined don't include its defined methods (not extensions) anymore"
	"self debug: #testRecategorizeClassUnregisterTheClassMethodsFromTheOldPackage"
		
	|XPackage YPackage ZPackage class|
	self addXCategory.
	self addYCategory.
	self addZCategory.
	XPackage := self organizer packageNamed: #XXXXX.
	YPackage := self organizer packageNamed: #YYYYY.
	ZPackage := self organizer packageNamed: #ZZZZZ.
	
	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.
	self createMethodNamed: 'method1' inClass: class inCategory: 'category'.
	self createMethodNamed: 'method2' inClass: class inCategory: '*zzzzz'.
	self createMethodNamed: 'method3' inClass: class inCategory: '*yyyyy'.
	
	class category: 'YYYYY'.
	self deny: (XPackage includesSelector: #method1 ofClass: class ). 
	self deny: (XPackage includesSelector: #method2 ofClass: class ). 
	self deny: (XPackage includesSelector: #method3 ofClass: class ).