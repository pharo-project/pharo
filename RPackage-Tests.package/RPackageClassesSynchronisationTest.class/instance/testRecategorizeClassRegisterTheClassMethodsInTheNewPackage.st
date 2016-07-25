tests - recategorizing class
testRecategorizeClassRegisterTheClassMethodsInTheNewPackage

	"test that when we recategorize a class, the new package in which it is defined include all the methods defined in this class (not extensions)"
	
	| addedCount addedCat XPackage YPackage ZPackage class|
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
	
	self assert: (YPackage includesDefinedSelector: #method1 ofClass: class ). 
	self deny: (XPackage includesSelector: #method1 ofClass: class ). 
	self deny: (ZPackage includesSelector: #method1 ofClass: class ). 
	
	self deny: (XPackage includesSelector: #method2 ofClass: class ). 
	self deny: (YPackage includesSelector: #method2 ofClass: class ). 
	self assert: (ZPackage includesExtensionSelector: #method2 ofClass: class ). 

	self deny: (XPackage includesSelector: #method3 ofClass: class ). 
	self assert: (YPackage includesDefinedSelector: #method3 ofClass: class ). 
	self deny: (ZPackage includesSelector: #method3 ofClass: class ). 
