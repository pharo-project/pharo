tests - recategorizing class
testRecategorizeClassWithMetaClassMethodsRegisterAllClassMethodsInTheNewPackage

	"test that when we recategorize a class (having methods defined in both instance and class side), the new package in which it is defined include all the methods (from instance and class side) defined in this class (not extensions)"
	
	| addedCount addedCat XPackage YPackage ZPackage class|
	self addXCategory.
	self addYCategory.
	self addZCategory.
	XPackage := self organizer packageNamed: #XXXXX.
	YPackage := self organizer packageNamed: #YYYYY.
	ZPackage := self organizer packageNamed: #ZZZZZ.
	
	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.
	
	self createMethodNamed: 'method1' inClass: class theMetaClass inCategory: 'category'.
	self createMethodNamed: 'method2' inClass: class theMetaClass inCategory: '*yyyyy'.
	self createMethodNamed: 'method3' inClass: class theMetaClass inCategory: '*zzzzz'.
	
	class category: 'YYYYY'.
	
	"lets check metaclass methods"
	self assert: (YPackage includesDefinedSelector: #method1 ofMetaclassName: class name).
	
	self assert: (YPackage includesDefinedSelector: #method2 ofMetaclassName: class name). 
	
	self assert: (ZPackage includesExtensionSelector: #method3 ofMetaclassName: class name ).