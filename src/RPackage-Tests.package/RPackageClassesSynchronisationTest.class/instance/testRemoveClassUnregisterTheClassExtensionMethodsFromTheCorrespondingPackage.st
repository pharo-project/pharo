tests - removing classes
testRemoveClassUnregisterTheClassExtensionMethodsFromTheCorrespondingPackage
	"test that when we remove a class, the class extension methods are all removed  from theire corresponding parent Package"
	
	|XPackage YPackage class|
	self addXYCategory.
	XPackage := self organizer packageNamed: #XXXXX.
	YPackage := self organizer packageNamed: #YYYYY.
	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.
	self createMethodNamed: 'stubMethod' inClass: class  inCategory: '*yyyyy'. 
	
	Smalltalk removeClassNamed: 'NewClass'. 
	self deny: (YPackage includesSelector: #stubMethod ofClass: class). 