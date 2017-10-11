tests - operations on methods
testRemoveExtensionMethodDoesNotRemoveExtendingPackage
	"test that when we remove a method that is an extension from an external package, the package is not removed from the the class extending packages if another extension still exist"

	|XPackage  class YPackage|
	self addXYCategory.
	XPackage := self organizer packageNamed: #XXXXX.
	YPackage  := self organizer packageNamed: #YYYYY.
	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.
	self createMethodNamed: 'stubMethod' inClass: class  inCategory: '*yyyyy'. 
	self createMethodNamed: 'stubMethod2' inClass: class  inCategory: '*yyyyy'. 
	
	self assert: (YPackage includesExtensionSelector: #stubMethod ofClass: class). 
	self assert: (YPackage includesExtensionSelector: #stubMethod2 ofClass: class). 

	"If we only remove one of the extension methods, the package should still be extending the class."
	class removeSelector: #stubMethod.
	self assert: (YPackage extendsClass: class).
	self assert: (YPackage extendsClass: class classSide).