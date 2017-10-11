tests - operations on methods
testRemoveMethodRemoveMethodFromItsPackage
	"test that when we remove a method, the method is remod from the package in which the method is defined"
	
	|XPackage  class|
	self addXCategory.
	XPackage := self organizer packageNamed: #XXXXX.
	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.
	self createMethodNamed: 'stubMethod' inClass: class  inCategory: 'classic category'. 
	
	class removeSelector: #stubMethod.
	
	self deny: (XPackage includesDefinedSelector: #stubMethod ofClass: class). 
	self deny: (XPackage includesExtensionSelector: #stubMethod ofClass: class). 
	self deny: (XPackage includesSelector: #stubMethod ofClass: class).