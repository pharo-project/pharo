tests - operations on methods
testAddMethodByUsingATraitDoesNotAddTheMethodToThePackage
	"test that when a method is added to a class bu using a trait, this method is not imported in the parent package of the class"
	"self debug: #testAddMethodByUsingATraitDoesNotAddTheMethodToThePackage"
	
	|XPackage YPackage class trait|
	self addXCategory.
	self addYCategory.
	XPackage := self organizer packageNamed: #XXXXX.
	YPackage := self organizer packageNamed: #YYYYY.
	trait := self createNewTraitNamed: 'NewTrait' inCategory: 'YYYYY'.
	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.
	self createMethodNamed: 'stubMethod' inClass:  trait inCategory: 'trait category'.
	
	self assert: (XPackage methods isEmpty ).
	self deny: (YPackage methods isEmpty ).
	class addToComposition: trait.
	self assert: (YPackage includesDefinedSelector: #stubMethod ofClass: trait).
	self assert: (XPackage methods isEmpty).
		