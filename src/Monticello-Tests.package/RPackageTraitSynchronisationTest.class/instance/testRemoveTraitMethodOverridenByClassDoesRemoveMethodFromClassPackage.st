tests - operations on methods
testRemoveTraitMethodOverridenByClassDoesRemoveMethodFromClassPackage
	"test that when we remove a class method overriding the one from a trait, the method is removed from the class package"
	
	|XPackage YPackage class trait|
	self addXCategory.
	self addYCategory.
	XPackage := self organizer packageNamed: #XXXXX.
	YPackage := self organizer packageNamed: #YYYYY.
	
	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.
	trait := self createNewTraitNamed: 'NewTrait' inCategory: 'YYYYY'.
	
	self createMethodNamed: 'stubMethod' inClass: trait  inCategory: 'classic protocol'. 
	class setTraitComposition: trait asTraitComposition.
	self createMethodNamed: 'stubMethod' inClass: class  inCategory: 'classic protocol'. 
	
	class removeSelector: #stubMethod.
	self deny: (XPackage includesDefinedSelector: #stubMethod ofClass: class). 