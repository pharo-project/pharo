tests - operations on methods
testTraitCompositionMethodsArePackagedWithTheirTraits
	"test that when we remove a class method coming from a trait, the method is not removed from the parent package of the trait"
	
	|XPackage YPackage class trait|
	self addXCategory.
	self addYCategory.
	XPackage := self organizer packageNamed: #XXXXX.
	YPackage := self organizer packageNamed: #YYYYY.
	
	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.
	trait := self createNewTraitNamed: 'NewTrait' inCategory: 'YYYYY'.
	
	self createMethodNamed: 'stubMethod' inClass: trait  inCategory: 'classic protocol'. 
	class setTraitComposition: trait asTraitComposition.
	
	self assert: (YPackage includesDefinedSelector: #stubMethod ofClass: trait). 
	self deny: (XPackage includesDefinedSelector: #stubMethod ofClass: class).