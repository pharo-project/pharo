tests - operations on protocols
testReorganizeClassByRemovingClassicCategoryRemoveMethodsFromTheParentPackage
	"test that when we reoganized a class by removing a classic category (a category not beginning with '*'), the methods from this category are removed from the parent RPackage of the class"
	
	|XPackage  class| 
		
	self addXCategory.
	XPackage := self organizer packageNamed: 'XXXXX'.
	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.
	self createMethodNamed: 'stubMethod' inClass: class  inCategory: 'classic category'.
	class removeProtocol: 'classic category'.
	self deny: (XPackage includesDefinedSelector: #stubMethod ofClass: class).