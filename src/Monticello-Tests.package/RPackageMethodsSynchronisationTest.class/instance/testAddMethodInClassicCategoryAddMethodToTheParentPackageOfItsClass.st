tests - operations on methods
testAddMethodInClassicCategoryAddMethodToTheParentPackageOfItsClass
	"test that when we add a method to a  class in a classic category (not beginning with *), this method is added to the parent package of the class"
	
	|tmpPackage class|
	self addXCategory.
	
	tmpPackage := self organizer packageNamed: #XXXXX.
	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.

	self createMethodNamed: 'stubMethod' inClass:  class inCategory: 'classic category'.

	self assert: (class package includesDefinedSelector: #stubMethod ofClass: class).
	self assert: ((class >> #stubMethod) packageFromOrganizer: self organizer ) equals: class package.