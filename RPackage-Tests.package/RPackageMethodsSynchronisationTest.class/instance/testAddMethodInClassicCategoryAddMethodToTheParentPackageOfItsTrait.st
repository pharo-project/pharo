tests - operations on methods
testAddMethodInClassicCategoryAddMethodToTheParentPackageOfItsTrait
	"test that when we add a method to a  trait in a classic category (*not beginning with *), this method is added to the parent package of the class"
	
	|tmpPackage trait|
	self addXCategory.

	tmpPackage := self organizer packageNamed: #XXXXX.
	trait := self createNewTraitNamed: 'NewClass' inCategory: 'XXXXX'.

	self createMethodNamed: 'stubMethod' inClass:  trait inCategory: 'classic category'.

	self assert: (trait package includesDefinedSelector: #stubMethod ofClass: trait).
	self assert: ((trait >> #stubMethod) packageFromOrganizer: self organizer ) = trait package