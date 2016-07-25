tests - operations on categories
testAddCategoryIsAlreadyAPackageDoesNotCreatePackage
	"test that when we add a category corresponding to an existing package, we do not create a second package"
	"self debug: #testAddCategoryIsAlreadyAPackageDoesNotCreatePackage"
	
	|tmpPackage|
	
	self flag: #todo. "To be removed when Categories are kicked out"
	
	self addXCategory.
	tmpPackage := self organizer packageNamed: #XXXXX.
	self addXCategory.
	self assert: tmpPackage =  (self organizer packageNamed: #XXXXX)
		