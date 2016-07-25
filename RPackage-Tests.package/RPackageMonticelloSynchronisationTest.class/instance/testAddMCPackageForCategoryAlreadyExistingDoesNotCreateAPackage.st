tests - operations on MCPackages
testAddMCPackageForCategoryAlreadyExistingDoesNotCreateAPackage
	"test that when we create a MCPackage and that a category of this name already exists, no package is created"
	"self debug: #testAddMCPackageForCategoryAlreadyExistingDoesNotCreateAPackage"
	"self run: #testAddMCPackageForCategoryAlreadyExistingDoesNotCreateAPackage"	
	
	|tmpPackage|
	Smalltalk globals organization addCategory: 'Zork'.
	tmpPackage := self organizer packageNamed: #Zork.
	MCWorkingCopy forPackage: (MCPackage new name: #Zork).
	self assert: tmpPackage = (self organizer packageNamed: #Zork).
		
		
					  