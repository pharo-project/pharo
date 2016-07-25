tests - operations on classes
testRenameClassUpdateOrganizerClassPackageMapping
	"test that when we rename a class, the classPackage dictionary of the organizer is updated with the new name, so that we can access the package when specifying the new name"
	
	|XPackage class refactoring|
	self addXYCategory.
	XPackage := self organizer packageNamed: #XXXXX.
	class := self createNewClassNamed: 'RPackageOldStubClass' inCategory: 'XXXXX'.
	
	class rename: 'RPackageNewStubClass'.
	
	self assert: ((self organizer packageOfClassNamed: 'RPackageNewStubClass' asSymbol) = XPackage). 
	self assert: ((self organizer packageOfClassNamed: 'RPackageOldStubClass' asSymbol) = nil).