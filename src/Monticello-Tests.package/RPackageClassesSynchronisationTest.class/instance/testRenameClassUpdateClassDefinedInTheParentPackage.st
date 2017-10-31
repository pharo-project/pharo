tests - operations on classes
testRenameClassUpdateClassDefinedInTheParentPackage
	"test that when we rename a class, the classes Defined in the parent package includes the new name"
	"self debug: #testRenameClassUpdateClassDefinedInTheParentPackage"
	
	|XPackage  class refactoring|
	self addXCategory.
	XPackage := self organizer packageNamed: #XXXXX.
	
	class := self createNewClassNamed: 'RPackageOldStubClass' inCategory: 'XXXXX'.	
	class rename: 'RPackageNewStubClass'.
	
	self assert: (XPackage includesClassNamed: 'RPackageNewStubClass'). 
	self deny: (XPackage includesClassNamed: 'RPackageOldStubClass').