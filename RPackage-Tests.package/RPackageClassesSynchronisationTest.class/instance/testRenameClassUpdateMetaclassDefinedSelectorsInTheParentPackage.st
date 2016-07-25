tests - operations on classes
testRenameClassUpdateMetaclassDefinedSelectorsInTheParentPackage
	"test that when we rename a class, the  'metaclassDefinedSelectors' dictionary of the parent package is updated with the new name. Ther fore we test that we can correctly access the selector from the package by specifying the right name (the new name)"
	
	|XPackage  class refactoring|
	self addXYCategory.
	XPackage := self organizer packageNamed: #XXXXX.
	class := self createNewClassNamed: 'RPackageOldStubClass' asSymbol inCategory: 'XXXXX'.
	self createMethodNamed: 'stubMethod' inClassSideOfClass: class  inCategory: 'classic category'.
	
	class rename: 'RPackageNewStubClass'.
	
	self assert: (XPackage  includesDefinedSelector: #stubMethod ofMetaclassName: 'RPackageNewStubClass' asSymbol ). 
	self deny: (XPackage  includesDefinedSelector: #stubMethod ofMetaclassName: 'RPackageOldStubClass' asSymbol ).