tests - operations on classes
testRenameClassUpdateClassDefinedSelectorsInTheParentPackage
	"test that when we rename a class, the  'classDefinedSelectors' dictionary of the parent package is updated with the new name. There fore we test that we can correctly access the selector from the package by specifying the right name (the new name)"
	
	|XPackage  class refactoring|
	self addXCategory.
	XPackage := self organizer packageNamed: #XXXXX.
	class := self createNewClassNamed: 'RPackageOldStubClass' asSymbol inCategory: 'XXXXX'.
	self createMethodNamed: 'stubMethod' inClass: class  inCategory: 'classic category'.
	
	class rename: 'RPackageNewStubClass'.
	
	self assert: (XPackage includesDefinedSelector: #stubMethod ofClassName: 'RPackageNewStubClass' asSymbol ). 
	self deny: (XPackage includesDefinedSelector: #stubMethod ofClassName: 'RPackageOldStubClass' asSymbol ).