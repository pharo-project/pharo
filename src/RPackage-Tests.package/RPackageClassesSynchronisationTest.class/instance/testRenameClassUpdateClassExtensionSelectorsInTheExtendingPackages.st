tests - operations on classes
testRenameClassUpdateClassExtensionSelectorsInTheExtendingPackages
	"test that when we rename a class, the  'classExtensionSelectors' dictionaries of the extending packages  are updated with the new name. Therfore we test that we can correctly access the selectors from the package by specifying the right name (the new name)"

	|XPackage YPackage class refactoring|
	self addXYCategory.
	XPackage := self organizer packageNamed: #XXXXX.
	YPackage := self organizer packageNamed: #YYYYY.
	class := self createNewClassNamed: 'RPackageOldStubClass' asSymbol inCategory: 'XXXXX'.
	self createMethodNamed: 'stubMethod' inClass: class  inCategory: '*yyyyy'.
	
	class rename: 'RPackageNewStubClass'.
	
	self assert: (YPackage includesExtensionSelector: #stubMethod ofClassName: 'RPackageNewStubClass' asSymbol ). 
	self deny: (YPackage includesExtensionSelector: #stubMethod ofClassName: 'RPackageOldStubClass' asSymbol ).