tests - operations on classes
testRenameClassUpdateMetaclassExtensionSelectorsInTheExtendingPackages
	"test that when we rename a class, the  'metaclassExtensionSelectors' dictionaries of the extending packages  are updated with the new name. Ther fore we test that we can correctly access the selectors from the package by specifying the right name (the new name)"
	
	|XPackage YPackage class refactoring|
	self addXYCategory.
	XPackage := self organizer packageNamed: #XXXXX.
	YPackage := self organizer packageNamed: #YYYYY.
	class := self createNewClassNamed: 'RPackageOldStubClass' asSymbol inCategory: 'XXXXX'.
	self createMethodNamed: 'stubMethod' inClassSideOfClass: class  inCategory: '*yyyyy'.
	
	class rename: 'RPackageNewStubClass'.
	
	self assert: (YPackage includesExtensionSelector: #stubMethod ofMetaclassName: 'RPackageNewStubClass' asSymbol ). 
	self deny: (YPackage includesExtensionSelector: #stubMethod ofMetaclassName: 'RPackageOldStubClass' asSymbol ).