tests - operations on classes
testRenameClassUpdateOrganizerClassExtendingPackagesMapping
	"test that when we rename a class, the classExtendingPackages dictionary of the organizer is updated with the new name, so that we can access the packages when specifying the new name"
	
	|XPackage YPackage class refactoring|
	self addXYCategory.
	XPackage := self organizer packageNamed: #XXXXX.
	YPackage := self organizer packageNamed: #YYYYY.
	class := self createNewClassNamed: 'RPackageOldStubClass' inCategory: 'XXXXX'.
	self createMethodNamed: #stubMethod inClass: class inCategory: '*yyyyy'. 
	
	class rename: 'RPackageNewStubClass'.
	
	self assert: ((self organizer extendingPackagesOfClassNamed: 'RPackageNewStubClass' asSymbol) includes: YPackage). 
	self deny: ((self organizer extendingPackagesOfClassNamed: 'RPackageOldStubClass' asSymbol) includes: YPackage).