we are not sure
testAddMethodInExtensionCategoryNotRespectingCaseAddMethodToTheExtendingPackage
	"test that when we add a method  in an extension category ( beginning with *)thae does not match the case of the corresponding package (for example *packagea for PackageA), this method is added to the corresponding extending package"
	
	| class firstPackage secondPackage|
	self addXYCategory.
	firstPackage := self organizer packageNamed: #XXXXX.
	secondPackage := self organizer packageNamed: #YYYYY.
	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.
	
	self createMethodNamed: 'stubMethod' inClass:  class inCategory: '*yyYyY'.
	
	self assert: (secondPackage includesExtensionSelector: #stubMethod ofClass: class).
	self deny: (firstPackage includesDefinedSelector: #stubMethod ofClass: class).
	
	self assert: ((class >> #stubMethod) packageFromOrganizer: self organizer ) equals: secondPackage.