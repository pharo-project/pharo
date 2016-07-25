we are not sure
testAddMethodInExtensionCategoryMatchingNameAddMethodToTheExtendingPackage
	"test that when we add a method  in an extension category ( beginning with*) that enlarge a package name (for example *mondrian-accessing for Mondrian), this method is added to the corresponding extending package"
	
	| class XPackage YPackage|
	self addXYCategory.
	XPackage := self organizer packageNamed: #XXXXX.
	YPackage := self organizer packageNamed: #YYYYY.
	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.

	self createMethodNamed: #newMethod inClass:  class inCategory: '*YYYYY-subcategory'.

	self assert: (YPackage includesExtensionSelector: #newMethod ofClass: class).
	self deny: (XPackage includesDefinedSelector: #newMethod ofClass: class).

	self assert: ((class >> #newMethod) packageFromOrganizer: self organizer ) = YPackage