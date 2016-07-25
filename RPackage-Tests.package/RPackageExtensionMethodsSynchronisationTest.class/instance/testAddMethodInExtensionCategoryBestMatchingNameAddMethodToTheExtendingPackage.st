we are not sure
testAddMethodInExtensionCategoryBestMatchingNameAddMethodToTheExtendingPackage
	"test that when we add a method  in an extension category ( beginning with*) that enlarge a package name (for example *mondrian-accessing for Mondrian), this method is added to the corresponding extending package"
	
	| class XPackage YPackage |

	self addXYCategory.
	self addXMatchCategory.
	
	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX-YYYY'.
	
	XPackage := self organizer packageNamed: #XXXXX.
	YPackage := self organizer packageNamed: #YYYYY.
	
	class := self createNewClassNamed: 'NewClass' inCategory: 'YYYYY'.

	self createMethodNamed: #newMethod inClass:  class inCategory: '*XXXXX-YYYY'.

	self deny: (YPackage includesDefinedSelector: #newMethod ofClass: class).
	self assert: (XPackage includesExtensionSelector: #newMethod ofClass: class).

	self assert: ((class >> #newMethod) packageFromOrganizer: self organizer ) = XPackage