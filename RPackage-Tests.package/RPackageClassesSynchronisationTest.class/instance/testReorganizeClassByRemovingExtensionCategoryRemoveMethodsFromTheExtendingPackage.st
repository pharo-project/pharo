tests - operations on protocols
testReorganizeClassByRemovingExtensionCategoryRemoveMethodsFromTheExtendingPackage
	"test that when we reoganized a class by removing an extension category (a category beginning with '*'), all the methods that were inside are removed from the extending package"
	 
	|XPackage YPackage class| 
	self addXYCategory.
	XPackage := self organizer packageNamed: #XXXXX.
	YPackage  := self organizer packageNamed: #YYYYY.
	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.
	
	self createMethodNamed: 'stubMethod' inClass: class  inCategory: '*yyyyy'.
	
	class removeProtocol: '*yyyyy'.
	
	self deny: (YPackage includesExtensionSelector: #stubMethod ofClass: class).
	self deny: (XPackage includesDefinedSelector: #stubMethod ofClass: class).