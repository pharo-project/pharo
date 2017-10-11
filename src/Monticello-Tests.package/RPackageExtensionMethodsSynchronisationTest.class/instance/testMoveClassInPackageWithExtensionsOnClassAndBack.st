tests - operations on methods
testMoveClassInPackageWithExtensionsOnClassAndBack
	"Move a class in package XXXXX (with extensions from YYYY) to package YYYYY."
	
	| class firstPackage secondPackage|
	self addXYCategory.
	firstPackage := self organizer  packageNamed: #XXXXX.
	secondPackage := self organizer packageNamed: #YYYYY.
	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.
	self createMethodNamed: 'stubMethod' inClass:  class inCategory: secondPackage methodCategoryPrefix.

	secondPackage addClass: class.
	
	"Everything should now be in second package (and not listed as an extension, but instead as 'as yet unclassified')."

	self assert: (class >> #stubMethod) category = Protocol defaultName. 
	self assert: (secondPackage includesDefinedSelector: #stubMethod ofClass: class).
	self deny: (secondPackage includesExtensionSelector: #stubMethod ofClass: class).
	self assert: ((class >> #stubMethod) packageFromOrganizer: self organizer ) = secondPackage.
	
	"Moving back, we should not see the extension reappear."

	firstPackage addClass: class.
	
	self assert: (class >> #stubMethod) category = Protocol defaultName. 
	self assert: (firstPackage includesDefinedSelector: #stubMethod ofClass: class).
	self deny: (secondPackage includesExtensionSelector: #stubMethod ofClass: class)
