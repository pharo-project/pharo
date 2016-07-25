tests - operations on protocols
testReorganizeClassByRenamingExtensionCategoryToClassicCategoryMoveAllMethodsFromExtendingPackageToParentPackage
	"test that when we reoganized a class by renaming an extension category (a category beginning with '*') to a classic category, all the methods are moved from the  extendingPackage to the parent package of the class"
	"self debug: #testReoganizeClassByRenamingExtensionCategoryToClassicCategoryMoveAllMethodsFromExtendingPackageToParentPackage"
	
	|XPackage YPackage class| 
	self addXYCategory.
	XPackage := self organizer packageNamed: #XXXXX.
	YPackage  := self organizer packageNamed: #YYYYY.
	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.
	
	self createMethodNamed: 'stubMethod' inClass: class  inCategory: '*yyyyy'.
	self createMethodNamed: 'stubMethod2' inClass: class  inCategory: '*yyyyy'.
	
	class organization renameCategory: '*yyyyy' toBe: 'classic category'.
	
	self deny: (YPackage includesExtensionSelector: #stubMethod ofClass: class).
	self deny: (YPackage includesExtensionSelector: #stubMethod2 ofClass: class).
	self assert: (XPackage includesDefinedSelector: #stubMethod ofClass: class).
	self assert: (XPackage includesDefinedSelector: #stubMethod2 ofClass: class).