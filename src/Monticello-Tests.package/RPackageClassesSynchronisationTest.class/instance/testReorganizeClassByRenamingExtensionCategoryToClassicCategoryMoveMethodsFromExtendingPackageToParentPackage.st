tests - operations on protocols
testReorganizeClassByRenamingExtensionCategoryToClassicCategoryMoveMethodsFromExtendingPackageToParentPackage
	"test that when we reoganized a class by renaming an extension category (a category beginning with '*') to a classic category, all the methods are moved from the  extendingPackage to the parent package of the class"
	
	|XPackage YPackage class| 
	self addXYCategory.
	XPackage := self organizer packageNamed: #XXXXX.
	YPackage  := self organizer packageNamed: #YYYYY.
	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.
	self createMethodNamed: 'stubMethod' inClass: class  inCategory: '*yyyyy'.
	class organization renameCategory: '*yyyyy' toBe: 'classic category'.
	self deny: (YPackage includesExtensionSelector: #stubMethod ofClass: class).
	self assert: (XPackage includesDefinedSelector: #stubMethod ofClass: class)