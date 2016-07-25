tests - operations on protocols
testReorganizeClassByRenamingClassicCategoryToExtensionCategoryMoveMethodsFromParentPackageToExtendingPackage
	"test that when we reoganized a class by renaming a  classic category (a category not beginning with '*') to an extension category, all the methods are moved from the  parent package of the class to the extending package"

	|XPackage YPackage class| 
	self addXYCategory.
	XPackage := self organizer packageNamed: #XXXXX.
	YPackage  := self organizer packageNamed: #YYYYY.
	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.
	self createMethodNamed: 'stubMethod' inClass: class  inCategory: 'classic category'.
	class organization renameCategory: 'classic category' toBe: '*yyyyy'.
	self assert: (YPackage includesExtensionSelector: #stubMethod ofClass: class).
	self deny: (XPackage includesDefinedSelector: #stubMethod ofClass: class)