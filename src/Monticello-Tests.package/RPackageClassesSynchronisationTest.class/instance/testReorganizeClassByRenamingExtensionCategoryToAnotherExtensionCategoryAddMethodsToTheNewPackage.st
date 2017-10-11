tests - operations on protocols
testReorganizeClassByRenamingExtensionCategoryToAnotherExtensionCategoryAddMethodsToTheNewPackage
	"test that when we reoganized a class by renaming an extension category (a category beginning with '*') to another extension category, all the methods are moved to the new extendingPackage"
	
	|XPackage YPackage ZPackage class| 
	self addXYZCategory.
	XPackage := self organizer packageNamed: #XXXXX.
	YPackage  := self organizer packageNamed: #YYYYY.
	ZPackage  := self organizer packageNamed: #ZZZZZ.
	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.
	self createMethodNamed: 'stubMethod' inClass: class  inCategory: '*yyyyy'.
	class organization renameCategory: '*yyyyy' toBe: '*zzzzz'.
	self assert: (ZPackage includesExtensionSelector: #stubMethod ofClass: class).
	self deny: (YPackage includesExtensionSelector: #stubMethod ofClass: class).
	self deny: (XPackage includesDefinedSelector: #stubMethod ofClass: class)