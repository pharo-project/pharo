tests - operations on methods
testModifyMethodByMovingFromExtensionCategoryToExtensionCategoryMoveItFromExtendingPackageToSecondExtendingPackage
	"test that when we move a method from an extension category ( begining with *) to another extending package , the method is moved from the extending package to the other extending package"
	
	| class XPackage YPackage ZPackage|
	self addXYZCategory.
	XPackage := self organizer  packageNamed: #XXXXX.
	YPackage := self organizer  packageNamed: #YYYYY.
	ZPackage := self organizer  packageNamed: #ZZZZZ.
	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.
	self createMethodNamed: #newMethod inClass:  class inCategory: '*yyyyy'.

	class organization classify: #newMethod under: '*zzzzz'.
	self deny: (XPackage includesDefinedSelector: #newMethod ofClass: class).
	self deny: (YPackage includesExtensionSelector: #newMethod ofClass: class).
	self assert: (ZPackage includesExtensionSelector: #newMethod ofClass: class).
	self assert: ((class >> #newMethod) packageFromOrganizer: self organizer ) = ZPackage.