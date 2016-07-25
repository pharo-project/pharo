tests - operations on methods
testModifyMethodByMovingFromExtensionCategoryToClassicCategoryMoveItFromExtendingPackageToClassPackage

	"test that when we move a method from an extension category ( begining with *) to a classic category , the method is moved from the extending package to the parent package of the class"
	
	| class firstPackage secondPackage|
	self addXYCategory.
	
	firstPackage := self organizer  packageNamed: #XXXXX.
	secondPackage := self organizer  packageNamed: #YYYYY.
	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.
	self createMethodNamed: 'stubMethod' inClass:  class inCategory: '*yyyyy'.

	class organization classify: #stubMethod under: 'classic category'.
	self assert: (firstPackage includesDefinedSelector: #stubMethod ofClass: class).
	self deny: (secondPackage includesExtensionSelector: #stubMethod ofClass: class).
	self assert: ((class >> #stubMethod) packageFromOrganizer: self organizer ) = firstPackage.