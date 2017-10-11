tests - operations on methods
testModifyMethodByMovingFromClassicCategoryToExtensionCategoryMoveItFromClassPackageToExtendingPackage
	"test that when we move a method from a classic category (not begining with *) to an extension category , the method is moved from the parent package of the class to the extending package"
	
	| class firstPackage secondPackage|
	self addXYCategory. 
	firstPackage := self organizer  packageNamed: #XXXXX.
	secondPackage := self organizer  packageNamed: #YYYYY.
	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.
	self createMethodNamed: 'stubMethod' inClass:  class inCategory: 'classic category'.

	class organization classify: #stubMethod under: '*yyyyy'.	
	self deny: (firstPackage includesDefinedSelector: #stubMethod ofClass: class).
	self assert: (secondPackage includesExtensionSelector: #stubMethod ofClass: class).
	self assert: ((class >> #stubMethod) packageFromOrganizer: self organizer ) = secondPackage.
	
	class organization classify: #stubMethod under: '*yyyyy-subcategory'.
	self deny: (firstPackage includesDefinedSelector: #stubMethod ofClass: class).
	self assert: (secondPackage includesExtensionSelector: #stubMethod ofClass: class).
	self assert: ((class >> #stubMethod) packageFromOrganizer: self organizer ) = secondPackage.