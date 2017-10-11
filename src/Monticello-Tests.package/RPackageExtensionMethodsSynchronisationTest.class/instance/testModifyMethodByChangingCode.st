tests - operations on methods
testModifyMethodByChangingCode
	"test that when we modify the code of a method, everything work well: NOTHING SHOULD HAPPEN REGARDING THE PACKAGING"
	
	| class firstPackage secondPackage|
	self addXCategory.
	firstPackage := self organizer  packageNamed: #XXXXX.
	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.
	self createMethodNamed: 'stubMethod' inClass:  class inCategory: 'classic category'.

	class compile: 'stubMethod ^22222222222'.
	
	"nothing should change"
	self assert: (class >> #stubMethod) category = 'classic category'. 
	self assert: (firstPackage includesDefinedSelector: #stubMethod ofClass: class).
	self deny: (firstPackage includesExtensionSelector: #stubMethod ofClass: class).
	self assert: ((class >> #stubMethod) packageFromOrganizer: self organizer ) = firstPackage.