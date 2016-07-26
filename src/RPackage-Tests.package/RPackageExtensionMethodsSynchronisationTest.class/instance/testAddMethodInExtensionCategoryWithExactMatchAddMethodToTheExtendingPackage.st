we are not sure
testAddMethodInExtensionCategoryWithExactMatchAddMethodToTheExtendingPackage
	"test that when we add a method to a  class in an extension category ( beginning with *), this method is added to the corresponding extending package"
	
	| class firstPackage secondPackage|
	self addXYCategory.		
	self addXMatchCategory.
	
	firstPackage := self organizer packageNamed: #XXXXX.
	secondPackage := self organizer packageNamed: #YYYYY.
	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.
	
	
	self createMethodNamed: 'stubMethod' inClass: class inCategory: '*YYYYY'.
	
	self assert: (secondPackage includesExtensionSelector: #stubMethod ofClass: class).
	self deny: (firstPackage includesDefinedSelector: #stubMethod ofClass: class).
	
	self assert: ((class >> #stubMethod) packageFromOrganizer: self organizer) = secondPackage