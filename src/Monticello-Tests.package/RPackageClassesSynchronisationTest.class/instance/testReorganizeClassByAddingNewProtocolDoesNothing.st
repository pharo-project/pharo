tests - operations on protocols
testReorganizeClassByAddingNewProtocolDoesNothing
	"test that when we reoganized a class by adding a category, nothing change from  RPackage point of vue."
	
	|XPackage YPackage class| 
	self addXCategory.
	XPackage := self organizer packageNamed: #XXXXX.
	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.
	self createMethodNamed: 'newMethod' inClass: class inCategory: 'xxxxx'.  
	class organization addCategory: 'accessing' before: nil.
	
	self assert: (XPackage includesClass: class).
	self assert: (XPackage includesDefinedSelector: #newMethod ofClass: class).
	self assert: (XPackage extensionMethods size = 0).