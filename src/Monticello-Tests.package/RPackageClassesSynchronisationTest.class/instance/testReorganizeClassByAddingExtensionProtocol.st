tests - operations on protocols
testReorganizeClassByAddingExtensionProtocol
	"test that when we reoganized a class by adding a category, nothing change from  RPackage point of vue."
	
	|XPackage YPackage class| 

	self addXCategory.
	XPackage := self organizer packageNamed: #XXXXX.
	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.
	self createMethodNamed: 'newMethod' inClass: class inCategory: 'xxxxx'.  
	class organization addCategory: '*yyyyy' before: nil.
	
	self assert: (self organizer includesPackageNamed: #Yyyyy).