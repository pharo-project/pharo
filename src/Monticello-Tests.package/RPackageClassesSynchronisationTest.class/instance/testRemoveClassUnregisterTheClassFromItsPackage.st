tests - removing classes
testRemoveClassUnregisterTheClassFromItsPackage
	"test that when we remove a class, the class is removed from its parent Package"	

	|XPackage  class|
	self addXCategory.
	XPackage := self organizer packageNamed: #XXXXX.
	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.
	
	Smalltalk removeClassNamed: 'NewClass'. 
	self deny: (XPackage includesClass: class)