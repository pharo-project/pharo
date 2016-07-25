tests - removing classes
testRemoveClassUpdateTheOrganizerMappings
	"test that when we remove a class, the organizer is updated so that the class is no longer present in the  'classPackageDictionary' dictionary"
	
	|XPackage  class|
	self addXCategory.
	XPackage := self organizer packageNamed: #XXXXX.
	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.

	Smalltalk removeClassNamed: 'NewClass'. 
	self deny: (self organizer includesPackageBackPointerForClass: class)