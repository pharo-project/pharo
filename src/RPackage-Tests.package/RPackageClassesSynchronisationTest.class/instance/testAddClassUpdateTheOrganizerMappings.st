tests - adding classes
testAddClassUpdateTheOrganizerMappings
	"test that when we add a class, the organizer 'classPackageMapping' dictionary is updated, so that the class is linked to its package and so that we can access its owning package"
	"self debug: #testAddClassUpdateTheOrganizer"
	|tmpPackage class|
	self addXCategory.
	
	tmpPackage := self organizer packageNamed: #XXXXX.
	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.
	 
	self assert: class package equals: tmpPackage