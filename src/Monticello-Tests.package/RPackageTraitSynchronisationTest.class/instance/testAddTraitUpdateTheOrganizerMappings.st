tests - operations on traits
testAddTraitUpdateTheOrganizerMappings
	"test that when we add a Trait, the organizer 'classPackageMapping' dictionary is updated, so that the trait is linked to its package and so that we can access its owning package"
	"self debug: #testAddClassUpdateTheOrganizer"
	|tmpPackage class|
	self addXCategory.
	
	tmpPackage := self organizer packageNamed: #XXXXX.
	class := self createNewTraitNamed: 'NewClass' inCategory: 'XXXXX'.
	 
	self assert: class package equals: tmpPackage