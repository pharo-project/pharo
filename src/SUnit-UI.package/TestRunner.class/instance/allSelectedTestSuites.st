accessing
allSelectedTestSuites
	"Return the suite for all the selected test case classes"
	
	^ classesSelected select: [ :each | each isAbstract not] thenCollect: [:each | each suite].