tests
testClassCreationShouldAnnounceClassAdded

	self subscribeOn: ClassAdded.
	
	"Create a class"
	aClass := self makeWithLayout: FixedLayout.
	
	self assert: self collectedAnnouncements size equals: 1. 
	self assert: self collectedAnnouncements first classAdded equals: aClass.