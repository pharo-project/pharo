tests
testLayoutChangeShouldAnnounceClassModified

	| classAdded classModified addedFormat |
	self subscribeOn: ClassModifiedClassDefinition.
	
	"Create a class"
	classAdded := self makeWithLayout: FixedLayout.
	addedFormat := classAdded format.
	
	"Modify its format"
	classModified := self makeWithLayout: WeakLayout.

	self assert: self collectedAnnouncements size equals: 1.
	announcement := self collectedAnnouncements first.
	self assert: announcement newClassDefinition equals: classModified.
	self assert: announcement oldClassDefinition format equals: addedFormat.
	self assert: announcement newClassDefinition format equals: classModified format.