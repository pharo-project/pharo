tests
testSlotAdditionShouldAnnounceClassModified

	| classAdded classModified |
	self subscribeOn: ClassModifiedClassDefinition.
	
	"Create a class"
	classAdded := self makeWithLayout: FixedLayout.
	
	"now we extend it with slots.  The class definition should've changed"
	classModified := self makeWithLayout: FixedLayout andSlots: { #aSlot }.

	self assert: self collectedAnnouncements size equals: 1.
	announcement := self collectedAnnouncements first.	
	self assert: announcement newClassDefinition equals: classModified.
	self assert: announcement oldClassDefinition allInstVarNames equals: classAdded allInstVarNames.