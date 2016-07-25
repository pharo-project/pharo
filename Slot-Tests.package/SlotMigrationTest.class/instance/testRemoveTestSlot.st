tests
testRemoveTestSlot
	aClass := self makeWithLayout: FixedLayout andSlots: {#anotherSlot => ExampleSlotWithState. #aSlot }.
	
	instance := aClass new.
	instance instVarAt: 1 put: 125.
	"now we remove the TestSlot from the class definition. As testSlot has a size of 0, the object should not change."
	self makeWithLayout: FixedLayout andSlots: {#aSlot. }.
	

	self assert: (instance instVarAt: 1) equals: 125.
	aClass classLayout checkIntegrity.