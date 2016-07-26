tests
testAddTestSlot
	"We create a class without slots and create an instance; then we add a slot and check the instance can hold a value."
	aClass := self makeWithLayout: FixedLayout andSlots: { #aSlot }.
	
	instance := aClass new.
	instance instVarAt: 1 put: 125.
	"now we extend it with a test slots.  The class definition should've changed"
	self makeWithLayout: FixedLayout andSlots: {#anotherSlot => ExampleSlotWithState. #aSlot. }.
	
	"It should not have move the first instance as TestSlot size = 0"
	self assert: (instance instVarAt: 1) equals: 125.
	aClass classLayout checkIntegrity.