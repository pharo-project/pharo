tests
testRemoveSlotAndMigrate
	"We create a class with a slot and create an instance; then we remove the slot and check the instance doesn't hold the value."
	aClass := self makeWithLayout: FixedLayout andSlots: { #aSlot }.

	instance := aClass new.
	"it should have an instance variable"
	instance instVarAt: 1 put: 125.
	
	"now we extend it with slots.  The class definition should've changed"
	self makeWithLayout: FixedLayout.
	
	self should: [ instance instVarAt: 1 ] raise: Error.
	aClass classLayout checkIntegrity.