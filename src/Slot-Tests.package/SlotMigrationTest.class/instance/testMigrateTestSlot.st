tests
testMigrateTestSlot
	"We create a class with a Test slot, so its  value is shared between instances"
	| instance2 |
	aClass := self makeWithLayout: FixedLayout andSlots: { #aSlot => ExampleSlotWithState. }.
	
	instance := aClass new.
	instance2 := aClass new.
	(aClass slotNamed: #aSlot) write: 125 to: instance2.
	"now we change the TestSlot with a InstanceVariableSlot. Both instances should both shared the same value in this slot."
	self makeWithLayout: FixedLayout andSlots: { #aSlot }.
	
	self assert: ((aClass slotNamed: #aSlot) read: instance) equals: 125.
	self assert: ((aClass slotNamed: #aSlot) read: instance2) equals: 125.
	aClass classLayout checkIntegrity.