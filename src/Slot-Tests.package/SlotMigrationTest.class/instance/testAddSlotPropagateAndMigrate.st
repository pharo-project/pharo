tests
testAddSlotPropagateAndMigrate
	"We create a class without slots and a subclass, and create an instance of the latter; then we add a slot to superclass and check the instance can hold a value."

	| superclass subclass |
	superclass := self makeWithLayout: FixedLayout.
	subclass := self make: [ :builder |
		builder 
			superclass: superclass;
			name: self anotherClassName.
		 ].
	
	superclass classLayout checkIntegrity.
	subclass classLayout checkIntegrity.
	
	instance := subclass new.
	
	"now we extend it with slots.  The class definition should've changed"
	superclass := self makeWithLayout: FixedLayout andSlots: { #aSlot }.
	
	"it should have an instance variable"
	instance instVarNamed: 'aSlot' put: 42.
	self assert: (instance instVarNamed: 'aSlot') equals: 42.
	superclass classLayout checkIntegrity.
	subclass classLayout checkIntegrity.