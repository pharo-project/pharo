tests
testShiftSlotAndMigrate

	aClass := self makeWithLayout: FixedLayout andSlots: { #a. #c }.
	aClass compile: 'a ^a'.
	aClass compile: 'c ^c'.
	
	instance := aClass new.
	instance instVarNamed: #a put: 1.
	instance instVarNamed: #c put: 3.

	"now we extend it with slots.  The class definition should've changed"
	self makeWithLayout: FixedLayout andSlots: { #a. #b. #c }.

	"it should have added an instance variable with nil in the middle"
	self assert: (instance instVarAt: 1) equals: 1.
	self assert: (instance instVarAt: 2) equals: nil.
	self assert: (instance instVarAt: 3) equals: 3.
	
	self assert: (instance instVarNamed: #a) equals: 1.
	self assert: (instance instVarNamed: #b) equals: nil.
	self assert: (instance instVarNamed: #c) equals: 3.
	
	"it should have migrated methods"
	self assert: instance a equals: 1.
	self assert: instance c equals: 3.
	aClass classLayout checkIntegrity.