tests
testSwitchSlotsAndMigrate

	aClass := self makeWithLayout: FixedLayout andSlots: { #a. #b }.
	aClass 
		compile: 'a ^a';
		compile: 'b ^b'.

	instance := aClass new.
	instance instVarNamed: #a put: $A.
	instance instVarNamed: #b put: $B.

	"now we switch the slots. The class definition should've changed"
	self makeWithLayout: FixedLayout andSlots: { #b. #a }.

	self assert: (instance instVarAt: 1) equals: $B.
	self assert: (instance instVarAt: 2) equals: $A.
	
	self assert: (instance instVarNamed: #a) equals: $A.
	self assert: (instance instVarNamed: #b) equals: $B.

	"it should have migrated methods"
	self assert: instance a equals: $A.
	self assert: instance b equals: $B.
	aClass classLayout checkIntegrity.