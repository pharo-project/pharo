tests
testClassSideSlotAdded
	| old new changesInSlots slotChange |
	old := self shFTestClassA.
	new := self shFTestClassAWithClassVariable.
	results := old compareTo: new.

	changesInSlots := results select: [ :e | ShFSlotChange allSubclasses includes: e class ].
	
	self assert: changesInSlots size equals:1.

	slotChange := changesInSlots at:1.

	self assert: slotChange class equals: ShFSlotAdded.
	self assert: slotChange slotName equals: #aClassSideVariable.
	self assert: slotChange slotType equals: #InstanceVariableSlot.
	self assert: slotChange slotSide equals: #metaclass