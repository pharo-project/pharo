tests
testSlotTypeChanged
	| old new changesInSlots slotChange |
	old := self shFTestClassWithDifferentSlotsOld.
	new := self shFTestClassWithDifferentSlotsNew.
	results := old compareTo: new.
	
	changesInSlots := results select: [ :e | ShFSlotChange allSubclasses includes: e class ].
	
	self assert: changesInSlots size equals:1.

	slotChange := changesInSlots at:1.

	self assert: slotChange class equals: ShFSlotTypeChanged .
	self assert: slotChange slotName equals: #oneSlot.
	self assert: slotChange oldSlotType equals: #InstanceVariableSlot.
	self assert: slotChange newSlotType equals: #PropertySlot.
	self assert: slotChange slotSide equals: #instance.
