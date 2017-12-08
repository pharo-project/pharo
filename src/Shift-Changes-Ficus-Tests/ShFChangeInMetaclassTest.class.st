Class {
	#name : #ShFChangeInMetaclassTest,
	#superclass : #AbstractShiftChangesFicusTest,
	#category : 'Shift-Changes-Ficus-Tests'
}

{ #category : #tests }
ShFChangeInMetaclassTest >> testClassSideSlotAdded [
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
]

{ #category : #tests }
ShFChangeInMetaclassTest >> testClassSideSlotRemoved [
	| old new changesInSlots slotChange |
	old := self shFTestClassAWithClassVariable.
	new := self shFTestClassA.
	results := old compareTo: new.

	changesInSlots := results select: [ :e | ShFSlotChange allSubclasses includes: e class ].
	
	self assert: changesInSlots size equals:1.

	slotChange := changesInSlots at:1.

	self assert: slotChange class equals: ShFSlotRemoved.
	self assert: slotChange slotName equals: #aClassSideVariable.
	self assert: slotChange slotType equals: #InstanceVariableSlot.
	self assert: slotChange slotSide equals: #metaclass
]
