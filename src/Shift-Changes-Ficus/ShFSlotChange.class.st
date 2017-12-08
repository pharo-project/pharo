Class {
	#name : #ShFSlotChange,
	#superclass : #ShFAbstractChange,
	#instVars : [
		'slotSide'
	],
	#category : 'Shift-Changes-Ficus'
}

{ #category : #'checking change' }
ShFSlotChange >> addChangesFrom: oldDefinition to: newDefinition into: results [
	self
		detectSlotChangeFrom: oldDefinition layout instanceVariables
		to: newDefinition layout instanceVariables
		into: results
		side: #instance.

	self
		detectSlotChangeFrom: oldDefinition metaclass layout instanceVariables
		to: newDefinition metaclass layout instanceVariables
		into: results
		side: #metaclass.


	results
		select: [ :e | ShSlotChange subclasses includes: e class ]
		thenDo: [ :e | 
			e
				oldClass: oldDefinition;
				newClass: newDefinition ]
]

{ #category : #'checking change' }
ShFSlotChange >> detectAddedSlotsOnOld: oldSlots new: newSlots into: results side: side [
	newSlots
		do: [ :new | 
			oldSlots
				detect: [ :old | old name = new name ]
				ifNone: [ results
						add:
							(ShFSlotAdded new
								slotName: new name;
								slotType: #InstanceVariableSlot;
								slotSide: side;
								yourself) ] ]
]

{ #category : #'checking change' }
ShFSlotChange >> detectRemovedSlotsOnOld: oldSlots new: results into: newSlots side: side [
	oldSlots
		do: [ :old | 
			newSlots
				detect: [ :new | old name = new name ]
				ifNone: [ results
						add:
							(ShFSlotRemoved new
								slotName: old name;
								slotType: #InstanceVariableSlot;
								slotSide: side;
								yourself) ] ]
]

{ #category : #'checking change' }
ShFSlotChange >> detectSlotChangeFrom:oldSlots to:newSlots into:results side: side. [
	self detectAddedSlotsOnOld: oldSlots new: newSlots into: results side:side.
	self detectRemovedSlotsOnOld: oldSlots new: results into: newSlots side:side.
	self detectSlotTypeChangeOnOld: oldSlots new: newSlots into: results side:side
]

{ #category : #'checking change' }
ShFSlotChange >> detectSlotTypeChangeOnOld: oldSlots new: newSlots into: results side: side [
	newSlots
		do: [ :new | 
			oldSlots
				detect: [ :old | old name = new name and: [ old slotType ~= new slotType ] ]
				ifFound: [ :old | 
					results
						add:
							(ShFSlotTypeChanged new
								slotName: new name;
								oldSlotType: old slotType;
								slotSide: side;
								newSlotType: new slotType) ] ]
]

{ #category : #accessing }
ShFSlotChange >> slotSide [
	^ slotSide
]

{ #category : #accessing }
ShFSlotChange >> slotSide: anObject [
	slotSide := anObject
]
