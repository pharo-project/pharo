"
I collect and provide the information to update methods when a class changed.
"
Class {
	#name : #MethodModification,
	#superclass : #AbstractModification,
	#instVars : [
		'addedSlots'
	],
	#category : #'Slot-ClassBuilder'
}

{ #category : #accessing }
MethodModification >> addedSlotNamed: name ifAbsent: aBlock [
	^ addedSlots at: name ifAbsent: aBlock
]

{ #category : #initialization }
MethodModification >> initialize [
	addedSlots := Dictionary new
]

{ #category : #installing }
MethodModification >> installAddedSlot: addedSlot [
	addedSlots at: addedSlot name put: addedSlot
]

{ #category : #installing }
MethodModification >> installModifiedSlot: modifiedSlot [
	modificationMap at: modifiedSlot oldFieldIndex put: modifiedSlot
]

{ #category : #installing }
MethodModification >> installRemovedSlot: removedSlot [
	modificationMap at: removedSlot oldFieldIndex put: removedSlot
]
