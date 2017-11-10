"
I collect and provide the information to migrate instances when a class changed.
"
Class {
	#name : #InstanceModification,
	#superclass : #AbstractModification,
	#category : #Slot-ClassBuilder
}

{ #category : #installing }
InstanceModification >> installAddedSlot: addedSlot [
	modificationMap at: addedSlot newFieldIndex put: addedSlot
]

{ #category : #installing }
InstanceModification >> installModifiedSlot: modifiedSlot [
	modificationMap at: modifiedSlot newFieldIndex put: modifiedSlot
]

{ #category : #installing }
InstanceModification >> installRemovedSlot: removedSlot [
	" ignore "
]
