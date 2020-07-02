"
I'm an relation slot  that  describes the one or zero side of an association.
"
Class {
	#name : #ToOneRelationSlot,
	#superclass : #RelationSlot,
	#category : #'Slot-Examples-Associations'
}

{ #category : #testing }
ToOneRelationSlot >> isToOneSlot [

	^true
]

{ #category : #internal }
ToOneRelationSlot >> updateOld: oldValue new: newValue in: anObject [
	newValue 
		ifNotNil: [ self checkValue: newValue ].
	self hasInverse
		ifFalse: [ ^ newValue ].
	oldValue 
		ifNotNil: [ self removeAssociationFrom: anObject to: oldValue ].
	newValue 
		ifNotNil: [ self addAssociationFrom: anObject to: newValue ].
	^newValue
]

{ #category : #'meta-object-protocol' }
ToOneRelationSlot >> write: newValue to: anObject [
	self updateOld: (self read: anObject) new: newValue in: anObject.
	super write: newValue to: anObject
]

{ #category : #internal }
ToOneRelationSlot >> writeInverse: newValue to: anObject [
	^anObject instVarAt: index put: newValue
]
