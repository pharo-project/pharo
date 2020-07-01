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
	newValue notNil
		ifTrue: [ self checkValue: newValue ].
	self hasInverse
		ifFalse: [ ^ self ].
	oldValue notNil
		ifTrue: [ self removeAssociationFrom: anObject to: oldValue ].
	newValue notNil
		ifTrue: [ self addAssociationFrom: anObject to: newValue ].
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
