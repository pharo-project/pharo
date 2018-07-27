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

{ #category : #'meta-object-protocol' }
ToOneRelationSlot >> write: newValue to: anObject [

	| oldValue |

	oldValue := self read: anObject.
	newValue notNil
		ifTrue: [ self checkValue: newValue ].
	self hasInverse 
		ifTrue: [ 
			oldValue notNil
				ifTrue: [ self removeAssociationFrom: anObject to: oldValue ].
			newValue notNil
				ifTrue: [ self addAssociationFrom: anObject to: newValue ] ].
		
	super write: newValue to: anObject
]

{ #category : #internal }
ToOneRelationSlot >> writeInverse: newValue to: anObject [
	^anObject instVarAt: index put: newValue
]
