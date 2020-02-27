"
I'm a relation slot  that  describes  slots that contain collections.
"
Class {
	#name : #ToManyRelationSlot,
	#superclass : #RelationSlot,
	#category : #'Slot-Examples-Associations'
}

{ #category : #internal }
ToManyRelationSlot >> add: newObject to: collectionOwner [

	self checkValue: newObject.
	self hasInverse
		ifTrue: [ self addAssociationFrom: collectionOwner to: newObject ]

]

{ #category : #initialization }
ToManyRelationSlot >> initialize: anObject [
	self write: (RelationSet owner: anObject slot: self) to: anObject. 
	
]

{ #category : #internal }
ToManyRelationSlot >> remove: oldObject from: collectionOwner [

	self hasInverse
		ifTrue: [ self removeAssociationFrom: collectionOwner to: oldObject ]

]

{ #category : #'meta-object-protocol' }
ToManyRelationSlot >> wantsInitialization [
	^ true
]
