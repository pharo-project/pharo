"
A slot on which one can write only once.
"
Class {
	#name : #WriteOnceSlot,
	#superclass : #IndexedSlot,
	#category : #'Slot-Examples-base'
}

{ #category : #initialization }
WriteOnceSlot >> initialize: anObject [
	super write: nil -> true to: anObject
]

{ #category : #testing }
WriteOnceSlot >> isWritten: anObject [
	^ (super read: anObject) value
]

{ #category : #'meta-object-protocol' }
WriteOnceSlot >> read: anObject [
	^ (super read: anObject) key
]

{ #category : #initialization }
WriteOnceSlot >> wantsInitalization [

	^ true
]

{ #category : #'meta-object-protocol' }
WriteOnceSlot >> write: aValue to: anObject [
	| asso |
	asso := super read: anObject.
	(self isWritten: anObject)
		ifTrue: [ asso key: aValue value: false ]
		ifFalse: [ self error: 'only one assigment allowed' ]
]
