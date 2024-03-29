"
A slot on which one can write only once.
"
Class {
	#name : 'WriteOnceSlot',
	#superclass : 'IndexedSlot',
	#category : 'Slot-Examples-Base',
	#package : 'Slot-Examples',
	#tag : 'Base'
}

{ #category : 'initialization' }
WriteOnceSlot >> initialize: anObject [
	super write: nil -> true to: anObject
]

{ #category : 'testing' }
WriteOnceSlot >> isWritten: anObject [
	^ (super read: anObject) value
]

{ #category : 'meta-object-protocol' }
WriteOnceSlot >> read: anObject [
	<reflection: 'Object Inspection - State inspection'>
	^ (super read: anObject) key
]

{ #category : 'initialization' }
WriteOnceSlot >> wantsInitialization [

	^ true
]

{ #category : 'meta-object-protocol' }
WriteOnceSlot >> write: aValue to: anObject [
	<reflection: 'Object Modification - State modification'>
	^(self isWritten: anObject)
		ifTrue: [ (super read: anObject) key: aValue value: false. aValue ]
		ifFalse: [ self error: 'only one assignment allowed' ]
]
