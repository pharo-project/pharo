"
I am a variable node for representing the slots of an object. I used the name of a slot as a key. I extract the value from the host object every time #rawValue is called.
"
Class {
	#name : #StInspectorSlotNode,
	#superclass : #StInspectorNode,
	#instVars : [
		'slot'
	],
	#category : #'NewTools-Inspector-Model'
}

{ #category : #'instance creation' }
StInspectorSlotNode class >> hostObject: anObject slot: aSlot [
	^ (self hostObject: anObject)
		slot: aSlot;
		yourself
]

{ #category : #comparing }
StInspectorSlotNode >> = anObject [

	^ super = anObject and: [ slot = anObject slot ]
]

{ #category : #accessing }
StInspectorSlotNode >> errorMessage [
	^ 'error obtaining attribute value'
]

{ #category : #comparing }
StInspectorSlotNode >> hash [

	^ super hash bitXor: slot hash
]

{ #category : #accessing }
StInspectorSlotNode >> key [
	^ self slot name
]

{ #category : #printing }
StInspectorSlotNode >> printOn: stream [ 

	super printOn: stream.
	stream << '(' << self slot name << ')'
]

{ #category : #accessing }
StInspectorSlotNode >> rawValue [

	^  slot read: self hostObject
]

{ #category : #actions }
StInspectorSlotNode >> save: anObject [

	self hostObject 
		writeSlot: self slot 
		value: anObject
]

{ #category : #accessing }
StInspectorSlotNode >> slot [
	^ slot
]

{ #category : #accessing }
StInspectorSlotNode >> slot: aSlot [

	self
		assert: [ aSlot isNotNil ]
		description: [ 'Slot can not be nil' ].

	slot := aSlot.
]

{ #category : #accessing }
StInspectorSlotNode >> variableTag [
	^ 'attribute'
]
