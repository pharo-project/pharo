"
I am weak holder of an object that uses GTEventCollector. If the object disappears from object memory (or #liberate message is sent), GTEventCollector can be removed from a GTEventRecorder object.

For the Collaborators Part: GTEventCollector

Public API and Key Messages

- object: 
- liberate
- isLiberated

Internal Representation and Key Implementation Points.

    Instance Variables
	weakHolder:		<Object>

"
Class {
	#name : #GTEventRegistry,
	#superclass : #Object,
	#instVars : [
		'weakHolder'
	],
	#category : #'GT-EventRecorder-Support'
}

{ #category : #initialization }
GTEventRegistry >> initialize [
	super initialize.
	weakHolder := WeakArray new: 1.
]

{ #category : #testing }
GTEventRegistry >> isLiberated [
	^ (weakHolder at: 1) isNil
]

{ #category : #actions }
GTEventRegistry >> liberate [
	self object: nil.
]

{ #category : #accessing }
GTEventRegistry >> object: anObject [ 
	weakHolder at: 1 put: anObject.
]

{ #category : #printing }
GTEventRegistry >> printOn: aStream [
	super printOn: aStream.
	aStream
		nextPut: $(;
		nextPutAll: (self isLiberated ifTrue: [ 'liberated' ] ifFalse: [ 'used' ]);
		nextPut: $)
]
