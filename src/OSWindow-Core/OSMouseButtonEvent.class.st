"
i am the root of various mouse button events.
"
Class {
	#name : #OSMouseButtonEvent,
	#superclass : #OSMouseEvent,
	#instVars : [
		'button'
	],
	#category : 'OSWindow-Core-Events'
}

{ #category : #accessing }
OSMouseButtonEvent >> button [
	
	^ button
]

{ #category : #accessing }
OSMouseButtonEvent >> button: anObject [
	
	button := anObject
]

{ #category : #testing }
OSMouseButtonEvent >> isWheel [
	^ self button > 3
]

{ #category : #printing }
OSMouseButtonEvent >> printOn: stream [
	super printOn: stream.
	stream space; nextPutAll: button asString
]

{ #category : #accessing }
OSMouseButtonEvent >> wheelDirection [
	"If button is wheel button (>3), answer direction (#up/#down)"
	^ self button = 4 
		ifTrue: [ #up ] 
		ifFalse: [ #down ]
]
