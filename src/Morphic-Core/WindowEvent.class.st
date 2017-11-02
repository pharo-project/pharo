"
I'm an event related to the host window, only dispatched to the World. 
"
Class {
	#name : #WindowEvent,
	#superclass : #MorphicEvent,
	#instVars : [
		'action',
		'rectangle'
	],
	#category : 'Morphic-Core-Events'
}

{ #category : #accessing }
WindowEvent >> action [
	^action
]

{ #category : #accessing }
WindowEvent >> action: aValue [
	action := aValue.
]

{ #category : #testing }
WindowEvent >> isWindowEvent [
	^true
]

{ #category : #printing }
WindowEvent >> printOn: aStream [

	aStream nextPut: $[.
	aStream nextPutAll: self type; space.
	aStream nextPut: $(; print: self rectangle; nextPut: $).
	aStream nextPut: $].
]

{ #category : #accessing }
WindowEvent >> rectangle [
	^rectangle
]

{ #category : #accessing }
WindowEvent >> rectangle: aValue [
	rectangle := aValue.
]

{ #category : #dispatching }
WindowEvent >> sentTo:anObject [
	"Dispatch the receiver into anObject"
	^anObject handleWindowEvent: self.
]

{ #category : #accessing }
WindowEvent >> type [
	"This should match the definitions in sq.h"
	^#(
		windowMetricChange
		windowClose
		windowIconise
		windowActivated
		windowPaint
	) at: action ifAbsent: [#windowEventUnknown]
]
