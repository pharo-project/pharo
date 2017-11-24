"
I am the  joystick axis event.

This event is generated when a joystick stick is moved.

The which parameter gives the joystick index.
The axis parameter is the index of the axis that moved.
The value parameter is how much the axis was moved.
The mapped parameter tells if the axis index has been mapped into the index of a standard layout.
The hasMapping parameters tells if this joystick is generating a mapped version of the event.
"
Class {
	#name : #OSJoyAxisEvent,
	#superclass : #OSEvent,
	#instVars : [
		'which',
		'axis',
		'value',
		'mapped',
		'hasMapping'
	],
	#category : #'OSWindow-Core-Events'
}

{ #category : #visitor }
OSJoyAxisEvent >> accept: aVisitor [
	^ aVisitor visitJoyAxisEvent: self
]

{ #category : #accessing }
OSJoyAxisEvent >> axis [
	
	^ axis
]

{ #category : #accessing }
OSJoyAxisEvent >> axis: anObject [
	
	axis := anObject
]

{ #category : #accessing }
OSJoyAxisEvent >> hasMapping [
	^ hasMapping
]

{ #category : #accessing }
OSJoyAxisEvent >> hasMapping: anObject [
	hasMapping := anObject
]

{ #category : #testing }
OSJoyAxisEvent >> isLeftX [
	^ axis == 0
]

{ #category : #testing }
OSJoyAxisEvent >> isLeftY [
	^ axis == 1
]

{ #category : #testing }
OSJoyAxisEvent >> isPreferredEvent [
	^ mapped or: hasMapping not
]

{ #category : #testing }
OSJoyAxisEvent >> isRightX [
	^ axis == 2
]

{ #category : #testing }
OSJoyAxisEvent >> isRightY [
	^ axis == 3
]

{ #category : #testing }
OSJoyAxisEvent >> isTriggerLeft [
	^ axis == 4
]

{ #category : #testing }
OSJoyAxisEvent >> isTriggerRight [
	^ axis == 5
]

{ #category : #accessing }
OSJoyAxisEvent >> mapped [
	^ mapped
]

{ #category : #accessing }
OSJoyAxisEvent >> mapped: anObject [
	mapped := anObject
]

{ #category : #accessing }
OSJoyAxisEvent >> value [
	^ value
]

{ #category : #accessing }
OSJoyAxisEvent >> value: anObject [
	
	value := anObject
]

{ #category : #accessing }
OSJoyAxisEvent >> which [
	
	^ which
]

{ #category : #accessing }
OSJoyAxisEvent >> which: anObject [
	
	which := anObject
]
