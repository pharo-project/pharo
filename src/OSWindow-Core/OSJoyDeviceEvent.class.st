"
I am a joystick device event.

The which parameter gives the joystick index.
The  mapped  parameters tells if this device is axis and buttons are going to be mapped into a standard layout.
The hasMapping parameters tells if this joystick is generating a mapped version of the event.
"
Class {
	#name : #OSJoyDeviceEvent,
	#superclass : #OSEvent,
	#instVars : [
		'which',
		'mapped',
		'hasMapping'
	],
	#category : #OSWindow-Core-Events
}

{ #category : #accessing }
OSJoyDeviceEvent >> hasMapping [
	^ hasMapping
]

{ #category : #accessing }
OSJoyDeviceEvent >> hasMapping: anObject [
	hasMapping := anObject
]

{ #category : #testing }
OSJoyDeviceEvent >> isPreferredEvent [
	^ mapped or: hasMapping not
]

{ #category : #accessing }
OSJoyDeviceEvent >> mapped [
	^ mapped
]

{ #category : #accessing }
OSJoyDeviceEvent >> mapped: anObject [
	mapped := anObject
]

{ #category : #accessing }
OSJoyDeviceEvent >> which [
	^ which
]

{ #category : #accessing }
OSJoyDeviceEvent >> which: anObject [
	which := anObject
]
