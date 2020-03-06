"
I am the detector of a finger which double tapped.
I send these event back to my handler: 
	- OSTouchOneFingerDoubleTapEvent
"
Class {
	#name : #OSTouchOneFingerDoubleTapDetector,
	#superclass : #OSTouchPunctualGestureDetector,
	#instVars : [
		'doubleTouchSpeed',
		'maxDistanceBetweenEachTouch',
		'position',
		'timestamp',
		'previousPosition',
		'previousTimestamp'
	],
	#classVars : [
		'DoubleTouchSpeed',
		'MaxDistance'
	],
	#category : #'OSWindow-Core-Gestures'
}

{ #category : #accessing }
OSTouchOneFingerDoubleTapDetector class >> doubleTouchSpeed [
	^ DoubleTouchSpeed
]

{ #category : #accessing }
OSTouchOneFingerDoubleTapDetector class >> doubleTouchSpeed: anObject [
	DoubleTouchSpeed := anObject
]

{ #category : #settings }
OSTouchOneFingerDoubleTapDetector class >> doubleTouchSpeedSettingsOn: aBuilder [
	<systemsettings>
	(aBuilder setting: #doubleTouchSpeed)
		target: self;
		label: 'Max time between tap (ms)';
		default: 700;
		description:
				'The time between two touch to send double touch (in milliseconds)';
		parent: #doubleTapGesture
]

{ #category : #'class initialization' }
OSTouchOneFingerDoubleTapDetector class >> initialize [
	super initialize.
	DoubleTouchSpeed := 700.
	MaxDistance := 0.05
]

{ #category : #accessing }
OSTouchOneFingerDoubleTapDetector class >> maxDistance [
	^ MaxDistance
]

{ #category : #accessing }
OSTouchOneFingerDoubleTapDetector class >> maxDistance: anObject [
	MaxDistance := anObject
]

{ #category : #settings }
OSTouchOneFingerDoubleTapDetector class >> maxDistanceSettingsOn: aBuilder [
	<systemsettings>
	(aBuilder setting: #maxDistance)
		target: self;
		label: 'Max distance between tap (%)';
		default: 0.05;
		description:
				'The distance tolerance to send double touch event (in percentage of the window)';
		parent: #doubleTapGesture
]

{ #category : #settings }
OSTouchOneFingerDoubleTapDetector class >> touchDoubleTapSettingsOn: aBuilder [
	<systemsettings>
	(aBuilder group: #doubleTapGesture)
		label: 'Double Tap';
		parent: #touchGestures
]

{ #category : #'gesture detection' }
OSTouchOneFingerDoubleTapDetector >> checkDoubleTouch [
	(timestamp - previousTimestamp < doubleTouchSpeed 
		and: [ (position distanceTo: previousPosition) < maxDistanceBetweenEachTouch ])
		ifFalse: [ ^ self ].
	self sendToEventHandler: self createEvent.
	self reinitialize
]

{ #category : #creation }
OSTouchOneFingerDoubleTapDetector >> createEvent [
	^ OSTouchOneFingerDoubleTapEvent new
		position: position;
		yourself
]

{ #category : #initialization }
OSTouchOneFingerDoubleTapDetector >> initialize [
	super initialize.
	doubleTouchSpeed := self class doubleTouchSpeed.
	maxDistanceBetweenEachTouch := self class maxDistance.
	self reinitialize.
]

{ #category : #operations }
OSTouchOneFingerDoubleTapDetector >> reinitialize [
	position := 0@0.
	timestamp := 0
]

{ #category : #visiting }
OSTouchOneFingerDoubleTapDetector >> visitTouchPointerDownEvent: anEvent [
	super visitTouchPointerDownEvent: anEvent.
	previousPosition := position.
	previousTimestamp := timestamp.
	position := anEvent position.
	timestamp := anEvent timestamp.
	self checkDoubleTouch.
]
