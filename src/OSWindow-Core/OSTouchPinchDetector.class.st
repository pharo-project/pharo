"
I am the abstract class for all the Pinch gesture detectors
"
Class {
	#name : #OSTouchPinchDetector,
	#superclass : #OSTouchDirectionnalGestureDetector,
	#instVars : [
		'maxDistanceBetweenFingersLine',
		'fingersDistanceFromCenter'
	],
	#classVars : [
		'MaxDistanceBetweenFingersLine'
	],
	#category : #'OSWindow-Core-Gestures'
}

{ #category : #'class initialization' }
OSTouchPinchDetector class >> initialize [
	super initialize.
	MaxDistanceBetweenFingersLine := 0.2.
	MinDistanceBeforeDetection := 0.03
]

{ #category : #accessing }
OSTouchPinchDetector class >> maxDistanceBetweenFingersLine [
	^ MaxDistanceBetweenFingersLine
]

{ #category : #accessing }
OSTouchPinchDetector class >> maxDistanceBetweenFingersLine: aNumber [
	MaxDistanceBetweenFingersLine := aNumber
]

{ #category : #settings }
OSTouchPinchDetector class >> maxDistanceBetweenFingersLineSettingsOn: aBuilder [
	<systemsettings>
	(aBuilder setting: #maxDistanceBetweenFingersLine)
		target: self;
		label: 'Max distance between fingers lines (%)';
		default: 0.2;
		description:
				'The maximum distance between the two lines formed by the fingers';
		parent: #pinchGesture
]

{ #category : #settings }
OSTouchPinchDetector class >> minDistanceBeforeDetectionSettingsOn: aBuilder [
	<systemsettings>
	(aBuilder setting: #minDistanceBeforeDetection)
		target: self;
		label: 'Minimal fingers distance crossed (%)';
		default: 0.05;
		description:
				'The distance, in percentage of the screen, that a finger of both fingers needs to travel before event detection';
		parent: #pinchGesture
]

{ #category : #settings }
OSTouchPinchDetector class >> touchPinchSettingsOn: aBuilder [
	<systemsettings>
	(aBuilder group: #pinchGesture)
		label: 'Pinch';
		parent: #touchGestures
]

{ #category : #preconditions }
OSTouchPinchDetector >> areFingersLinesNotTooFarApart [
	| firstFinger secondFinger minDistance |
	firstFinger := self activeFingers first.
	secondFinger := self activeFingers last.
	minDistance := (firstFinger distanceFromLineFormedByFinger: secondFinger)
		min: (secondFinger distanceFromLineFormedByFinger: firstFinger).
	^ minDistance < maxDistanceBetweenFingersLine
]

{ #category : #preconditions }
OSTouchPinchDetector >> beginConditions [
	^ super beginConditions
		and: [ self areFingersLinesNotTooFarApart
		and: [ self doesFingersGoesInOppositeDirection ]]
]

{ #category : #factory }
OSTouchPinchDetector >> createEventBeginning [
	fingersDistanceFromCenter := self fingersDistanceFromCenter.
	^ self eventClassBeginGesture new
		timestamp: eventHandler lastUpdateTimestamp;
		deviceId: deviceId;
		position: self centerOfAllFingers;
		dDist: self dDist;
		yourself
]

{ #category : #factory }
OSTouchPinchDetector >> createEventDuringMove [
	^ self eventClassMoveGesture new
		timestamp: eventHandler lastUpdateTimestamp;
		deviceId: deviceId;
		position: self centerOfAllFingers;
		dDist: self dDist;
		yourself
]

{ #category : #factory }
OSTouchPinchDetector >> createEventEnding [
	^ self eventClassEndGesture new
		timestamp: eventHandler lastUpdateTimestamp;
		deviceId: deviceId;
		position: self centerOfAllFingers;
		dDist: self dDist;
		yourself
]

{ #category : #'event field' }
OSTouchPinchDetector >> dDist [
	| oldDistance |
	oldDistance := fingersDistanceFromCenter.
	fingersDistanceFromCenter := self fingersDistanceFromCenter.
	^ fingersDistanceFromCenter - oldDistance
]

{ #category : #preconditions }
OSTouchPinchDetector >> doesFingersGoesInOppositeDirection [
	| fingersDirection direction |
	fingersDirection := self activeFingers collect: [ :each | each direction ].

	"We suppose that the direction of the gesture is the same as the first finger"
	direction := fingersDirection first.

	"We remove the first so he can't be evaluated a second time"
	fingersDirection at: 1 put: nil.
	^ fingersDirection anySatisfy: [ :finger | finger = direction negated ]
]

{ #category : #abstract }
OSTouchPinchDetector >> eventClassBeginGesture [
	self subclassResponsibility
]

{ #category : #abstract }
OSTouchPinchDetector >> eventClassEndGesture [
	self subclassResponsibility
]

{ #category : #abstract }
OSTouchPinchDetector >> eventClassMoveGesture [
	self subclassResponsibility
]

{ #category : #preconditions }
OSTouchPinchDetector >> fingersDistanceFromCenter [
	| centerOfAllFingers |
	centerOfAllFingers := self centerOfAllFingers.
	^ (self activeFingers collect: [ :each | centerOfAllFingers distanceTo: each lastPosition ]) sum
]

{ #category : #initialization }
OSTouchPinchDetector >> initialize [
	super initialize.
	maxDistanceBetweenFingersLine := self class maxDistanceBetweenFingersLine.
]
