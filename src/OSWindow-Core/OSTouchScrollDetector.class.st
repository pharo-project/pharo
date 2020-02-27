"
I am the abstract class for all the Scroll gesture detectors
"
Class {
	#name : #OSTouchScrollDetector,
	#superclass : #OSTouchOneDirectionGestureDetector,
	#instVars : [
		'directionAtBeginning',
		'persistentDirection'
	],
	#classVars : [
		'PersistentDirection'
	],
	#category : #'OSWindow-Core-Gestures'
}

{ #category : #'class initialization' }
OSTouchScrollDetector class >> initialize [
	super initialize.
	PersistentDirection := false
]

{ #category : #settings }
OSTouchScrollDetector class >> minDistanceBeforeDetectionSettingsOn: aBuilder [
	<systemsettings>
	(aBuilder setting: #minDistanceBeforeDetection)
		target: self;
		label: 'Minimal fingers distance crossed (%)';
		default: 0.05;		
		description:
				'The distance, in percentage of the screen, that both fingers needs to travel before event detection';
		parent: #scrollGesture
]

{ #category : #accessing }
OSTouchScrollDetector class >> persistentDirection [
	^ PersistentDirection
]

{ #category : #accessing }
OSTouchScrollDetector class >> persistentDirection: anObject [
	PersistentDirection := anObject
]

{ #category : #settings }
OSTouchScrollDetector class >> persistentDirectionSettingsOn: aBuilder [
	<systemsettings>
	(aBuilder setting: #persistentDirection)
		target: self;
		label: 'Persitent scrolling';
		default: false;
		description:
				'Should the scroll be keeping his direction after started?';
		parent: #scrollGesture
]

{ #category : #settings }
OSTouchScrollDetector class >> touchScrollSettingsOn: aBuilder [
	<systemsettings>
	(aBuilder group: #scrollGesture)
		label: 'Scroll';
		parent: #touchGestures
]

{ #category : #preconditions }
OSTouchScrollDetector >> beginConditions [
	^ super beginConditions
		and: [ self isKeepingDirection ]
]

{ #category : #factory }
OSTouchScrollDetector >> createEventBeginning [
	directionAtBeginning := direction.
	^ self eventClassBeginGesture new
		timestamp: eventHandler lastUpdateTimestamp;
		dDist: self dDist;
		direction: direction;
		deviceId: deviceId;
		position: self centerOfAllFingers;
		yourself
]

{ #category : #factory }
OSTouchScrollDetector >> createEventDuringMove [
	^ self eventClassMoveGesture new
		timestamp: eventHandler lastUpdateTimestamp;
		dDist: self dDist;
		direction: direction;
		deviceId: deviceId;
		position: self centerOfAllFingers;
		yourself
]

{ #category : #factory }
OSTouchScrollDetector >> createEventEnding [
	directionAtBeginning := 0@0.
	^ self eventClassEndGesture new
		timestamp: eventHandler lastUpdateTimestamp;
		dDist: self dDist;
		direction: direction;
		deviceId: deviceId;
		position: self centerOfAllFingers;
		yourself
]

{ #category : #abstract }
OSTouchScrollDetector >> eventClassBeginGesture [
	self subclassResponsibility
]

{ #category : #abstract }
OSTouchScrollDetector >> eventClassEndGesture [
	self subclassResponsibility
]

{ #category : #abstract }
OSTouchScrollDetector >> eventClassMoveGesture [
	self subclassResponsibility
]

{ #category : #initialization }
OSTouchScrollDetector >> initialize [
	super initialize.
	persistentDirection := self class persistentDirection.
	directionAtBeginning := 0@0.
]

{ #category : #preconditions }
OSTouchScrollDetector >> isKeepingDirection [
	^ persistentDirection not
		or: [	directionAtBeginning = (0 @ 0)
		or: [ direction abs = directionAtBeginning abs ] ]
]
