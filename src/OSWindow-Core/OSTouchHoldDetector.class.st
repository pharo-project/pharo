"
I am the abstract class for all the Hold gesture detectors
"
Class {
	#name : #OSTouchHoldDetector,
	#superclass : #OSTouchPunctualGestureDetector,
	#instVars : [
		'waitingTime',
		'maxDistanceTravelled',
		'process'
	],
	#classVars : [
		'MaxDistanceTravelled',
		'WaitingTime'
	],
	#category : #'OSWindow-Core-Gestures'
}

{ #category : #'class initialization' }
OSTouchHoldDetector class >> initialize [
	super initialize.
	MaxDistanceTravelled := 0.02.
	WaitingTime := 1000
]

{ #category : #accessing }
OSTouchHoldDetector class >> maxDistanceTravelled [
	^ MaxDistanceTravelled
]

{ #category : #accessing }
OSTouchHoldDetector class >> maxDistanceTravelled: anObject [
	MaxDistanceTravelled := anObject
]

{ #category : #settings }
OSTouchHoldDetector class >> maxDistanceTravelledSettingsOn: aBuilder [
	<systemsettings>
	(aBuilder setting: #maxDistanceTravelled)
		target: self;
		label: 'Detection area (%)';
		default: 0.02;
		description:
				'The area to consider the finger to be still';
		parent: #holdGesture
]

{ #category : #settings }
OSTouchHoldDetector class >> touchHoldSettingsOn: aBuilder [
	<systemsettings>
	(aBuilder group: #holdGesture)
		label: 'Hold';
		parent: #touchGestures
]

{ #category : #accessing }
OSTouchHoldDetector class >> waitingTime [
	^ WaitingTime
]

{ #category : #accessing }
OSTouchHoldDetector class >> waitingTime: anObject [
	WaitingTime := anObject
]

{ #category : #settings }
OSTouchHoldDetector class >> waitingTimeSettingsOn: aBuilder [
	<systemsettings>
	(aBuilder setting: #waitingTime)
		target: self;
		label: 'Time to hold (ms)';
		default: 1000;
		description:
				'How long does a finger has to stay on the screen to be considered as holding';
		parent: #holdGesture
]

{ #category : #creation }
OSTouchHoldDetector >> createHoldEvent [
	self subclassResponsibility
]

{ #category : #creation }
OSTouchHoldDetector >> createWaitingProcess [
	^ [ 
	(Delay forMilliseconds: waitingTime) wait.
	(self activeFingers allSatisfy: [ :each | each totalDistanceTravelled < maxDistanceTravelled ])
		ifTrue: [ self sendToEventHandler: self createHoldEvent ] ] fork
]

{ #category : #initialization }
OSTouchHoldDetector >> initialize [
	super initialize.
	waitingTime := self class waitingTime.
	maxDistanceTravelled := self class maxDistanceTravelled.
	process := Process new terminate.
]

{ #category : #preconditions }
OSTouchHoldDetector >> necessaryFingers [
	self subclassResponsibility
]

{ #category : #visiting }
OSTouchHoldDetector >> visitTouchPointerDownEvent: anEvent [
	super visitTouchPointerDownEvent: anEvent.
	eventHandler fingerCount = self necessaryFingers
		ifTrue: [  process := self createWaitingProcess ]
		ifFalse: [ process terminate ]
]

{ #category : #visiting }
OSTouchHoldDetector >> visitTouchPointerUpEvent: anEvent [
	super visitTouchPointerUpEvent: anEvent.
	process terminate
]
