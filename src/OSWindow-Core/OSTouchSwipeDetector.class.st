"
I am the abstract class for all the Swipe gesture detectors
"
Class {
	#name : #OSTouchSwipeDetector,
	#superclass : #OSTouchOneDirectionGestureDetector,
	#instVars : [
		'minDistToBorder'
	],
	#classVars : [
		'MinDistToBorder'
	],
	#category : #'OSWindow-Core-Gestures'
}

{ #category : #'class initialization' }
OSTouchSwipeDetector class >> initialize [
	super initialize.
	MinDistToBorder := 0.1 
]

{ #category : #accessing }
OSTouchSwipeDetector class >> minDistToBorder [
	^ MinDistToBorder
]

{ #category : #accessing }
OSTouchSwipeDetector class >> minDistToBorder: aFloat [
	MinDistToBorder := aFloat .
]

{ #category : #settings }
OSTouchSwipeDetector class >> minDistToBorderSettingsOn: aBuilder [
	<systemsettings>
	(aBuilder setting: #minDistToBorder)
		target: self;
		default: 0.1;
		label: 'Minimal distance of the fingers to a border (%)';
		description:
				'The distance, in percentage of the screen, that fingers needs to be close to a border to trigger a swipe';
		parent: #swipeGesture
]

{ #category : #settings }
OSTouchSwipeDetector class >> minDistanceBeforeDetectionSettingsOn: aBuilder [
	<systemsettings>
	(aBuilder setting: #minDistanceBeforeDetection)
		target: self;
		label: 'Minimal fingers distance crossed (%)';
		default: 0.05;		
		description:
				'The distance, in percentage of the screen, that both fingers needs to travel before event detection';
		parent: #swipeGesture
]

{ #category : #settings }
OSTouchSwipeDetector class >> touchSwipeSettingsOn: aBuilder [
	<systemsettings>
	(aBuilder group: #swipeGesture)
		label: 'Swipe';
		parent: #touchGestures
]

{ #category : #preconditions }
OSTouchSwipeDetector >> areFingersCloseToABorder [
	"0.5 represents half the screen in percentage"
	| halfScreen fingersCenterTranslatedToScreenCenter |
	halfScreen := 0.5.
	
	fingersCenterTranslatedToScreenCenter := (self centerOfAllFingers - halfScreen) abs.
	
	^ modeActivated or: [ fingersCenterTranslatedToScreenCenter x > (halfScreen - minDistToBorder)
		or: [ fingersCenterTranslatedToScreenCenter y > (halfScreen - minDistToBorder)  ]]
]

{ #category : #preconditions }
OSTouchSwipeDetector >> beginConditions [
	^ super beginConditions and: [ self areFingersCloseToABorder ]
]

{ #category : #factory }
OSTouchSwipeDetector >> createEventBeginning [
	^ self eventClassBeginGesture new
		timestamp: eventHandler lastUpdateTimestamp;
		direction: direction;
		dDist: self dDist;
		deviceId: deviceId;
		position: self centerOfAllFingers;
		yourself
]

{ #category : #factory }
OSTouchSwipeDetector >> createEventDuringMove [
	^ self eventClassMoveGesture new
		timestamp: eventHandler lastUpdateTimestamp;
		direction: direction;
		dDist: self dDist;
		deviceId: deviceId;
		position: self centerOfAllFingers;
		yourself
]

{ #category : #factory }
OSTouchSwipeDetector >> createEventEnding [
	| endClass |
	endClass := self areFingersCloseToABorder ifTrue: [ self eventClassEndGesture ] ifFalse: [ self eventClassAbortGesture ].
	^ endClass new
		timestamp: eventHandler lastUpdateTimestamp;
		direction: direction;
		deviceId: deviceId;
		dDist: self dDist;
		position: self centerOfAllFingers;
		yourself
]

{ #category : #abstract }
OSTouchSwipeDetector >> eventClassAbortGesture [
	self subclassResponsibility 
]

{ #category : #abstract }
OSTouchSwipeDetector >> eventClassBeginGesture [
	self subclassResponsibility
]

{ #category : #abstract }
OSTouchSwipeDetector >> eventClassEndGesture [
	self subclassResponsibility
]

{ #category : #abstract }
OSTouchSwipeDetector >> eventClassMoveGesture [
	self subclassResponsibility
]

{ #category : #initialization }
OSTouchSwipeDetector >> initialize [
	super initialize.
	minDistToBorder := self class minDistToBorder
]
