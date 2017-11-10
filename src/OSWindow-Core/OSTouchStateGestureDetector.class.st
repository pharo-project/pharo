"
I am a gesture detector who detects gestures with a begin, current and end state
"
Class {
	#name : #OSTouchStateGestureDetector,
	#superclass : #OSTouchGestureDetector,
	#instVars : [
		'modeActivated'
	],
	#category : #OSWindow-Core-Gestures
}

{ #category : #preconditions }
OSTouchStateGestureDetector >> beginConditions [
	^ self hasGoodNumberOfFingers
]

{ #category : #factory }
OSTouchStateGestureDetector >> createEventBeginning [
	self subclassResponsibility
]

{ #category : #factory }
OSTouchStateGestureDetector >> createEventDuringMove [
	self subclassResponsibility 
]

{ #category : #factory }
OSTouchStateGestureDetector >> createEventEnding [
	self subclassResponsibility 
]

{ #category : #preconditions }
OSTouchStateGestureDetector >> hasGoodNumberOfFingers [ 
	^ eventHandler fingerCount = self necessaryFingers
]

{ #category : #initialization }
OSTouchStateGestureDetector >> initialize [
	super initialize.
	modeActivated := false.
]

{ #category : #preconditions }
OSTouchStateGestureDetector >> necessaryFingers [
	self subclassResponsibility
]

{ #category : #preconditions }
OSTouchStateGestureDetector >> stayActiveConditions [
	^ self hasGoodNumberOfFingers
]

{ #category : #'changing state' }
OSTouchStateGestureDetector >> tryToEndGesture [
	self stayActiveConditions
		ifTrue: [ ^ self ].
	modeActivated := false.
	self sendToEventHandler: self createEventEnding
]

{ #category : #'changing state' }
OSTouchStateGestureDetector >> tryToGoThroughNextState [
	modeActivated
		ifTrue: [ self tryToEndGesture ]
		ifFalse: [ self tryToStartGesture ]
]

{ #category : #'changing state' }
OSTouchStateGestureDetector >> tryToStartGesture [
	self beginConditions ifFalse: [ ^self ].
	modeActivated := true.
	self sendToEventHandler: self createEventBeginning
]

{ #category : #visiting }
OSTouchStateGestureDetector >> visitTouchPointerDownEvent: anEvent [
	super visitTouchPointerDownEvent: anEvent.
	self tryToGoThroughNextState
]

{ #category : #visiting }
OSTouchStateGestureDetector >> visitTouchPointerMoveEvent: anEvent [
	super visitTouchPointerMoveEvent: anEvent.
	self tryToGoThroughNextState.
	(modeActivated and: [ self beginConditions ])
		ifTrue: [ self sendToEventHandler: self createEventDuringMove ]
]

{ #category : #visiting }
OSTouchStateGestureDetector >> visitTouchPointerUpEvent: anEvent [
	super visitTouchPointerUpEvent: anEvent.
	self tryToGoThroughNextState
]
