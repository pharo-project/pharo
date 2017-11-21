"
I am detecting the pinches made with two fingers.
I send these event back to my handler: 
	- OSTouchTwoFingersPinchBeginEvent 
	- OSTouchTwoFingersPinchMoveEvent 
	- OSTouchTwoFingersPinchEndEvent 
"
Class {
	#name : #OSTouchTwoFingersPinchDetector,
	#superclass : #OSTouchPinchDetector,
	#category : #OSWindow-Core-Gestures
}

{ #category : #abstract }
OSTouchTwoFingersPinchDetector >> eventClassBeginGesture [
	^OSTouchTwoFingersPinchBeginEvent
]

{ #category : #abstract }
OSTouchTwoFingersPinchDetector >> eventClassEndGesture [
	^ OSTouchTwoFingersPinchEndEvent
]

{ #category : #abstract }
OSTouchTwoFingersPinchDetector >> eventClassMoveGesture [
	^ OSTouchTwoFingersPinchMoveEvent
]

{ #category : #preconditions }
OSTouchTwoFingersPinchDetector >> necessaryFingers [
	^ 2
]
