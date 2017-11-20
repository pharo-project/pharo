"
I am detecting the scrolls made with two fingers.
I send these event back to my handler: 
	- OSTouchTwoFingersScrollBeginEvent 
	- OSTouchTwoFingersScrollMoveEvent 
	- OSTouchTwoFingersScrollEndEvent 
"
Class {
	#name : #OSTouchTwoFingersScrollDetector,
	#superclass : #OSTouchScrollDetector,
	#category : #OSWindow-Core-Gestures
}

{ #category : #abstract }
OSTouchTwoFingersScrollDetector >> eventClassBeginGesture [
	^ OSTouchTwoFingersScrollBeginEvent
]

{ #category : #abstract }
OSTouchTwoFingersScrollDetector >> eventClassEndGesture [
	^ OSTouchTwoFingersScrollEndEvent
]

{ #category : #abstract }
OSTouchTwoFingersScrollDetector >> eventClassMoveGesture [
	^ OSTouchTwoFingersScrollMoveEvent
]

{ #category : #preconditions }
OSTouchTwoFingersScrollDetector >> necessaryFingers [
	^ 2
]
