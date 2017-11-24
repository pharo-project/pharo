"
I am detecting the rotations made with two fingers.
I send these event back to my handler: 
	- OSTouchTwoFingersRotationBeginEvent 
	- OSTouchTwoFingersRotationMoveEvent 
	- OSTouchTwoFingersRotationEndEvent 
"
Class {
	#name : #OSTouchTwoFingersRotationDetector,
	#superclass : #OSTouchRotationDetector,
	#category : #'OSWindow-Core-Gestures'
}

{ #category : #abstract }
OSTouchTwoFingersRotationDetector >> eventClassBeginGesture [
	^ OSTouchTwoFingersRotationBeginEvent
]

{ #category : #abstract }
OSTouchTwoFingersRotationDetector >> eventClassEndGesture [
	^ OSTouchTwoFingersRotationEndEvent
]

{ #category : #abstract }
OSTouchTwoFingersRotationDetector >> eventClassMoveGesture [
	^ OSTouchTwoFingersRotationMoveEvent
]

{ #category : #preconditions }
OSTouchTwoFingersRotationDetector >> necessaryFingers [
	^ 2
]
