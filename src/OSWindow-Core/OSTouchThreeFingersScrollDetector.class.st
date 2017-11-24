"
I am detecting the scrolls made with three fingers.
I send these event back to my handler: 
	- OSTouchThreeFingersScrollBeginEvent 
	- OSTouchThreeFingersScrollMoveEvent 
	- OSTouchThreeFingersScrollEndEvent 
"
Class {
	#name : #OSTouchThreeFingersScrollDetector,
	#superclass : #OSTouchScrollDetector,
	#category : #'OSWindow-Core-Gestures'
}

{ #category : #abstract }
OSTouchThreeFingersScrollDetector >> eventClassBeginGesture [
	^ OSTouchThreeFingersScrollBeginEvent
]

{ #category : #abstract }
OSTouchThreeFingersScrollDetector >> eventClassEndGesture [
	^ OSTouchThreeFingersScrollEndEvent
]

{ #category : #abstract }
OSTouchThreeFingersScrollDetector >> eventClassMoveGesture [
	^ OSTouchThreeFingersScrollMoveEvent
]

{ #category : #preconditions }
OSTouchThreeFingersScrollDetector >> necessaryFingers [
	^ 3
]
