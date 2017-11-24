"
I am detecting the scrolls made with two fingers.
I send these event back to my handler: 
	- OSTouchThreeFingersSwipeBeginEvent 
	- OSTouchThreeFingersSwipeMoveEvent 
	- OSTouchThreeFingersSwipeEndEvent 
	- OSTouchThreeFingersSwipeAbortEvent 
"
Class {
	#name : #OSTouchThreeFingersSwipeDetector,
	#superclass : #OSTouchSwipeDetector,
	#category : #'OSWindow-Core-Gestures'
}

{ #category : #abstract }
OSTouchThreeFingersSwipeDetector >> eventClassAbortGesture [
	^ OSTouchThreeFingersSwipeAbortEvent
]

{ #category : #abstract }
OSTouchThreeFingersSwipeDetector >> eventClassBeginGesture [
	^ OSTouchThreeFingersSwipeBeginEvent
]

{ #category : #abstract }
OSTouchThreeFingersSwipeDetector >> eventClassEndGesture [
	^ OSTouchThreeFingersSwipeEndEvent 
]

{ #category : #abstract }
OSTouchThreeFingersSwipeDetector >> eventClassMoveGesture [
	^ OSTouchThreeFingersSwipeMoveEvent
]

{ #category : #preconditions }
OSTouchThreeFingersSwipeDetector >> necessaryFingers [
	^ 3
]
