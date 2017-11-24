"
Three fingers aborted to swipe
"
Class {
	#name : #OSTouchThreeFingersSwipeAbortEvent,
	#superclass : #OSTouchThreeFingersSwipeEvent,
	#category : #'OSWindow-Core-Events-Touch'
}

{ #category : #visitor }
OSTouchThreeFingersSwipeAbortEvent >> accept: aVisitor [
	aVisitor visitTouchThreeFingersSwipeAbortEvent: self
]
