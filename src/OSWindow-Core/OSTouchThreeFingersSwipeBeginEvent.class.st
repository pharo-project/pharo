"
Three fingers started to swipe
"
Class {
	#name : 'OSTouchThreeFingersSwipeBeginEvent',
	#superclass : 'OSTouchThreeFingersSwipeEvent',
	#category : 'OSWindow-Core-Events-Touch',
	#package : 'OSWindow-Core',
	#tag : 'Events-Touch'
}

{ #category : 'visitor' }
OSTouchThreeFingersSwipeBeginEvent >> accept: aVisitor [
	aVisitor visitTouchThreeFingersSwipeBeginEvent: self
]
