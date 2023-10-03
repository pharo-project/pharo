"
Three fingers finished to swipe
"
Class {
	#name : 'OSTouchThreeFingersSwipeEndEvent',
	#superclass : 'OSTouchThreeFingersSwipeEvent',
	#category : 'OSWindow-Core-Events-Touch',
	#package : 'OSWindow-Core',
	#tag : 'Events-Touch'
}

{ #category : 'visitor' }
OSTouchThreeFingersSwipeEndEvent >> accept: aVisitor [
	aVisitor visitTouchThreeFingersSwipeEndEvent: self
]
