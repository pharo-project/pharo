"
Three fingers started to scroll
"
Class {
	#name : 'OSTouchThreeFingersScrollBeginEvent',
	#superclass : 'OSTouchThreeFingersScrollEvent',
	#category : 'OSWindow-Core-Events-Touch',
	#package : 'OSWindow-Core',
	#tag : 'Events-Touch'
}

{ #category : 'visitor' }
OSTouchThreeFingersScrollBeginEvent >> accept: aVisitor [
	aVisitor visitTouchThreeFingersScrollBeginEvent: self
]
