"
Three fingers finished to scroll
"
Class {
	#name : 'OSTouchThreeFingersScrollEndEvent',
	#superclass : 'OSTouchThreeFingersScrollEvent',
	#category : 'OSWindow-Core-Events-Touch',
	#package : 'OSWindow-Core',
	#tag : 'Events-Touch'
}

{ #category : 'visitor' }
OSTouchThreeFingersScrollEndEvent >> accept: aVisitor [
	aVisitor visitTouchThreeFingersScrollEndEvent: self
]
