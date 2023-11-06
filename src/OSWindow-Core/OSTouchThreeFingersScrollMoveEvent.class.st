"
Three fingers are scrolling
"
Class {
	#name : 'OSTouchThreeFingersScrollMoveEvent',
	#superclass : 'OSTouchThreeFingersScrollEvent',
	#category : 'OSWindow-Core-Events-Touch',
	#package : 'OSWindow-Core',
	#tag : 'Events-Touch'
}

{ #category : 'visitor' }
OSTouchThreeFingersScrollMoveEvent >> accept: aVisitor [
	aVisitor visitTouchThreeFingersScrollMoveEvent: self
]
