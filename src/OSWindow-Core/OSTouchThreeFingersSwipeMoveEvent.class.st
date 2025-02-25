"
Three fingers are swiping
"
Class {
	#name : 'OSTouchThreeFingersSwipeMoveEvent',
	#superclass : 'OSTouchThreeFingersSwipeEvent',
	#category : 'OSWindow-Core-Events-Touch',
	#package : 'OSWindow-Core',
	#tag : 'Events-Touch'
}

{ #category : 'visitor' }
OSTouchThreeFingersSwipeMoveEvent >> accept: aVisitor [
	aVisitor visitTouchThreeFingersSwipeMoveEvent: self
]
