"
Two fingers are scrolling
"
Class {
	#name : 'OSTouchTwoFingersScrollMoveEvent',
	#superclass : 'OSTouchTwoFingersScrollEvent',
	#category : 'OSWindow-Core-Events-Touch',
	#package : 'OSWindow-Core',
	#tag : 'Events-Touch'
}

{ #category : 'visitor' }
OSTouchTwoFingersScrollMoveEvent >> accept: aVisitor [
	aVisitor visitTouchTwoFingersScrollMoveEvent: self
]
