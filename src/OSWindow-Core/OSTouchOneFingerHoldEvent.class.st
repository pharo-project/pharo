"
One finger is down and has not moved on the screen
"
Class {
	#name : 'OSTouchOneFingerHoldEvent',
	#superclass : 'OSTouchHoldEvent',
	#category : 'OSWindow-Core-Events-Touch',
	#package : 'OSWindow-Core',
	#tag : 'Events-Touch'
}

{ #category : 'visitor' }
OSTouchOneFingerHoldEvent >> accept: aVisitor [
	aVisitor visitTouchOneFingerHoldEvent: self
]
