"
Two fingers had began to rotate
"
Class {
	#name : 'OSTouchTwoFingersRotationBeginEvent',
	#superclass : 'OSTouchTwoFingersRotationEvent',
	#category : 'OSWindow-Core-Events-Touch',
	#package : 'OSWindow-Core',
	#tag : 'Events-Touch'
}

{ #category : 'visitor' }
OSTouchTwoFingersRotationBeginEvent >> accept: aVisitor [
	aVisitor visitTouchTwoFingersRotationBeginEvent: self
]
