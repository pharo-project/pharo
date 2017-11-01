"
Two fingers are rotating
"
Class {
	#name : #OSTouchTwoFingersRotationMoveEvent,
	#superclass : #OSTouchTwoFingersRotationEvent,
	#category : 'OSWindow-Core-Events-Touch'
}

{ #category : #visitor }
OSTouchTwoFingersRotationMoveEvent >> accept: aVisitor [
	aVisitor visitTouchTwoFingersRotationMoveEvent: self
]
