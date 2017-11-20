"
Two fingers had began to rotate
"
Class {
	#name : #OSTouchTwoFingersRotationBeginEvent,
	#superclass : #OSTouchTwoFingersRotationEvent,
	#category : #OSWindow-Core-Events-Touch
}

{ #category : #visitor }
OSTouchTwoFingersRotationBeginEvent >> accept: aVisitor [
	aVisitor visitTouchTwoFingersRotationBeginEvent: self
]
