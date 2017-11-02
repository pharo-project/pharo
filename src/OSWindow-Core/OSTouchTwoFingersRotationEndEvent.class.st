"
Two fingers had stopped to rotate
"
Class {
	#name : #OSTouchTwoFingersRotationEndEvent,
	#superclass : #OSTouchTwoFingersRotationEvent,
	#category : 'OSWindow-Core-Events-Touch'
}

{ #category : #visitor }
OSTouchTwoFingersRotationEndEvent >> accept: aVisitor [
	aVisitor visitTouchTwoFingersRotationEndEvent: self
]
