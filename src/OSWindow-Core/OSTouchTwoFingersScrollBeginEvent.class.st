"
Two fingers started to scroll
"
Class {
	#name : #OSTouchTwoFingersScrollBeginEvent,
	#superclass : #OSTouchTwoFingersScrollEvent,
	#category : 'OSWindow-Core-Events-Touch'
}

{ #category : #visitor }
OSTouchTwoFingersScrollBeginEvent >> accept: aVisitor [
	aVisitor visitTouchTwoFingersScrollBeginEvent: self
]
