"
Two fingers stopped to scroll
"
Class {
	#name : #OSTouchTwoFingersScrollEndEvent,
	#superclass : #OSTouchTwoFingersScrollEvent,
	#category : #'OSWindow-Core-Events-Touch'
}

{ #category : #visitor }
OSTouchTwoFingersScrollEndEvent >> accept: aVisitor [
	aVisitor visitTouchTwoFingersScrollEndEvent: self
]
