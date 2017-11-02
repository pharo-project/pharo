"
Two fingers started pinching
"
Class {
	#name : #OSTouchTwoFingersPinchBeginEvent,
	#superclass : #OSTouchTwoFingersPinchEvent,
	#category : 'OSWindow-Core-Events-Touch'
}

{ #category : #visitor }
OSTouchTwoFingersPinchBeginEvent >> accept: aVisitor [
	aVisitor visitTouchTwoFingersPinchBeginEvent: self
]
