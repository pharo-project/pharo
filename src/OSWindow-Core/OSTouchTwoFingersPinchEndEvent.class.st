"
Two fingers finished to pinch
"
Class {
	#name : 'OSTouchTwoFingersPinchEndEvent',
	#superclass : 'OSTouchTwoFingersPinchEvent',
	#category : 'OSWindow-Core-Events-Touch',
	#package : 'OSWindow-Core',
	#tag : 'Events-Touch'
}

{ #category : 'visitor' }
OSTouchTwoFingersPinchEndEvent >> accept: aVisitor [
	aVisitor visitTouchTwoFingersPinchEndEvent: self
]
