"
Two fingers are pinching
"
Class {
	#name : 'OSTouchTwoFingersPinchMoveEvent',
	#superclass : 'OSTouchTwoFingersPinchEvent',
	#category : 'OSWindow-Core-Events-Touch',
	#package : 'OSWindow-Core',
	#tag : 'Events-Touch'
}

{ #category : 'visitor' }
OSTouchTwoFingersPinchMoveEvent >> accept: aVisitor [
	aVisitor visitTouchTwoFingersPinchMoveEvent: self
]
