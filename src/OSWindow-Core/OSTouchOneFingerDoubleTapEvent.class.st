"
A finger tapped quickly two time at the same position
"
Class {
	#name : 'OSTouchOneFingerDoubleTapEvent',
	#superclass : 'OSTouchDoubleTapEvent',
	#category : 'OSWindow-Core-Events-Touch',
	#package : 'OSWindow-Core',
	#tag : 'Events-Touch'
}

{ #category : 'visitor' }
OSTouchOneFingerDoubleTapEvent >> accept: aVisitor [
	aVisitor visitTouchOneFingerDoubleTapEvent: self
]
