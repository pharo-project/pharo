"
aFinger just move.
"
Class {
	#name : 'OSTouchActionPointerMoveEvent',
	#superclass : 'OSTouchActionPointerEvent',
	#category : 'OSWindow-Core-Events-Touch',
	#package : 'OSWindow-Core',
	#tag : 'Events-Touch'
}

{ #category : 'visitor' }
OSTouchActionPointerMoveEvent >> accept: aVisitor [
	^aVisitor visitTouchPointerMoveEvent: self
]
