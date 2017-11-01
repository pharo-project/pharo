"
aFinger just move.
"
Class {
	#name : #OSTouchActionPointerMoveEvent,
	#superclass : #OSTouchActionPointerEvent,
	#category : 'OSWindow-Core-Events-Touch'
}

{ #category : #visitor }
OSTouchActionPointerMoveEvent >> accept: aVisitor [
	^aVisitor visitTouchPointerMoveEvent: self.
]
