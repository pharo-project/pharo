"
A non-primary pointer has gone down.
"
Class {
	#name : #OSTouchActionPointerDownEvent,
	#superclass : #OSTouchActionPointerEvent,
	#category : 'OSWindow-Core-Events-Touch'
}

{ #category : #visitor }
OSTouchActionPointerDownEvent >> accept: aVisitor [
	^aVisitor visitTouchPointerDownEvent: self.
]
