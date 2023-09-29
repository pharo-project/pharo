"
A non-primary pointer has gone up.
"
Class {
	#name : 'OSTouchActionPointerUpEvent',
	#superclass : 'OSTouchActionPointerEvent',
	#category : 'OSWindow-Core-Events-Touch',
	#package : 'OSWindow-Core',
	#tag : 'Events-Touch'
}

{ #category : 'visitor' }
OSTouchActionPointerUpEvent >> accept: aVisitor [
	^ aVisitor visitTouchPointerUpEvent: self
]
