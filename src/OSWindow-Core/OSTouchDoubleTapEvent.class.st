"
I am the abstract class for all double tap gestures
"
Class {
	#name : 'OSTouchDoubleTapEvent',
	#superclass : 'OSTouchComplexEvent',
	#category : 'OSWindow-Core-Events-Touch',
	#package : 'OSWindow-Core',
	#tag : 'Events-Touch'
}

{ #category : 'accessing' }
OSTouchDoubleTapEvent class >> id [
	^ #doubleTap
]
