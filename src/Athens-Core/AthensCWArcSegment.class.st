"
i represent a circular arc, connecting  
previous segment endpoint and my endpoing
of given angle, passing in clockwise direction.
"
Class {
	#name : #AthensCWArcSegment,
	#superclass : #AthensArcSegment,
	#category : #'Athens-Core-Paths'
}

{ #category : #visitor }
AthensCWArcSegment >> accept: aVisitor [
	^ aVisitor cwArcSegment: self
]

{ #category : #visitor }
AthensCWArcSegment >> sendCommandTo: aBuilder [
	^ aBuilder cwArcTo: endPoint angle: angle
]
