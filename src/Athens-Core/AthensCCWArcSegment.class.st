"
i represent a circular arc, connecting  
previous segment endpoint and my endpoing
of given angle, passing in counter-clockwise direction.
"
Class {
	#name : #AthensCCWArcSegment,
	#superclass : #AthensArcSegment,
	#category : #'Athens-Core-Paths'
}

{ #category : #visitor }
AthensCCWArcSegment >> accept: aVisitor [
	^ aVisitor ccwArcSegment: self
]

{ #category : #visitor }
AthensCCWArcSegment >> sendCommandTo: aBuilder [
	^ aBuilder ccwArcTo: endPoint angle: angle
]
