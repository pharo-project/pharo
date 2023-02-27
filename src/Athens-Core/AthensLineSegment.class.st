"
i represent a line segment,
created using 
	#lineTo: 
command of AthensPathBuilder
"
Class {
	#name : #AthensLineSegment,
	#superclass : #AthensPathSegment,
	#instVars : [
		'point'
	],
	#category : #'Athens-Core-Paths'
}

{ #category : #visitor }
AthensLineSegment >> accept: aVisitor [
	^ aVisitor lineSegment: self
]

{ #category : #accessing }
AthensLineSegment >> endPoint [
	^ point
]

{ #category : #accessing }
AthensLineSegment >> point [

	^ point
]

{ #category : #accessing }
AthensLineSegment >> point: anObject [

	point := anObject
]

{ #category : #visitor }
AthensLineSegment >> sendCommandTo: aBuilder [
	^ aBuilder lineTo: point
]

{ #category : #visitor }
AthensLineSegment >> visitWith: visitor [

	visitor visitLineSegment:  self
]
