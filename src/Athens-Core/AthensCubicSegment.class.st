"
i represent a cubic Bezier path segment
"
Class {
	#name : #AthensCubicSegment,
	#superclass : #AthensPathSegment,
	#instVars : [
		'via1',
		'via2',
		'to'
	],
	#category : #'Athens-Core-Paths'
}

{ #category : #visitor }
AthensCubicSegment >> accept: aVisitor [
	^ aVisitor cubicBezierSegment: self
]

{ #category : #accessing }
AthensCubicSegment >> endPoint [
	^ to
]

{ #category : #accessing }
AthensCubicSegment >> from: origin
	via: controlPoint
	and: secondControlPoint
	to: destination [


	via1:= controlPoint.
	via2 := secondControlPoint.
	to := destination
]

{ #category : #testing }
AthensCubicSegment >> isCubic [
	^true
]

{ #category : #visitor }
AthensCubicSegment >> sendCommandTo: aBuilder [
	^ aBuilder curveVia: via1 and: via2  to: to
]

{ #category : #accessing }
AthensCubicSegment >> to [

	^ to
]

{ #category : #accessing }
AthensCubicSegment >> via1 [
	^ via1
]

{ #category : #accessing }
AthensCubicSegment >> via2 [
	^ via2
]

{ #category : #accessing }
AthensCubicSegment >> via2reflected [
	^self endPoint * 2 - via2
]
