"
i represent a quadric Bezier path segment.

Any quadratic spline can be expressed as a cubic (where the cubic term is zero). The end points of the cubic will be the same as the quadratic's.

    CP0 = QP0
    CP3 = QP2 

The two control points for the cubic are:

    CP1 = QP0 + 2/3 *(QP1-QP0)
    CP2 = QP2 + 2/3 *(QP1-QP2) 
"
Class {
	#name : #AthensQuadSegment,
	#superclass : #AthensPathSegment,
	#instVars : [
		'via',
		'to'
	],
	#category : #'Athens-Core-Paths'
}

{ #category : #visitor }
AthensQuadSegment >> accept: aVisitor [
	^ aVisitor quadricBezierSegment: self
]

{ #category : #accessing }
AthensQuadSegment >> endPoint [
	^ to
]

{ #category : #initialization }
AthensQuadSegment >> from: pt0 via: pt1 to: p [

	via := pt1.
	to := p
]

{ #category : #visitor }
AthensQuadSegment >> sendCommandTo: aBuilder [
	^ aBuilder curveVia: via to: to
]

{ #category : #accessing }
AthensQuadSegment >> to [

	^ to
]

{ #category : #accessing }
AthensQuadSegment >> via [

	^ via
]

{ #category : #visitor }
AthensQuadSegment >> visitWith: visitor [

	visitor visitQuadSegment: self
]
