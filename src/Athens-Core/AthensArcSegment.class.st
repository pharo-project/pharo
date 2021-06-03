"
i am abstract class , representing an arc segment
"
Class {
	#name : #AthensArcSegment,
	#superclass : #AthensPathSegment,
	#instVars : [
		'endPoint',
		'angle'
	],
	#category : #'Athens-Core-Paths'
}

{ #category : #accessing }
AthensArcSegment >> angle [
	^ angle
]

{ #category : #accessing }
AthensArcSegment >> endPoint [
	^ endPoint
]

{ #category : #accessing }
AthensArcSegment >> endPoint: pt angle: a [

	endPoint := pt.
	angle := a
]
