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

{ #category : #accessors }
AthensArcSegment >> angle [
	^ angle
]

{ #category : #accessors }
AthensArcSegment >> endPoint [
	^ endPoint
]

{ #category : #accessors }
AthensArcSegment >> endPoint: pt angle: a [

	endPoint := pt.
	angle := a
]
