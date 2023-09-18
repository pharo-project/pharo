"
this class is subject of changes/removal. do not use it.
"
Class {
	#name : #AthensPathSegmentConverter,
	#superclass : #Object,
	#instVars : [
		'dest',
		'endPoint',
		'contourStartPt'
	],
	#category : #'Athens-Core-Paths'
}

{ #category : #converting }
AthensPathSegmentConverter >> convert: aPath [
	"pipe the path segments to destination, converting
	segments on the way"

	contourStartPt := endPoint := 0@0.

	aPath sendCommandsTo: self
]

{ #category : #accessing }
AthensPathSegmentConverter >> dest: aDest [
	dest := aDest
]
