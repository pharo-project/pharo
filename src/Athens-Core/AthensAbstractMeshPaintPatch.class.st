"
I am a patch in a mesh gradient.
"
Class {
	#name : #AthensAbstractMeshPaintPatch,
	#superclass : #Object,
	#instVars : [
		'controlPoints',
		'colors'
	],
	#category : #'Athens-Core-Paints'
}

{ #category : #constructor }
AthensAbstractMeshPaintPatch class >> controlPoints: controlPoints colors: colors [
	^ self new controlPoints: controlPoints; colors: colors; yourself
]

{ #category : #testing }
AthensAbstractMeshPaintPatch class >> isAbstract [

	^ self == AthensAbstractMeshPaintPatch
]

{ #category : #accessing }
AthensAbstractMeshPaintPatch >> colors [
	^ colors
]

{ #category : #accessing }
AthensAbstractMeshPaintPatch >> colors: anObject [
	colors := anObject
]

{ #category : #accessing }
AthensAbstractMeshPaintPatch >> controlPoints [
	^ controlPoints
]

{ #category : #accessing }
AthensAbstractMeshPaintPatch >> controlPoints: anObject [
	controlPoints := anObject
]
