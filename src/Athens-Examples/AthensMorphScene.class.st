"
Allows to render a morph as a scene in athens
"
Class {
	#name : #AthensMorphScene,
	#superclass : #Object,
	#instVars : [
		'morph'
	],
	#category : #'Athens-Examples-Morphic'
}

{ #category : #accessing }
AthensMorphScene >> morph [

	^ morph
]

{ #category : #accessing }
AthensMorphScene >> morph: anObject [

	morph := anObject
]

{ #category : #rendering }
AthensMorphScene >> renderOn:aCanvas [
	morph fullDrawOnAthensCanvas: aCanvas
]
