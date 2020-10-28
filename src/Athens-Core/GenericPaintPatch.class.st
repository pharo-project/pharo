"
I am a generic patch whose content is built through path building style commands. I have rough correspondence to SVG mesh gradient paths.
"
Class {
	#name : #GenericPaintPatch,
	#superclass : #AthensAbstractMeshPaintPatch,
	#instVars : [
		'buildBlock'
	],
	#category : #'Athens-Core-Paints'
}

{ #category : #accessing }
GenericPaintPatch >> buildBlock [
	^ buildBlock
]

{ #category : #accessing }
GenericPaintPatch >> buildBlock: anObject [
	buildBlock := anObject
]
