"
It exposes an athens surface in a morph
"
Class {
	#name : #SpAthensMorph,
	#superclass : #Morph,
	#instVars : [
		'surface',
		'drawBlock'
	],
	#category : #'Spec2-Adapters-Morphic-Notebook'
}

{ #category : #accessing }
SpAthensMorph >> drawBlock: aBlockClosure [
	drawBlock := aBlockClosure
]

{ #category : #accessing }
SpAthensMorph >> drawOn: aMorphicCanvas [
	self redraw.
	self surface displayOnMorphicCanvas: aMorphicCanvas at: bounds origin
]

{ #category : #drawing }
SpAthensMorph >> redraw [
	self surface drawDuring: [ :canvas | drawBlock cull: canvas ]
]

{ #category : #accessing }
SpAthensMorph >> surface [
	^ surface
		ifNil: [ surface := AthensCairoSurface extent: self extent ]
]
