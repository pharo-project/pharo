"
It exposes an athens surface in a morph
"
Class {
	#name : #SpecAthensMorph,
	#superclass : #Morph,
	#instVars : [
		'surface',
		'drawBlock'
	],
	#category : #'Spec-MorphicAdapters-Notebook'
}

{ #category : #accessing }
SpecAthensMorph >> drawBlock: aBlockClosure [
	drawBlock := aBlockClosure
]

{ #category : #accessing }
SpecAthensMorph >> drawOn: aMorphicCanvas [
	self redraw.
	self surface displayOnMorphicCanvas: aMorphicCanvas at: bounds origin
]

{ #category : #drawing }
SpecAthensMorph >> redraw [
	self surface drawDuring: [ :canvas | drawBlock cull: canvas ]
]

{ #category : #accessing }
SpecAthensMorph >> surface [
	^ surface
		ifNil: [ surface := AthensCairoSurface extent: self extent ]
]
