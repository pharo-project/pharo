"
Gradient fillstyle that draws with optional extent.
"
Class {
	#name : 'BoundedGradientFillStyle',
	#superclass : 'GradientFillStyle',
	#instVars : [
		'extent'
	],
	#category : 'Graphics-Canvas-Fills',
	#package : 'Graphics-Canvas',
	#tag : 'Fills'
}

{ #category : 'comparing' }
BoundedGradientFillStyle >> = aGradientFillStyle [
	"Answer whether equal."

	^super = aGradientFillStyle
		and: [self extent = aGradientFillStyle extent]
]

{ #category : 'accessing' }
BoundedGradientFillStyle >> extent [
	"Answer the value of extent"

	^ extent
]

{ #category : 'accessing' }
BoundedGradientFillStyle >> extent: anObject [
	"Set the value of extent"

	extent := anObject
]

{ #category : 'drawing - rectangles' }
BoundedGradientFillStyle >> fillRectangle: aRectangle on: aCanvas [
	"Fill the given rectangle on the given canvas with the receiver."

	self extent ifNil: [^super fillRectangle: aRectangle on: aCanvas].
	aCanvas fillRectangle: ((self origin extent: self extent) intersect: aRectangle ifNone: [^ self ]) basicFillStyle: self
]

{ #category : 'comparing' }
BoundedGradientFillStyle >> hash [
	"Hash is implemented because #= is implemented."

	^super hash bitXor: self extent hash
]
