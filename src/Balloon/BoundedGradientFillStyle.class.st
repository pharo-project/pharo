"
Gradient fillstyle that draws with optional extent.
"
Class {
	#name : #BoundedGradientFillStyle,
	#superclass : #GradientFillStyle,
	#instVars : [
		'extent'
	],
	#category : 'Balloon-Fills'
}

{ #category : #comparing }
BoundedGradientFillStyle >> = aGradientFillStyle [
	"Answer whether equal."

	^super = aGradientFillStyle
		and: [self extent = aGradientFillStyle extent]
]

{ #category : #accessing }
BoundedGradientFillStyle >> extent [
	"Answer the value of extent"

	^ extent
]

{ #category : #accessing }
BoundedGradientFillStyle >> extent: anObject [
	"Set the value of extent"

	extent := anObject
]

{ #category : #comparing }
BoundedGradientFillStyle >> hash [
	"Hash is implemented because #= is implemented."
	
	^super hash bitXor: self extent hash
]
