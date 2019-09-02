"
Simple fillstyle that draws a color at the specified origin with option extent.
"
Class {
	#name : #ColorFillStyle,
	#superclass : #SolidFillStyle,
	#instVars : [
		'origin',
		'extent'
	],
	#category : #'Balloon-Fills'
}

{ #category : #comparing }
ColorFillStyle >> = anObject [

	^ super = anObject
		and: [ origin = anObject origin
			and: [ extent = anObject extent ] ]
]

{ #category : #accessing }
ColorFillStyle >> extent [
	"Answer the value of extent"

	^ extent
]

{ #category : #accessing }
ColorFillStyle >> extent: anObject [
	"Set the value of extent"

	extent := anObject
]

{ #category : #operating }
ColorFillStyle >> fillRectangle: aRectangle on: aCanvas [
	"Fill the given rectangle on the given canvas with the receiver."
	
	|o c|
	o := self origin ifNil: [aRectangle origin] ifNotNil: [self origin].
	c := self extent ifNil: [aRectangle corner] ifNotNil: [o + self extent].
	aCanvas fillRectangle: (o corner: c) basicFillStyle: self
]

{ #category : #testing }
ColorFillStyle >> isOrientedFill [
	"Answer true if origin is not nil so that morph movement adjusts origin."
	
	^self origin notNil
]

{ #category : #accessing }
ColorFillStyle >> origin [
	"Answer the value of origin"

	^ origin
]

{ #category : #accessing }
ColorFillStyle >> origin: anObject [
	"Set the value of origin"

	origin := anObject
]
