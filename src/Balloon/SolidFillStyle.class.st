"
SolidFillStyle is a fill which represents a color for the BalloonEngine.

Instance variables:
	color	<Color>	The color to use.
	pixelValue32 <Integer>	The cached pixel value to use.
"
Class {
	#name : #SolidFillStyle,
	#superclass : #FillStyle,
	#instVars : [
		'color',
		'pixelValue32'
	],
	#category : #'Balloon-Fills'
}

{ #category : #'instance creation' }
SolidFillStyle class >> color: aColor [
	^self new color: aColor
]

{ #category : #comparing }
SolidFillStyle >> = aFillStyle [

	^ aFillStyle species = self species
		and: [ self asColor = aFillStyle asColor ]
]

{ #category : #converting }
SolidFillStyle >> asColor [
	^color
]

{ #category : #accessing }
SolidFillStyle >> color: aColor [
	color := aColor.
	pixelValue32 := aColor scaledPixelValue32
]

{ #category : #comparing }
SolidFillStyle >> hash [

	^ color hash
]

{ #category : #testing }
SolidFillStyle >> isSolidFill [
	^true
]

{ #category : #testing }
SolidFillStyle >> isTranslucent [
	^color isTranslucent
]

{ #category : #testing }
SolidFillStyle >> isTransparent [
	^color isTransparent
]

{ #category : #printing }
SolidFillStyle >> printOn: aStream [
	super printOn: aStream.
	aStream nextPut:$(; print: color; nextPut:$).
]

{ #category : #accessing }
SolidFillStyle >> scaledPixelValue32 [
	"Return the alpha scaled pixel value for depth 32"
	^pixelValue32
]
