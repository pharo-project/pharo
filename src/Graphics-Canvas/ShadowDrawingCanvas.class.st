"
see ColorMappingCanvas comment.
"
Class {
	#name : #ShadowDrawingCanvas,
	#superclass : #ColorMappingCanvas,
	#instVars : [
		'shadowColor'
	],
	#category : #'Graphics-Canvas'
}

{ #category : #private }
ShadowDrawingCanvas >> image: aForm at: aPoint sourceRect: sourceRect rule: rule [
	"Draw the given form. For the 'paint' combination rule use stenciling otherwise simply fill the source rectangle."
	rule = Form paint ifTrue:[
		^myCanvas
			stencil: aForm
			at: aPoint
			sourceRect: sourceRect
			color: shadowColor
	] ifFalse:[
		^myCanvas
			fillRectangle: (sourceRect translateBy: aPoint)
			color: shadowColor
	].
]

{ #category : #testing }
ShadowDrawingCanvas >> isShadowDrawing [
	^true
]

{ #category : #private }
ShadowDrawingCanvas >> mapColor: aColor [
	aColor isColor ifFalse:[^aColor]. "Should not happen but who knows..."
	^aColor isTransparent
		ifTrue:[aColor]
		ifFalse:[shadowColor]
]

{ #category : #initialization }
ShadowDrawingCanvas >> on: aCanvas [
	myCanvas := aCanvas.
	shadowColor := Color black.
]

{ #category : #accessing }
ShadowDrawingCanvas >> shadowColor [
	^shadowColor
]

{ #category : #accessing }
ShadowDrawingCanvas >> shadowColor: aColor [
	shadowColor := aColor
]
