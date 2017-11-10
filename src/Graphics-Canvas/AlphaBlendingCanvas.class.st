"
see ColorMappingCanvas comment.
"
Class {
	#name : #AlphaBlendingCanvas,
	#superclass : #ColorMappingCanvas,
	#instVars : [
		'alpha'
	],
	#category : #Graphics-Canvas
}

{ #category : #accessing }
AlphaBlendingCanvas >> alpha [
	^alpha
]

{ #category : #accessing }
AlphaBlendingCanvas >> alpha: newAlpha [
	alpha := newAlpha.
]

{ #category : #drawing-images }
AlphaBlendingCanvas >> image: aForm at: aPoint sourceRect: sourceRect rule: rule [
	"Draw the given form. For the 'paint' combination rule use stenciling otherwise simply fill the source rectangle.
	Do a blendAlpha if the rule is blend to cope with translucent images being drawn (via translucentImage:...)."
	
	rule = Form paint ifTrue:[
		^myCanvas
			image: aForm
			at: aPoint
			sourceRect: sourceRect
			rule: Form paintAlpha
			alpha: alpha.
	].
	rule = Form over ifTrue:[
		^myCanvas
			image: aForm
			at: aPoint
			sourceRect: sourceRect
			rule: Form blendAlpha
			alpha: alpha.
	].
	rule = Form blend ifTrue:[
		^myCanvas
			image: aForm
			at: aPoint
			sourceRect: sourceRect
			rule: Form blendAlpha
			alpha: alpha.
	].
]

{ #category : #private }
AlphaBlendingCanvas >> mapColor: aColor [
	aColor isColor ifFalse:[^aColor]. "Should not happen but who knows..."
	aColor isTransparent ifTrue:[^aColor].
	aColor isOpaque ifTrue:[^aColor alpha: alpha].
	^aColor alpha: (aColor alpha * alpha)
]

{ #category : #initialization }
AlphaBlendingCanvas >> on: aCanvas [
	myCanvas := aCanvas.
	alpha := 1.0.
]
