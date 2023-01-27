"
I am used to convert gradient fill style(s) to appropriate paint(s) in Athens.
"
Class {
	#name : #AthensGradientPaint,
	#superclass : #AthensPaint,
	#instVars : [
		'gradient'
	],
	#category : #'Athens-Balloon-Paint'
}

{ #category : #rendering }
AthensGradientPaint >> athensFillPath: aPath on: canvas [
	"This is a terminal method in rendering dispatch scheme
	canvas->shape->paint. See AthensCanvas>>fillShape: "

	| fill |
	fill := self convertGradientToPaintOn: canvas.
	fill athensFillPath: aPath on: canvas
]

{ #category : #rendering }
AthensGradientPaint >> athensFillRectangle: aRect on: canvas [
	"This is a terminal method in rendering dispatch scheme
	canvas->shape->paint. See AthensCanvas>>fillShape: "

	| fill |
	fill := self convertGradientToPaintOn: canvas.
	fill athensFillRectangle: aRect on: canvas
]

{ #category : #private }
AthensGradientPaint >> convertGradientToPaintOn: aCanvas [

	"This is a terminal method in rendering dispatch scheme
	canvas->shape->paint. See AthensCanvas>>fillShape: "

	^ gradient radial
		  ifTrue: [
			  aCanvas surface
				  createRadialGradient: gradient colorRamp
				  center:
				  (aCanvas pathTransform inverseTransform: gradient origin)
				  radius: gradient direction y ]
		  ifFalse: [
			  aCanvas surface
				  createLinearGradient: gradient colorRamp
				  start: (aCanvas pathTransform inverseTransform: gradient origin)
				  stop: (aCanvas pathTransform inverseTransform:
						   gradient origin + gradient direction) ]
]

{ #category : #accessing }
AthensGradientPaint >> gradient: aGradient [

	gradient := aGradient
]
