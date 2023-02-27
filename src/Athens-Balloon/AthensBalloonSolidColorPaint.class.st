"
maybe Color could act as a AthensPaint by itself by implementing a corresponding protocol
"
Class {
	#name : #AthensBalloonSolidColorPaint,
	#superclass : #AthensBalloonPaint,
	#instVars : [
		'color'
	],
	#category : #'Athens-Balloon-Paint'
}

{ #category : #rendering }
AthensBalloonSolidColorPaint >> athensFillPath: athensBalloonPath on: anAthensCanvas [

	^ anAthensCanvas surface
		fillBezierShape: athensBalloonPath contoursForFilling
		color: color
]

{ #category : #rendering }
AthensBalloonSolidColorPaint >> athensFillRectangle: aRect on: canvas [
	"This is a terminal method in rendering dispatch scheme
	canvas->shape->paint. See AthensCanvas>>fillShape: "

	^ canvas surface fillRectangle: aRect color: color
]

{ #category : #accessing }
AthensBalloonSolidColorPaint >> color [

	^ color
]

{ #category : #accessing }
AthensBalloonSolidColorPaint >> color: anObject [

	color := anObject
]

{ #category : #rendering }
AthensBalloonSolidColorPaint >> strokePath: aPath on: anAthensCanvas stroke: strokePaint [

	 anAthensCanvas surface
		strokeBezierShape: aPath contoursForStroke
		color: color
		width: strokePaint width
]

{ #category : #rendering }
AthensBalloonSolidColorPaint >> strokePath: aPath on: anAthensCanvas strokeWidth: strokeWidth [

	 anAthensCanvas surface
		strokeBezierShape: aPath contoursForStroke
		color: color
		width: strokeWidth
]

{ #category : #rendering }
AthensBalloonSolidColorPaint >> strokeRect: rect on: canvas stroke: strokePaint [

	^ canvas surface strokeRectangle: rect color: color width: strokePaint width
]

{ #category : #rendering }
AthensBalloonSolidColorPaint >> strokeRect: rect on: canvas strokeWidth: strokeWidth [

	^ canvas surface strokeRectangle: rect color: color width: strokeWidth
]
