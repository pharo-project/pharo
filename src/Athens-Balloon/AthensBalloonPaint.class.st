"
I'm an abstract root class of many balloon-specific paints
"
Class {
	#name : #AthensBalloonPaint,
	#superclass : #AthensPaint,
	#category : #'Athens-Balloon-Paint'
}

{ #category : #testing }
AthensBalloonPaint class >> isAbstract [

	^ self == AthensBalloonPaint
]

{ #category : #converting }
AthensBalloonPaint >> asAthensPaintOn: surf [

	^ self
]

{ #category : #converting }
AthensBalloonPaint >> asStrokePaintOn: aCanvas [

	^ AthensBalloonStrokePaint new fillPaint: self
]
