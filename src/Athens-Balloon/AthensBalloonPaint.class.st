"
i am an abstract root of many balloon-specific paints
"
Class {
	#name : #AthensBalloonPaint,
	#superclass : #AthensPaint,
	#category : #Athens-Balloon
}

{ #category : #converting }
AthensBalloonPaint >> asAthensPaintOn: surf [
	^ self 
]

{ #category : #converting }
AthensBalloonPaint >> asStrokePaintOn: aCanvas [
	^ AthensBalloonStrokePaint new fillPaint: self
]
