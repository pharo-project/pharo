"
My subclasses used internally in balloon backend for Athens to represent gradient paints.
"
Class {
	#name : #AthensBalloonGradientPaint,
	#superclass : #AthensBalloonPaint,
	#instVars : [
		'colorRamp'
	],
	#category : #'Athens-Balloon-Paint'
}

{ #category : #testing }
AthensBalloonGradientPaint class >> isAbstract [

	^ self == AthensBalloonGradientPaint
]

{ #category : #accessing }
AthensBalloonGradientPaint >> colorRamp [

	^ colorRamp
]

{ #category : #accessing }
AthensBalloonGradientPaint >> colorRamp: anObject [

	colorRamp := anObject
]

{ #category : #rendering }
AthensBalloonGradientPaint >> strokeRect: aRectangle on: anAthensCanvas stroke: anAthensBalloonStrokePaint [

	self shouldBeImplemented
]

{ #category : #rendering }
AthensBalloonGradientPaint >> strokeRect: aRectangle on: anAthensCanvas strokeWidth: strokeWidth [

	self shouldBeImplemented
]
