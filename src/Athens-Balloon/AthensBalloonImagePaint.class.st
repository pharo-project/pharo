"
i representing an image (form) paint  
"
Class {
	#name : #AthensBalloonImagePaint,
	#superclass : #AthensPaint,
	#instVars : [
		'form',
		'origin',
		'direction',
		'tileFlag'
	],
	#category : #'Athens-Balloon-Paint'
}

{ #category : #rendering }
AthensBalloonImagePaint >> athensFillPath: aPath on: anAthensCanvas [

	anAthensCanvas surface engine
		prepareForRendering;
		registerFormFill: form origin: origin direction: direction normal: self normal;
		fillBezierShape: aPath contoursForFilling
]

{ #category : #rendering }
AthensBalloonImagePaint >> athensFillRectangle: aRect on: anAthensCanvas [
	"This is a terminal method in rendering dispatch scheme
	canvas->shape->paint. See AthensCanvas>>fillShape: "
	|rotation normal|
	rotation := anAthensCanvas surface paintTransform transform: direction.
	normal := anAthensCanvas surface paintTransform transform:self normal.


	anAthensCanvas surface engine reset
		prepareForRendering;
		registerFormFill: form origin: origin direction: rotation normal: normal tileFlag: tileFlag;
		fillRectangle: aRect
]

{ #category : #accessing }
AthensBalloonImagePaint >> direction [

	^ direction
]

{ #category : #accessing }
AthensBalloonImagePaint >> direction: anObject [

	direction := anObject
]

{ #category : #accessing }
AthensBalloonImagePaint >> form [

	^ form
]

{ #category : #accessing }
AthensBalloonImagePaint >> form: anObject [

	form := anObject.
	origin := 0@0.
	direction := form extent x @0
]

{ #category : #initialization }
AthensBalloonImagePaint >> initialize [
	super initialize.

	direction := 0 @ 0.
	origin := 0 @ 0.
	tileFlag := false
]

{ #category : #accessing }
AthensBalloonImagePaint >> normal [

	^ (direction y negated @ direction x * form height / form width) rounded
]

{ #category : #accessing }
AthensBalloonImagePaint >> origin [

	^ origin
]

{ #category : #accessing }
AthensBalloonImagePaint >> origin: anObject [

	origin := anObject
]

{ #category : #accessing }
AthensBalloonImagePaint >> repeat [

	tileFlag := true
]
