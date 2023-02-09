"
i represent a linear gradient paint in balloon backend.
"
Class {
	#name : #AthensBalloonLinearGradient,
	#superclass : #AthensBalloonGradientPaint,
	#instVars : [
		'origin',
		'corner'
	],
	#category : #'Athens-Balloon-Paint'
}

{ #category : #rendering }
AthensBalloonLinearGradient >> athensFillPath: aPath on: anAthensCanvas [

	| torigin tcorner |

	"A gradient fill coordinates are first transformed by paintTransform matrix, then
	 by pathTransform matrix"

	torigin := anAthensCanvas paintTransform transform: origin.
	tcorner :=  anAthensCanvas paintTransform transform: corner.

	^ anAthensCanvas surface engine
		prepareForRendering;
		registerLinearGradient: colorRamp origin: torigin corner: tcorner;
		fillBezierShape: aPath contoursForFilling
]

{ #category : #rendering }
AthensBalloonLinearGradient >> athensFillRectangle: aRect on: anAthensCanvas [
	"This is a terminal method in rendering dispatch scheme
	canvas->shape->paint. See AthensCanvas>>fillShape: "

	| torigin tcorner |

	"A gradient fill coordinates are first transformed by paintTransform matrix, then
	 by pathTransform matrix"

	torigin := anAthensCanvas paintTransform transform: origin.
	tcorner :=  anAthensCanvas paintTransform transform: corner.

	^ anAthensCanvas surface engine
		prepareForRendering;
		registerLinearGradient: colorRamp origin: torigin corner: tcorner;
		fillRectangle: aRect
]

{ #category : #accessing }
AthensBalloonLinearGradient >> corner [

	^ corner
]

{ #category : #accessing }
AthensBalloonLinearGradient >> corner: anObject [

	corner := anObject
]

{ #category : #accessing }
AthensBalloonLinearGradient >> origin [

	^ origin
]

{ #category : #accessing }
AthensBalloonLinearGradient >> origin: anObject [

	origin := anObject
]

{ #category : #rendering }
AthensBalloonLinearGradient >> strokeRect: rect on: canvas stroke: strokePaint [

	| torigin tcorner |

	"A gradient fill coordinates are first transformed by
	paintTransform matrix,
	then
	by pathTransform matrix"

	torigin := canvas paintTransform transform: origin.
	tcorner :=  canvas paintTransform transform: corner.

	^ canvas surface engine
		prepareForRendering;
		registerLinearGradient: colorRamp origin: torigin corner: tcorner;
		strokeRectangle: rect width: strokePaint width
]

{ #category : #rendering }
AthensBalloonLinearGradient >> strokeRect: rect on: canvas strokeWidth: strokeWidth [

	| torigin tcorner |

	"A gradient fill coordinates are first transformed by
	paintTransform matrix,
	then
	by pathTransform matrix"

	torigin := canvas paintTransform transform: origin.
	tcorner :=  canvas paintTransform transform: corner.

	^ canvas surface engine
		prepareForRendering;
		registerLinearGradient: colorRamp origin: torigin corner: tcorner;
		strokeRectangle: rect width: strokeWidth
]
