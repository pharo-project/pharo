"
i represent a radial gradient paint in balloon backend.
"
Class {
	#name : #AthensBalloonRadialGradient,
	#superclass : #AthensBalloonGradientPaint,
	#instVars : [
		'center',
		'radius'
	],
	#category : #'Athens-Balloon-Paint'
}

{ #category : #rendering }
AthensBalloonRadialGradient >> athensFillPath: aPath on: anAthensCanvas [
	"This is a terminal method in rendering dispatch scheme
	canvas->shape->paint. See AthensCanvas>>fillShape: "

	| c |

	c := anAthensCanvas paintTransform transform: center.

	^ anAthensCanvas surface engine
		prepareForRendering;
		registerRadialGradient: colorRamp center: c radius: radius;
		fillBezierShape: aPath contoursForFilling
]

{ #category : #rendering }
AthensBalloonRadialGradient >> athensFillRectangle: aRect on: anAthensCanvas [
	"This is a terminal method in rendering dispatch scheme
	canvas->shape->paint. See AthensCanvas>>fillShape: "

	| c dir n |

	c := anAthensCanvas paintTransform transform: center.
	dir :=  (radius@0).
	n := (0@radius).

	^ anAthensCanvas surface engine
		prepareForRendering;
		registerRadialGradient: colorRamp center: c direction: dir normal: n;
		fillRectangle: aRect
]

{ #category : #accessing }
AthensBalloonRadialGradient >> center [

	^ center
]

{ #category : #accessing }
AthensBalloonRadialGradient >> center: anObject [

	center := anObject
]

{ #category : #accessing }
AthensBalloonRadialGradient >> radius [

	^ radius
]

{ #category : #accessing }
AthensBalloonRadialGradient >> radius: anObject [

	radius := anObject
]
