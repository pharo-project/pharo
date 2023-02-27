"
I control the paint modes supported by Cairo backend.

for the list and description of paint modes supported by Cairo library, see
http://cairographics.org/operators/
"
Class {
	#name : #AthensCairoPaintMode,
	#superclass : #AthensPaintMode,
	#traits : 'TCairoLibrary',
	#classTraits : 'TCairoLibrary classTrait',
	#instVars : [
		'canvas'
	],
	#pools : [
		'AthensCairoDefinitions'
	],
	#category : #'Athens-Cairo-Paints'
}

{ #category : #modes }
AthensCairoPaintMode >> add [

	^ self setOperator: CAIRO_OPERATOR_ADD
]

{ #category : #modes }
AthensCairoPaintMode >> atop [

	^ self setOperator: CAIRO_OPERATOR_ATOP
]

{ #category : #capabilities }
AthensCairoPaintMode >> availableModes [

	^ #(
		clear
		source
		over
		in
		out
		atop
		dest
		destOver
		destIn
		destOut
		destAtop
		xor
		add
		saturate
		multiply
		screen
		overlay
		darken
		lighten
		colorDodge
		colorBurn
		hardLight
		softLight
		difference
		exclusion
		hslHue
		hslSaturation
		hslColor
		hslLuminosity
	)
]

{ #category : #initialization }
AthensCairoPaintMode >> canvas: aCanvas [
	canvas := aCanvas
]

{ #category : #modes }
AthensCairoPaintMode >> clear [
	"Set 'clear' drawing mode.

	Drawing under this mode will clear the surface under the shape boundaries"

	^ self setOperator: CAIRO_OPERATOR_CLEAR
]

{ #category : #modes }
AthensCairoPaintMode >> colorBurn [

	^ self setOperator: CAIRO_OPERATOR_COLOR_BURN
]

{ #category : #modes }
AthensCairoPaintMode >> colorDodge [

	^ self setOperator: CAIRO_OPERATOR_COLOR_DODGE
]

{ #category : #modes }
AthensCairoPaintMode >> darken [

	^ self setOperator: CAIRO_OPERATOR_DARKEN
]

{ #category : #modes }
AthensCairoPaintMode >> dest [
	"Set 'dest' drawing mode.

	Drawing under this mode will ignore the source, as if nothing drawn"

	^ self setOperator: CAIRO_OPERATOR_DEST
]

{ #category : #modes }
AthensCairoPaintMode >> destAtop [

	^ self setOperator: CAIRO_OPERATOR_DEST_ATOP
]

{ #category : #modes }
AthensCairoPaintMode >> destIn [

	^ self setOperator: CAIRO_OPERATOR_DEST_IN
]

{ #category : #modes }
AthensCairoPaintMode >> destOut [

	^ self setOperator: CAIRO_OPERATOR_DEST_OUT
]

{ #category : #modes }
AthensCairoPaintMode >> destOver [

	^ self setOperator: CAIRO_OPERATOR_DEST_OVER
]

{ #category : #modes }
AthensCairoPaintMode >> difference [

	^ self setOperator: CAIRO_OPERATOR_DIFFERENCE
]

{ #category : #modes }
AthensCairoPaintMode >> exclusion [

	^ self setOperator: CAIRO_OPERATOR_EXCLUSION
]

{ #category : #private }
AthensCairoPaintMode >> getOperator [
	^ self ffiCall: #(
		cairo_operator_t cairo_get_operator (AthensCairoCanvas canvas) )
]

{ #category : #modes }
AthensCairoPaintMode >> hardLight [

	^ self setOperator: CAIRO_OPERATOR_HARD_LIGHT
]

{ #category : #modes }
AthensCairoPaintMode >> hslColor [

	^ self setOperator: CAIRO_OPERATOR_HSL_COLOR
]

{ #category : #modes }
AthensCairoPaintMode >> hslHue [

	^ self setOperator: CAIRO_OPERATOR_HSL_HUE
]

{ #category : #modes }
AthensCairoPaintMode >> hslLuminosity [

	^ self setOperator: CAIRO_OPERATOR_HSL_LUMINOSITY
]

{ #category : #modes }
AthensCairoPaintMode >> hslSaturation [

	^ self setOperator: CAIRO_OPERATOR_HSL_SATURATION
]

{ #category : #modes }
AthensCairoPaintMode >> in [

	^ self setOperator: CAIRO_OPERATOR_IN
]

{ #category : #modes }
AthensCairoPaintMode >> lighten [

	^ self setOperator: CAIRO_OPERATOR_LIGHTEN
]

{ #category : #modes }
AthensCairoPaintMode >> multiply [

	^ self setOperator: CAIRO_OPERATOR_MULTIPLY
]

{ #category : #modes }
AthensCairoPaintMode >> out [

	^ self setOperator: CAIRO_OPERATOR_OUT
]

{ #category : #modes }
AthensCairoPaintMode >> over [
	"Set 'over' drawing mode. This is default paint mode.

	Drawing under this mode will blend source with destination color using source alpha component"

	^ self setOperator: CAIRO_OPERATOR_OVER
]

{ #category : #modes }
AthensCairoPaintMode >> overlay [

	^ self setOperator: CAIRO_OPERATOR_OVERLAY
]

{ #category : #convenience }
AthensCairoPaintMode >> restoreAfter: aBlock [
	| op |
	op := self getOperator.

	aBlock ensure: [ self setOperator: op ]
]

{ #category : #modes }
AthensCairoPaintMode >> saturate [

	^ self setOperator: CAIRO_OPERATOR_SATURATE
]

{ #category : #modes }
AthensCairoPaintMode >> screen [

	^ self setOperator: CAIRO_OPERATOR_SCREEN
]

{ #category : #private }
AthensCairoPaintMode >> setOperator: aCairoOperatorT [
	^ self ffiCall: #(void cairo_set_operator (AthensCairoCanvas canvas, cairo_operator_t aCairoOperatorT) )
]

{ #category : #modes }
AthensCairoPaintMode >> softLight [

	^ self setOperator: CAIRO_OPERATOR_SOFT_LIGHT
]

{ #category : #modes }
AthensCairoPaintMode >> source [
	"Set 'source' drawing mode.

	Drawing under this mode will replace the content with incoming source under the shape boundaries"

	^ self setOperator: CAIRO_OPERATOR_SOURCE
]

{ #category : #modes }
AthensCairoPaintMode >> xor [

	^ self setOperator: CAIRO_OPERATOR_XOR
]
