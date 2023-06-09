"
i represent stroke paint in cairo backend
"
Class {
	#name : #AthensCairoStrokePaint,
	#superclass : #AthensStrokePaint,
	#traits : 'TCairoLibrary',
	#classTraits : 'TCairoLibrary classTrait',
	#instVars : [
		'capStyle',
		'joinStyle',
		'dashLenghts',
		'dashOffset'
	],
	#pools : [
		'AthensCairoDefinitions'
	],
	#category : #'Athens-Cairo-Paints'
}

{ #category : #drawing }
AthensCairoStrokePaint >> athensFillPath: aPath on: aCanvas [

	self prepareForDrawingOn: aCanvas.

	aCanvas
		newPath;
		loadPath: aPath;
		stroke
]

{ #category : #drawing }
AthensCairoStrokePaint >> athensFillRectangle: aRect on: aCanvas [

	self prepareForDrawingOn: aCanvas.

	aCanvas
		newPath;
		rectangleX: aRect left
		y: aRect top
		width: aRect width
		height: aRect height;
		stroke
]

{ #category : #'setting - cap styles' }
AthensCairoStrokePaint >> capButt [
	capStyle := CAIRO_LINE_CAP_BUTT
]

{ #category : #'setting - cap styles' }
AthensCairoStrokePaint >> capRound [
	capStyle := CAIRO_LINE_CAP_ROUND
]

{ #category : #'setting - cap styles' }
AthensCairoStrokePaint >> capSquare [
	capStyle := CAIRO_LINE_CAP_SQUARE
]

{ #category : #'setting - dashes' }
AthensCairoStrokePaint >> dashes: anAlternateCollectionOfLenghts offset: anOffset [
	dashLenghts := anAlternateCollectionOfLenghts.
	dashOffset := anOffset asFloat
]

{ #category : #'setting - join styles' }
AthensCairoStrokePaint >> joinBevel [
	joinStyle := CAIRO_LINE_JOIN_BEVEL
]

{ #category : #'setting - join styles' }
AthensCairoStrokePaint >> joinMiter [
	joinStyle := CAIRO_LINE_JOIN_MITER
]

{ #category : #'setting - join styles' }
AthensCairoStrokePaint >> joinRound [
	joinStyle := CAIRO_LINE_JOIN_ROUND
]

{ #category : #private }
AthensCairoStrokePaint >> prepareForDrawingOn: aCanvas [

	fillPaint
		loadOnCairoCanvas: aCanvas.

	aCanvas
		primSetLineWidth: width asFloat;
		primSetLineJoin: joinStyle;
		primSetLineCap: capStyle.

	dashLenghts
		ifNil: [ aCanvas resetDash ]
		ifNotNil: [ self setDashesOn: aCanvas ]
]

{ #category : #private }
AthensCairoStrokePaint >> primSetDashesOn: aCanvas lengths: dashesLengths count: dashesCount offset: anOffset [
"	void                cairo_set_dash                      (cairo_t *cr,
                                                         double *dashes,
                                                         int num_dashes,
                                                         double offset);"

	^ self ffiCall: #(void cairo_set_dash (
				AthensCairoCanvas aCanvas,
				double* dashesLengths,
				int dashesCount,
				double anOffset) )
]

{ #category : #private }
AthensCairoStrokePaint >> primSetLineCapOn: aCanvas [
	^ self ffiCall: #(void cairo_set_line_cap (
			AthensCairoCanvas aCanvas,
			cairo_line_cap_t capStyle) )
]

{ #category : #private }
AthensCairoStrokePaint >> primSetLineJoinOn: aCanvas [
	^ self ffiCall: #(void cairo_set_line_join (
			AthensCairoCanvas aCanvas,
			cairo_line_join_t joinStyle) )
]

{ #category : #private }
AthensCairoStrokePaint >> primSetLineWidthOn: aCanvas [
	^ self ffiCall: #(void cairo_set_line_width (
				AthensCairoCanvas aCanvas,
				double width) )
]

{ #category : #private }
AthensCairoStrokePaint >> setDashesOn: aCanvas [
	| buf |

	buf := ByteArray new: 8*dashLenghts size.

	dashLenghts withIndexDo: [:len :i |

		buf doubleAt: (i-1)*8 + 1 put: len asFloat
		].

	aCanvas
		primSetDashesLengths: buf
		count: dashLenghts size
		offset: dashOffset
]
