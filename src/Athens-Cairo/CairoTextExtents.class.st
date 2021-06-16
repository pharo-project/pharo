"
I represent the `cairo_text_extents_t` structure type.

The `cairo_text_extents_t` structure stores the extents of a single glyph or a string of glyphs in user-space coordinates. Because text extents are in user-space coordinates, they are mostly, but not entirely, independent of the current transformation matrix. If you call `cairo_scale(cr, 2.0, 2.0)`, text will be drawn twice as big, but the reported text extents will not be doubled. They will change slightly due to hinting (so you can't assume that metrics are independent of the transformation matrix), but otherwise will remain unchanged.

- `double x_bearing` the horizontal distance from the origin to the leftmost part of the glyphs as drawn. Positive if the glyphs lie entirely to the right of the origin.
- `double y_bearing` the vertical distance from the origin to the topmost part of the glyphs as drawn. Positive only if the glyphs lie completely below the origin; will usually be negative.
- `double width`	width of the glyphs as drawn
- `double height`height of the glyphs as drawn
- `double x_advance` distance to advance in the X direction after drawing these glyphs
- `double y_advance` distance to advance in the Y direction after drawing these glyphs. Will typically be zero except for vertical text layout as found in East-Asian languages.
"
Class {
	#name : #CairoTextExtents,
	#superclass : #FFIExternalStructure,
	#classVars : [
		'OFFSET_HEIGHT',
		'OFFSET_WIDTH',
		'OFFSET_X_ADVANCE',
		'OFFSET_X_BEARING',
		'OFFSET_Y_ADVANCE',
		'OFFSET_Y_BEARING'
	],
	#category : #'Athens-Cairo-Text'
}

{ #category : #'field definition' }
CairoTextExtents class >> fieldsDesc [
	"self rebuildFieldAccessors"
	^ #(
	double x_bearing;
	double y_bearing;
	double width;
	double height;
	double x_advance;
	double y_advance;
	)
]

{ #category : #'accessing - structure variables' }
CairoTextExtents >> height [
	"This method was automatically generated"
	^handle doubleAt: OFFSET_HEIGHT
]

{ #category : #'accessing - structure variables' }
CairoTextExtents >> height: anObject [
	"This method was automatically generated"
	handle doubleAt: OFFSET_HEIGHT put: anObject
]

{ #category : #'accessing - structure variables' }
CairoTextExtents >> width [
	"This method was automatically generated"
	^handle doubleAt: OFFSET_WIDTH
]

{ #category : #'accessing - structure variables' }
CairoTextExtents >> width: anObject [
	"This method was automatically generated"
	handle doubleAt: OFFSET_WIDTH put: anObject
]

{ #category : #'accessing - structure variables' }
CairoTextExtents >> x_advance [
	"This method was automatically generated"
	^handle doubleAt: OFFSET_X_ADVANCE
]

{ #category : #'accessing - structure variables' }
CairoTextExtents >> x_advance: anObject [
	"This method was automatically generated"
	handle doubleAt: OFFSET_X_ADVANCE put: anObject
]

{ #category : #'accessing - structure variables' }
CairoTextExtents >> x_bearing [
	"This method was automatically generated"
	^handle doubleAt: OFFSET_X_BEARING
]

{ #category : #'accessing - structure variables' }
CairoTextExtents >> x_bearing: anObject [
	"This method was automatically generated"
	handle doubleAt: OFFSET_X_BEARING put: anObject
]

{ #category : #'accessing - structure variables' }
CairoTextExtents >> y_advance [
	"This method was automatically generated"
	^handle doubleAt: OFFSET_Y_ADVANCE
]

{ #category : #'accessing - structure variables' }
CairoTextExtents >> y_advance: anObject [
	"This method was automatically generated"
	handle doubleAt: OFFSET_Y_ADVANCE put: anObject
]

{ #category : #'accessing - structure variables' }
CairoTextExtents >> y_bearing [
	"This method was automatically generated"
	^handle doubleAt: OFFSET_Y_BEARING
]

{ #category : #'accessing - structure variables' }
CairoTextExtents >> y_bearing: anObject [
	"This method was automatically generated"
	handle doubleAt: OFFSET_Y_BEARING put: anObject
]
