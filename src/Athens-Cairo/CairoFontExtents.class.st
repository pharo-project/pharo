"
I represent the
cairo_font_extents_t structure type

The cairo_font_extents_t structure stores metric information for a font. Values are given in the current user-space coordinate system.

Because font metrics are in user-space coordinates, they are mostly, but not entirely, independent of the current transformation matrix. If you call cairo_scale(cr, 2.0, 2.0), text will be drawn twice as big, but the reported text extents will not be doubled. They will change slightly due to hinting (so you can't assume that metrics are independent of the transformation matrix), but otherwise will remain unchanged.

double ascent;
	the distance that the font extends above the baseline. Note that this is not always exactly equal to the maximum of the extents of all the glyphs in the font, but rather is picked to express the font designer's intent as to how the font should align with elements above it.

double descent;
	the distance that the font extends below the baseline. This value is positive for typical fonts that include portions below the baseline. Note that this is not always exactly equal to the maximum of the extents of all the glyphs in the font, but rather is picked to express the font designer's intent as to how the font should align with elements below it.

double height;
	the recommended vertical distance between baselines when setting consecutive lines of text with the font. This is greater than ascent+descent by a quantity known as the line spacing or external leading. When space is at a premium, most fonts can be set with only a distance of ascent+descent between lines.

double max_x_advance;
	the maximum distance in the X direction that the origin is advanced for any glyph in the font.

double max_y_advance;
	the maximum distance in the Y direction that the origin is advanced for any glyph in the font. This will be zero for normal fonts used for horizontal writing. (The scripts of East Asia are sometimes written vertically.)
"
Class {
	#name : #CairoFontExtents,
	#superclass : #FFIExternalStructure,
	#classVars : [
		'OFFSET_ASCENT',
		'OFFSET_DESCENT',
		'OFFSET_HEIGHT',
		'OFFSET_MAX_X_ADVANCE',
		'OFFSET_MAX_Y_ADVANCE',
		'OFFSET_WIDTH'
	],
	#category : #'Athens-Cairo-Text'
}

{ #category : #'field definition' }
CairoFontExtents class >> fieldsDesc [
	"self rebuildFieldAccessors"
	^ #(
	double ascent;
	double descent;
	double width;
	double height;
	double max_x_advance;
	double max_y_advance;
	)
]

{ #category : #'accessing - structure variables' }
CairoFontExtents >> ascent [
	"This method was automatically generated"
	^handle doubleAt: OFFSET_ASCENT
]

{ #category : #'accessing - structure variables' }
CairoFontExtents >> ascent: anObject [
	"This method was automatically generated"
	handle doubleAt: OFFSET_ASCENT put: anObject
]

{ #category : #'accessing - structure variables' }
CairoFontExtents >> descent [
	"This method was automatically generated"
	^handle doubleAt: OFFSET_DESCENT
]

{ #category : #'accessing - structure variables' }
CairoFontExtents >> descent: anObject [
	"This method was automatically generated"
	handle doubleAt: OFFSET_DESCENT put: anObject
]

{ #category : #'accessing - structure variables' }
CairoFontExtents >> height [
	"This method was automatically generated"
	^handle doubleAt: OFFSET_HEIGHT
]

{ #category : #'accessing - structure variables' }
CairoFontExtents >> height: anObject [
	"This method was automatically generated"
	handle doubleAt: OFFSET_HEIGHT put: anObject
]

{ #category : #'accessing - structure variables' }
CairoFontExtents >> max_x_advance [
	"This method was automatically generated"
	^handle doubleAt: OFFSET_MAX_X_ADVANCE
]

{ #category : #'accessing - structure variables' }
CairoFontExtents >> max_x_advance: anObject [
	"This method was automatically generated"
	handle doubleAt: OFFSET_MAX_X_ADVANCE put: anObject
]

{ #category : #'accessing - structure variables' }
CairoFontExtents >> max_y_advance [
	"This method was automatically generated"
	^handle doubleAt: OFFSET_MAX_Y_ADVANCE
]

{ #category : #'accessing - structure variables' }
CairoFontExtents >> max_y_advance: anObject [
	"This method was automatically generated"
	handle doubleAt: OFFSET_MAX_Y_ADVANCE put: anObject
]

{ #category : #'accessing - structure variables' }
CairoFontExtents >> width [
	"This method was automatically generated"
	^handle doubleAt: OFFSET_WIDTH
]

{ #category : #'accessing - structure variables' }
CairoFontExtents >> width: anObject [
	"This method was automatically generated"
	handle doubleAt: OFFSET_WIDTH put: anObject
]
