"
Do not rearrange these fields!

face -- the FT2Face that owns this FT2GlyphSlot.


Note that even when the glyph image is transformed, the metrics are not.

linearHoriAdvance -- For scalable formats only, this field holds the
linearly scaled horizontal advance width for the glyph (i.e. the scaled
and unhinted value of the hori advance).  This can be important to
perform correct WYSIWYG layout.

Note that this value is expressed by default in 16.16 pixels. However,
when the glyph is loaded with the FT_LOAD_LINEAR_DESIGN flag, this field
contains simply the value of the advance in original font units.

linearVertAdvance -- For scalable formats only, this field holds the
linearly scaled vertical advance height for the glyph.  See
linearHoriAdvance for comments.

advance -- This is the transformed advance width for the glyph.

format -- This field indicates the format of the image contained in the
glyph slot.  Typically FT_GLYPH_FORMAT_BITMAP, FT_GLYPH_FORMAT_OUTLINE,
and FT_GLYPH_FORMAT_COMPOSITE, but others are possible.

bitmap -- This field is used as a bitmap descriptor when the slot format
is FT_GLYPH_FORMAT_BITMAP.  Note that the address and content of the
bitmap buffer can change between calls of @FT_Load_Glyph and a few other
functions.

bitmap_left -- This is the bitmap's left bearing expressed in integer
pixels.  Of course, this is only valid if the format is
FT_GLYPH_FORMAT_BITMAP.

bitmap_top -- This is the bitmap's top bearing expressed in integer
pixels.  Remember that this is the distance from the baseline to the
top-most glyph scanline, upwards y-coordinates being *positive*.

outline -- The outline descriptor for the current glyph image if its
format is FT_GLYPH_FORMAT_OUTLINE.

num_subglyphs -- The number of subglyphs in a composite glyph.  This
field is only valid for the composite glyph format that should normally
only be loaded with the @FT_LOAD_NO_RECURSE flag.  For now this is
internal to FreeType.

subglyphs -- An array of subglyph descriptors for composite glyphs.
There are `num_subglyphs' elements in there.  Currently internal to
FreeType.

control_data -- Certain font drivers can also return the control data
for a given glyph image (e.g.  TrueType bytecode, Type 1 charstrings,
etc.).  This field is a pointer to such data.

control_len -- This is the length in bytes of the control data.

other -- Really wicked formats can use this pointer to present their own
glyph image to client apps.  Note that the app will need to know about
the image format.

width, height, hBearingX, hBearingY, hAdvance, vBearingX, vBearingY, vAdvance 
-- The metrics of the last loaded glyph in the slot.  The
returned values depend on the last load flags (see the @FT_Load_Glyph
API function) and can be expressed either in 26.6 fractional pixels or
font units.

"
Class {
	#name : #FT2GlyphSlot,
	#superclass : #Object,
	#instVars : [
		'face',
		'linearHorizontalAdvance',
		'linearVerticalAdvance',
		'advanceX',
		'advanceY',
		'format',
		'bitmapLeft',
		'bitmapTop',
		'width',
		'height',
		'hBearingX',
		'hBearingY',
		'hAdvance',
		'vBearingX',
		'vBearingY',
		'vAdvance',
		'outline'
	],
	#pools : [
		'FT2Constants'
	],
	#category : #FreeType-Base
}

{ #category : #'instance creation' }
FT2GlyphSlot class >> fromFace: anFT2Face [
	^(super new)
		loadFrom: anFT2Face;
		yourself.
]

{ #category : #accessing-convenience }
FT2GlyphSlot >> advance [
	^advanceX@advanceY
]

{ #category : #accessing-fields }
FT2GlyphSlot >> advanceX [
	^advanceX
]

{ #category : #accessing-fields }
FT2GlyphSlot >> advanceY [
	^advanceY
]

{ #category : #accessing-fields }
FT2GlyphSlot >> bitmapLeft [
	^bitmapLeft
]

{ #category : #accessing-fields }
FT2GlyphSlot >> bitmapTop [
	^bitmapTop
]

{ #category : #accessing-convenience }
FT2GlyphSlot >> extent [
	^width@height
]

{ #category : #accessing-fields }
FT2GlyphSlot >> format [
	^format
]

{ #category : #accessing-convenience }
FT2GlyphSlot >> hBearing [
	^hBearingX@hBearingY
]

{ #category : #accessing-fields }
FT2GlyphSlot >> hBearingX [
	^hBearingX
]

{ #category : #accessing-fields }
FT2GlyphSlot >> hBearingY [
	^hBearingY
]

{ #category : #accessing-fields }
FT2GlyphSlot >> height [
	^height
]

{ #category : #accessing-convenience }
FT2GlyphSlot >> linearAdvance [
	^"("(linearHorizontalAdvance @ linearVerticalAdvance) "* 2540) rounded"

]

{ #category : #accessing-fields }
FT2GlyphSlot >> linearHorizontalAdvance [
	^linearHorizontalAdvance
]

{ #category : #accessing-fields }
FT2GlyphSlot >> linearVerticalAdvance [
	^linearVerticalAdvance
]

{ #category : #private }
FT2GlyphSlot >> loadFrom: anFT2Face [
	face := anFT2Face.
	self primLoadFrom: anFT2Face.
	format := ((EndianDetector isLittleEndian) ifTrue: [ format reversed ] ifFalse: [ format ]) asString.
	linearHorizontalAdvance := linearHorizontalAdvance / 65536.0.
	linearVerticalAdvance isZero ifFalse: [ linearVerticalAdvance := linearVerticalAdvance / 65536.0 ].
	advanceX := advanceX bitShift: -6.
	advanceY isZero ifFalse: [ advanceY := advanceY bitShift: -6 ].
	width := width + 63 bitShift: -6. "round up"
	height := height + 63 bitShift: -6. "round up"
	hBearingX := hBearingX bitShift: -6.
	hBearingY := hBearingY bitShift: -6.
	hAdvance := hAdvance bitShift: -6.
	vBearingX := vBearingX bitShift: -6.
	vBearingY := vBearingY bitShift: -6.
	vAdvance := vAdvance bitShift: -6.
]

{ #category : #accessing-fields }
FT2GlyphSlot >> outline [
	^outline
]

{ #category : #accessing-fields }
FT2GlyphSlot >> outline: anOutline [
	outline := anOutline
]

{ #category : #private }
FT2GlyphSlot >> primLoadFrom: anFT2Face [
	<primitive: 'primitiveLoadGlyphSlotFromFace' module: 'FT2Plugin'>
	^self primitiveFailed.
]

{ #category : #accessing-convenience }
FT2GlyphSlot >> roundedPixelLinearAdvance [
	"Answer the scaled linearAdvance, rounded to whole pixels"
	^linearHorizontalAdvance  rounded @ linearVerticalAdvance rounded

]

{ #category : #accessing-fields }
FT2GlyphSlot >> width [
	^width
]
