"
I am used to render freetype font(s) with cairo library.

Before start rendering, you must set:
 - canvas
 - font

"
Class {
	#name : #CairoFreetypeFontRenderer,
	#superclass : #Object,
	#traits : 'TCairoLibrary',
	#classTraits : 'TCairoLibrary classTrait',
	#instVars : [
		'font',
		'utfConverter',
		'canvas',
		'advance',
		'originalFont',
		'fontExtents',
		'glyphExtents'
	],
	#pools : [
		'AthensCairoDefinitions'
	],
	#category : #'Athens-Cairo-Text'
}

{ #category : #accessing }
CairoFreetypeFontRenderer >> advance [
	^ advance
]

{ #category : #accessing }
CairoFreetypeFontRenderer >> advance: aPoint [
	advance := aPoint
]

{ #category : #private }
CairoFreetypeFontRenderer >> applyKerningOn: aGlyphs of: aString from: start to: end [
	"Apply kerning on a glyphs that represent aString from
	start to end"
	|kerning  previous|
	kerning := 0.
	previous := nil.
	aGlyphs doWithIndex: [ :glyph :index | |current|
		current := aString at: start + (index - 1).
		index > 1 ifTrue: [
			kerning := kerning + (originalFont kerningLeft: previous right: current).
			glyph x: glyph x + kerning ].
		previous := current ]
]

{ #category : #accessing }
CairoFreetypeFontRenderer >> canvas: aCairoCanvas [

	canvas := aCairoCanvas
]

{ #category : #private }
CairoFreetypeFontRenderer >> convertString: utf8String len: strlen ofFont: aScaledFont toGlyphs: glyphs numGlyphs: numGlyphs x: x y: y [
"
all of this for using
http://www.cairographics.org/manual/cairo-User-Fonts.html#cairo-user-scaled-font-text-to-glyphs-func-t

"
	^ self ffiCall: #(
		cairo_status_t cairo_scaled_font_text_to_glyphs (CairoScaledFont aScaledFont,
			double x,
			double y,
			void * utf8String,
			int strlen,
			void ** glyphs,
			int * numGlyphs,
			NULL,
			NULL,
			NULL))
]

{ #category : #accessing }
CairoFreetypeFontRenderer >> font: aFreetypeFont [

	font := CairoScaledFont fromFreetypeFont: aFreetypeFont.
	fontExtents := font extents.
	originalFont := aFreetypeFont
]

{ #category : #'accessing - font metrics' }
CairoFreetypeFontRenderer >> fontAscent [
	^ fontExtents ascent
]

{ #category : #'accessing - font metrics' }
CairoFreetypeFontRenderer >> fontHeight [
	^ fontExtents height
]

{ #category : #private }
CairoFreetypeFontRenderer >> freeGlyphs: glyphs [
	^ self ffiCall: #( void cairo_glyph_free (void *glyphs))
]

{ #category : #'accessing - font metrics' }
CairoFreetypeFontRenderer >> getGlyphWidth: aCharacter [
	utfConverter convert: aCharacter asString from: 1 to: 1.
	font getExtentsOf: utfConverter buffer into: glyphExtents.
	^ glyphExtents x_advance
]

{ #category : #'accessing - font metrics' }
CairoFreetypeFontRenderer >> glyphsOf: aString from: start to: end [
	| len ptr glyphs lenValue glyphsSize utf8Len error |

	len := end-start+1.
	utf8Len := utfConverter convert: aString from: start to: end.

	ptr := ExternalAddress new.
	lenValue := ByteArray new: 4.
	lenValue unsignedLongAt: 1 put: len.

	"somehow scaled font is buggy and calculated glyphs have wrong extents,
	setting font size manually and retrieving 'fixed' scaled font solves the problem"
	canvas
		setScaledFont: font;
		setFontSize: originalFont pixelSize.

	error := self
		convertString: utfConverter buffer
		len: utf8Len
		ofFont: canvas getScaledFont
		toGlyphs: ptr
		numGlyphs: lenValue
		x: 0.0
		y: 0.0.

	error = CAIRO_STATUS_SUCCESS ifFalse: [  ^ CairoGlyphsArray new: 0 ].

	glyphsSize := lenValue unsignedLongAt: 1.
   font getExtentsOfGlyphs: ptr ofLength: glyphsSize into: glyphExtents.
	advance := glyphExtents width @ glyphExtents height.

	glyphs := CairoGlyphsArray new: glyphsSize.
	LibC memCopy: ptr to: glyphs getHandle size: (glyphsSize * glyphs typeSize).
	"Apply kerning on glyphs if font supports it"
	originalFont face hasKerning
		ifTrue: [ self applyKerningOn: glyphs of: aString from: start to: end ].
	self freeGlyphs: ptr.

	^ glyphs
]

{ #category : #initialization }
CairoFreetypeFontRenderer >> initialize [
	utfConverter := CairoUTF8Converter new.
	advance := 0@0.
	glyphExtents := CairoTextExtents new
]

{ #category : #rendering }
CairoFreetypeFontRenderer >> render: aString [

	^ self renderCharacters: aString from: 1 to: aString size
]

{ #category : #rendering }
CairoFreetypeFontRenderer >> renderCharacters: aString from: start to: end [
	| glyphs |

	glyphs := self glyphsOf: aString from: start to: end.
	self renderGlyphs: glyphs.
	^ self advance
]

{ #category : #rendering }
CairoFreetypeFontRenderer >> renderGlyphs: cairoGlyphs [

	font lock.
	canvas
		setPathMatrix;
		setScaledFont: font.
	canvas paint loadOnCairoCanvas: canvas.

	canvas showGlyphs: cairoGlyphs getHandle size: cairoGlyphs size.
	font unlock
]

{ #category : #accessing }
CairoFreetypeFontRenderer >> setColor: aColor [
	(canvas setPaint: aColor )
]
