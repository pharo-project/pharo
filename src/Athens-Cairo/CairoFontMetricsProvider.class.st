"
Provide font metrics (Cairo)
"
Class {
	#name : #CairoFontMetricsProvider,
	#superclass : #Object,
	#traits : 'TCairoLibrary',
	#classTraits : 'TCairoLibrary classTrait',
	#instVars : [
		'font',
		'utfConverter',
		'cairoFont',
		'extents',
		'glyphExtents',
		'cache'
	],
	#pools : [
		'AthensCairoDefinitions'
	],
	#category : #'Athens-Cairo-Text'
}

{ #category : #private }
CairoFontMetricsProvider >> applyKerningOn: aGlyphs of: aString from: start to: end [
	|kerning  previous|
	kerning := 0.
	previous := nil.
	aGlyphs doWithIndex: [ :glyph :index | |current|
		current := aString at: start + (index - 1).
		index > 1 ifTrue: [
			kerning := kerning + (font kerningLeft: previous right: current).
			glyph x: glyph x + kerning ].
		previous := current ]
]

{ #category : #accessing }
CairoFontMetricsProvider >> cairoFont [
	^ cairoFont
]

{ #category : #private }
CairoFontMetricsProvider >> convertString: utf8String len: strlen ofFont: aScaledFont toGlyphs: glyphs numGlyphs: numGlyphs x: x y: y [
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
CairoFontMetricsProvider >> extentsOf: aString [
	|glyphs|
	glyphs := self glyphsOf: aString.
   cairoFont getExtentsOfGlyphs: glyphs getHandle ofLength: glyphs size into: glyphExtents.
	^ glyphExtents
]

{ #category : #accessing }
CairoFontMetricsProvider >> font: aFont [
	font := aFont asFreetypeFont.
	cairoFont :=	CairoScaledFont fromFreetypeFont: font.
	extents := cairoFont extents
]

{ #category : #accessing }
CairoFontMetricsProvider >> fontAscent [
	^ extents ascent
]

{ #category : #accessing }
CairoFontMetricsProvider >> fontHeight [
	^ extents height
]

{ #category : #private }
CairoFontMetricsProvider >> freeGlyphs: glyphs [
	^ self ffiCall: #( void cairo_glyph_free (void *glyphs))
]

{ #category : #accessing }
CairoFontMetricsProvider >> getGlyphWidth: aCharacter [
	cairoFont getExtentsOf: (utfConverter convertChar: aCharacter) into: glyphExtents.
	^ glyphExtents x_advance
]

{ #category : #private }
CairoFontMetricsProvider >> glyphsOf: aString [
	^ self glyphsOf: aString from: 1 to: aString size
]

{ #category : #private }
CairoFontMetricsProvider >> glyphsOf: aString from: start to: end [
	| len ptr glyphs lenValue glyphsSize utf8Len error |

	len := end-start+1.
	utf8Len := utfConverter convert: aString from: start to: end.

	ptr := ExternalAddress new.
	lenValue := ByteArray new: 4.
	lenValue unsignedLongAt: 1 put: len.

	error := self
		convertString: utfConverter buffer
		len: utf8Len
		ofFont: cairoFont
		toGlyphs: ptr
		numGlyphs: lenValue
		x: 0.0
		y: 0.0.

	error = CAIRO_STATUS_SUCCESS ifFalse: [  ^ CairoGlyphsArray new: 0 ].

	glyphsSize := lenValue unsignedLongAt: 1.
   cairoFont getExtentsOfGlyphs: ptr ofLength: glyphsSize into: glyphExtents.

	glyphs := CairoGlyphsArray new: glyphsSize.
	LibC memCopy: ptr to: glyphs getHandle size: (glyphsSize * glyphs typeSize).
	"Apply kerning on glyphs if font supports it"
	font face hasKerning
		ifTrue: [ self applyKerningOn: glyphs of: aString from: start to: end ].
	self freeGlyphs: ptr.

	^ glyphs
]

{ #category : #initialization }
CairoFontMetricsProvider >> initialize [
	utfConverter := CairoUTF8Converter new.
	glyphExtents := CairoTextExtents new.
	cache := CairoBackendCache soleInstance
]
