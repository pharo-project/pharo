"
This class produces glyphs for a FreeTypeFont.
It can be subclassed to provide, for example, sub-pixel anti-aliased glyphs.
"
Class {
	#name : #FreeTypeGlyphRenderer,
	#superclass : #Object,
	#pools : [
		'FT2Constants'
	],
	#classInstVars : [
		'current'
	],
	#category : 'FreeType-GlyphRendering'
}

{ #category : #'instance creation' }
FreeTypeGlyphRenderer class >> current [
	"
	FreeTypeGlyphRenderer current
	"
	^current ifNil:[current := self new]
]

{ #category : #accessing }
FreeTypeGlyphRenderer class >> current: aKindOfFreeTypeGlyphRender [
	current := aKindOfFreeTypeGlyphRender
]

{ #category : #private }
FreeTypeGlyphRenderer >> convert8To32: aGlyphForm [
		"convert aGlyphForm from the 8 bit deep form produced by FreeType, where each byte represents the intensity of a single pixel, to a 32 bit deep form" 
	| w h s answer rowstart bytes word littleEndian shift v a colorVal |

	bytes := aGlyphForm bits.
	w := aGlyphForm width.
	h := aGlyphForm height.
	answer := aGlyphForm class extent: w@h depth: 32.
	answer 
		offset: (aGlyphForm offset x) @(aGlyphForm offset y);
		advance: aGlyphForm advance;
		linearAdvance: aGlyphForm linearAdvance.
	s := w + 3 >> 2.
	littleEndian := aGlyphForm isLittleEndian.
	0 to: h - 1 do: [:y |
		rowstart := (y * s)+1.
		0 to: w - 1 do:[:x | 
			word := bytes at: rowstart + (x//4).
			shift := 8* (littleEndian 
				ifTrue:[x bitAnd: 3] 
				ifFalse:[3-(x bitAnd: 3)]).
			v := word >>shift bitAnd: 16rFF.
			a := v > 0 ifTrue:[16rFF] ifFalse:[0].
			colorVal := v + (v bitShift: 8) +  (v bitShift: 16) + (a bitShift: 24).
			answer bits integerAt: (y*w)+(x+1) put: colorVal]].
	^answer
]

{ #category : #private }
FreeTypeGlyphRenderer >> convert8to32: aGlyphForm colorValue: foreColorValue [
	"convert from the 8 bit deep form produced by FreeType, where each byte represents the intensity of a single pixel, to a 32 bit deep form with pixels of color foreColorValue " 
	| w h s answer rowstart bytes word littleEndian shift v a colorVal foreColorVal foreColorA foreColorR foreColorG foreColorB r g b |

	foreColorVal := foreColorValue.
	foreColorA := foreColorVal >> 24.
	foreColorR := foreColorVal >> 16 bitAnd: 16rFF.
	foreColorG := foreColorVal >> 8 bitAnd: 16rFF.
	foreColorB := foreColorVal bitAnd: 16rFF.
	bytes := aGlyphForm bits.
	w := aGlyphForm width.
	h := aGlyphForm height.
	answer := aGlyphForm class extent: w@h depth: 32.
	answer 
		offset: (aGlyphForm offset x) @ (aGlyphForm offset y);
		advance: aGlyphForm advance;
		linearAdvance: aGlyphForm linearAdvance.
	s := w + 3 >> 2.
	littleEndian := aGlyphForm isLittleEndian.
	0 to: h - 1 do: [:y |
		rowstart := (y * s)+1.
		0 to: w - 1 do:[:x | 
			word := bytes at: rowstart + (x//4).
			shift := 8* (littleEndian 
				ifTrue:[x bitAnd: 3] 
				ifFalse:[3-(x bitAnd: 3)]).
			v := word >>shift bitAnd: 16rFF.
			a := v > 0 ifTrue:[v * foreColorA // 16rFF] ifFalse:[0].
			r := v > 0 ifTrue:[a * foreColorR // 16rFF] ifFalse:[0].
			g := v > 0 ifTrue:[a * foreColorG // 16rFF] ifFalse:[0].
			b := v > 0 ifTrue:[a * foreColorB // 16rFF] ifFalse:[0].
			colorVal := (a bitShift: 24) + (r bitShift: 16) + (g bitShift: 8) + b.
			answer bits integerAt: (y*w)+(x+1) put: colorVal]].
	^answer
]

{ #category : #private }
FreeTypeGlyphRenderer >> fixBytesForMono: aGlyphForm [
	"On Windows, the bits in each byte are in reverse order, and inverted.
	i.e. 2r10100000 should be 2r11111010  to display correctly.
	This needs further investigation"
	| b newB bits |
	bits := aGlyphForm bits.
	1 to: bits byteSize do:[:i |
		b := bits byteAt: i.
		newB := ((((((((b bitAnd: 2r10000000) bitShift: -7)
			bitOr: ((b bitAnd: 2r1000000) bitShift: -5))
			bitOr: ((b bitAnd: 2r100000) bitShift: -3))
			bitOr: ((b bitAnd: 2r10000) bitShift: -1))
			bitOr: ((b bitAnd: 2r1000) bitShift: 1))
			bitOr: ((b bitAnd: 2r100) bitShift: 3))
			bitOr: ((b bitAnd: 2r10) bitShift: 5))
			bitOr: ((b bitAnd: 2r1) bitShift: 7).
		bits byteAt: i put: (newB bitXor: 2r11111111)].
	^aGlyphForm
]

{ #category : #public }
FreeTypeGlyphRenderer >> glyphOf: aCharacter colorValue: aColorValue mono: monoBoolean subpixelPosition: sub font: aFreeTypeFont [

	| f |
	f := self 
		renderGlyph: aCharacter 
		depth: (monoBoolean ifTrue:[1] ifFalse:[8]) 
		subpixelPosition: sub
		font: aFreeTypeFont.
	monoBoolean 
		ifTrue:[
			f := self fixBytesForMono: f.
			f := f asFormOfDepth: 8].
	f := self convert8to32: f colorValue: aColorValue.
	^f
	
]

{ #category : #public }
FreeTypeGlyphRenderer >> mode41GlyphOf: aCharacter colorValue: aColorValue mono: monoBoolean subpixelPosition: sub font: aFreeTypeFont [

	| f |
	f := self 
		renderGlyph: aCharacter 
		depth: (monoBoolean ifTrue:[1] ifFalse:[8]) 
		subpixelPosition: sub 
		font: aFreeTypeFont.
	monoBoolean 
		ifTrue:[
			f := self fixBytesForMono: f.
			f := f asFormOfDepth: 32]
		ifFalse:[
			f := self convert8To32: f].
	^f
]

{ #category : #private }
FreeTypeGlyphRenderer >> renderGlyph: aCharacter depth: depth subpixelPosition: sub font: aFreeTypeFont [
	"Glyphs are either 1 or 8 bit deep. For 32 bpp we use 8 bits, otherwise 1"
	| em form glyph charCode slant extraWidth extraHeight boldExtra offsetX offsetY s synthBoldStrength face |

	charCode := aCharacter asUnicode asInteger.
	(aFreeTypeFont face charmaps includes:'unic')
		ifTrue:[
			(aFreeTypeFont isSymbolFont and: [ charCode between: 16r20 and: 16rFF ])
				ifTrue:[charCode := charCode + 16rF000]]
		ifFalse:[
			(aFreeTypeFont face charmaps includes:'armn')
				ifTrue:[ "select apple roman char map, and map character from unicode to mac encoding"
					aFreeTypeFont face setCharMap:'armn'.
					charCode := aCharacter unicodeToMacRoman asUnicode asInteger. "check this!"]].
	aCharacter < $  ifTrue: ["charCode := $  asUnicode asInteger"
		^(GlyphForm extent: 0@0 depth: depth)
			advance: 0@0;
			linearAdvance: 0@0;
			offset:0@0;
			yourself ].
	em := aFreeTypeFont pixelSize.
	[ | hintingFlags flags |face := aFreeTypeFont face.
	face setPixelWidth: em height: em.
	hintingFlags := FreeTypeSettings current hintingFlags.
	flags :=  LoadNoBitmap bitOr:( LoadIgnoreTransform bitOr: hintingFlags). 
	face loadCharacter:charCode flags: flags]
	on: FT2Error do:[:e | 
		^(GlyphForm extent: 0@0 depth: depth)
			advance: 0@0;
			linearAdvance: 0@0;
			offset:0@0;
			yourself]. 
	glyph := face glyph. 
	slant := aFreeTypeFont simulatedItalicSlant.
	extraWidth := (glyph height * slant) abs ceiling.
	synthBoldStrength := aFreeTypeFont simulatedBoldStrength.
	boldExtra := 4 * synthBoldStrength abs ceiling.
	extraWidth := extraWidth + boldExtra.
	sub > 0 ifTrue:[ extraWidth := extraWidth + 1].
	extraHeight := boldExtra.
	form := GlyphForm extent: (glyph width + extraWidth + 1)@(glyph height + extraHeight+ 1) depth: depth.
	s := (glyph height-glyph hBearingY)  * slant.
	s := s sign * (s abs ceiling). 
	offsetX := glyph hBearingX negated + s + (boldExtra // 2) .
	offsetY := glyph height - glyph hBearingY + (boldExtra//2).
	synthBoldStrength ~= 0
		ifTrue:[face emboldenOutline: synthBoldStrength].
	face transformOutlineAngle: 0 scalePoint: 1@1  slant: slant.
	face translateOutlineBy: (offsetX+(sub/64))@offsetY.
	face renderGlyphIntoForm: form.
	form offset: (glyph hBearingX - s - (boldExtra // 2) ) @ (glyph hBearingY + 1 + (boldExtra / 2) ceiling  ) negated.
	"When not hinting FreeType sets the advance to the truncated linearAdvance.
	The characters appear squashed together. Rounding is probably better, so we fix the advance here"
	aFreeTypeFont subPixelPositioned
		ifTrue:[ form advance: glyph roundedPixelLinearAdvance]
		ifFalse:[ form advance: glyph advance].
	form linearAdvance: glyph linearAdvance.
	^form
]

{ #category : #public }
FreeTypeGlyphRenderer >> subGlyphOf: aCharacter colorValue: aColorValue mono: monoBoolean subpixelPosition: sub font: aFreeTypeFont [
	"the default renderer does not support sub-pixel anti-aliasing,
	so answer an ordinary glyph"
	^self mode41GlyphOf: aCharacter colorValue: aColorValue mono: monoBoolean subpixelPosition: sub font: aFreeTypeFont

]
