"
A SubPixel-AntiAliased GlyphRenderer for FreeType
"
Class {
	#name : #FreeTypeSubPixelAntiAliasedGlyphRenderer,
	#superclass : #FreeTypeGlyphRenderer,
	#pools : [
		'FT2Constants'
	],
	#category : #'FreeType-GlyphRendering'
}

{ #category : #'class initialization' }
FreeTypeSubPixelAntiAliasedGlyphRenderer class >> initialize [
	"
	self initialize
	"
	FreeTypeGlyphRenderer current: self new.

]

{ #category : #rendering }
FreeTypeSubPixelAntiAliasedGlyphRenderer >> filter: aGlyphForm [
	"aGlyphForm should be 3x stretched 8 bit GlyphForm"
	| w h s answer rowstart bytes word littleEndian shift v a colorVal i 
	  prevG prevB r g b nextR nextG  filters rfilter gfilter bfilter  
	balR balG balB |
	
	"correctionFactor := 0.0 ."
	filters := FreeTypeSettings current subPixelFilters.
	rfilter := filters at: 1.
	gfilter := filters at: 2.
	bfilter := filters at: 3.
	bytes := aGlyphForm bits.
	w := aGlyphForm width.
	h := aGlyphForm height.
	answer := aGlyphForm class extent: ((aGlyphForm width / 3) ceiling + 2)@h depth: 32.
	answer 
		offset: (aGlyphForm offset x / 3) rounded@(aGlyphForm offset y);
		advance: (aGlyphForm advance / 3) rounded;
		linearAdvance: aGlyphForm linearAdvance.	
	s := w + 3 >> 2.
	littleEndian := aGlyphForm isLittleEndian.
	0 to: h - 1 do: [:y |
		rowstart := (y * s)+1.
		prevG := prevB :=0.
		0 to: w - 1 by: 3 do:[:x | 
			0 to: 2 do:[:subpixelindex |
				i := x + subpixelindex.
				word := bytes at: rowstart + (i//4).
				shift := -8* (littleEndian 
					ifTrue:[i bitAnd: 3] 
					ifFalse:[3-(i bitAnd: 3)]).
				v := (word bitShift: shift) bitAnd: 16rFF.
				subpixelindex = 0 ifTrue:[r := v].
				subpixelindex = 1 ifTrue:[g := v].
				subpixelindex = 2 ifTrue:[b := v]].
			x >= (w-3)
				ifTrue:[nextR := nextG := 0]
				ifFalse:[
					0 to: 1 do:[:subpixelindex |
						i := x + 3 + subpixelindex.
						word := bytes at: rowstart + (i//4).
						shift := -8* (littleEndian 
							ifTrue:[i bitAnd: 3] 
							ifFalse:[3-(i bitAnd: 3)]).
						v := (word bitShift: shift) bitAnd: 16rFF.
						subpixelindex = 0 ifTrue:[nextR := v].
						subpixelindex = 1 ifTrue:[nextG := v]]].
			"balance r g b"	
			balR := (prevG*(rfilter at: 1))+
				(prevB*(rfilter at: 2))+
				(r*(rfilter at: 3))+
				(g*(rfilter at: 4))+
				(b*(rfilter at: 5)).
			balG := (prevB*(gfilter at: 1))+
				(r*(gfilter at: 2))+
				(g*(gfilter at: 3))+
				(b*(gfilter at: 4))+
				(nextR*(gfilter at: 5)).
			balB := (r*(bfilter at: 1))+
				(g*(bfilter at: 2))+
				(b*(bfilter at: 3))+
				(nextR*(bfilter at: 4))+
				(nextG*(bfilter at: 5)).
			"luminance := (0.299*balR)+(0.587*balG)+(0.114*balB).
			balR := balR + ((luminance - balR)*correctionFactor).
			balG := balG + ((luminance - balG)*correctionFactor).
			balB := balB + ((luminance - balB)*correctionFactor)."
			balR := balR  truncated.
			balR < 0 ifTrue:[balR := 0] ifFalse:[balR > 255 ifTrue:[balR := 255]].	
			balG := balG  truncated.
			balG < 0 ifTrue:[balG := 0] ifFalse:[balG > 255 ifTrue:[balG := 255]].		
			balB := balB  truncated.
			balB < 0 ifTrue:[balB := 0] ifFalse:[balB > 255 ifTrue:[balB := 255]].	 
			a := balR + balG + balB > 0 ifTrue:[16rFF] ifFalse:[0].
			colorVal := balB + (balG bitShift: 8) +  (balR bitShift: 16) + (a bitShift: 24).
			answer bits integerAt: (y*answer width)+(x//3+1) put: colorVal.
			prevB := b. prevG := g.  "remember the unbalanced values" ]].
	^answer
]

{ #category : #rendering }
FreeTypeSubPixelAntiAliasedGlyphRenderer >> renderStretchedGlyph: aCharacter depth: depth subpixelPosition: sub font: aFreeTypeFont [
	"Glyphs are either 1 or 8 bit deep. For 32 bpp we use 8 bits, otherwise 1"
	| em form glyph scaleX charCode slant extraWidth s offsetX offsetY synthBoldStrength boldExtra extraHeight face |

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
	scaleX := 3.
	em := aFreeTypeFont pixelSize.
	[ | hintingFlags flags |face := aFreeTypeFont face.
	face setPixelWidth: em height: em.
	hintingFlags := FreeTypeSettings current hintingFlags.
	flags :=  LoadNoBitmap bitOr:( LoadIgnoreTransform bitOr: hintingFlags). 
	face loadCharacter:charCode flags: flags. 
	] on: FT2Error do:[:e | 
		^(GlyphForm extent: 0@0 depth: depth)
			advance: 0@0;
			linearAdvance: 0@0;
			offset:0@0;
			yourself]. 
	glyph := face glyph.
	slant := aFreeTypeFont simulatedItalicSlant.
	synthBoldStrength := aFreeTypeFont simulatedBoldStrength.
	synthBoldStrength ~= 0
		ifTrue:[face emboldenOutline: synthBoldStrength].
	boldExtra := 4 * synthBoldStrength abs ceiling.
	face transformOutlineAngle: 0 scalePoint: scaleX@1  slant: slant.
	extraWidth := (glyph height * slant) abs ceiling.
	extraWidth := extraWidth + boldExtra.
	sub > 0 ifTrue:[ extraWidth := extraWidth + 3].
	extraHeight := boldExtra.
	form := GlyphForm 
		extent: ((glyph width + extraWidth "+ 6" + 1 + 2)*scaleX)@(glyph height +extraHeight + 1)  
		depth: depth.
	s := (glyph height-glyph hBearingY)  * slant.
	s := s sign * (s abs ceiling). 
	offsetX := (glyph hBearingX negated + s + (boldExtra // 2) + 1) * scaleX .
	offsetY := glyph height - glyph hBearingY + (boldExtra//2).
	face translateOutlineBy: (offsetX+(sub*scaleX/64))@offsetY.
	face renderGlyphIntoForm: form.
	form offset: ((glyph hBearingX - s - 1 - (boldExtra // 2)) * scaleX)@ (glyph hBearingY + 1 + (boldExtra / 2) ceiling) negated.
	"When not hinting FreeType sets the advance to the truncated linearAdvance.
	The characters appear squashed together. Rounding is probably better, so we fix the advance here"
	aFreeTypeFont subPixelPositioned
		ifTrue:[ form advance: glyph roundedPixelLinearAdvance * (scaleX@1)]
		ifFalse:[ form advance: glyph advance x * scaleX@glyph advance y].
	form linearAdvance: glyph linearAdvance.
	^form
]

{ #category : #rendering }
FreeTypeSubPixelAntiAliasedGlyphRenderer >> subGlyphOf: aCharacter colorValue: aColorValue mono: monoBoolean subpixelPosition: sub font: aFreeTypeFont [

	| f |
	monoBoolean
		ifFalse:[
			f := self 
				renderStretchedGlyph: aCharacter 
				depth: 8 
				subpixelPosition: sub 
				font: aFreeTypeFont. 
			f := self filter: f]
		ifTrue:[
			f := self 
				renderGlyph: aCharacter 
				depth: 1 
				subpixelPosition: sub 
				font: aFreeTypeFont.
			f := self fixBytesForMono: f.					
			f := f asFormOfDepth: 32].
	^f
]
