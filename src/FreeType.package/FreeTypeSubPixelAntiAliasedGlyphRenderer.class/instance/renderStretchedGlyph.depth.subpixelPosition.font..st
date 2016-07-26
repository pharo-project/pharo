rendering
renderStretchedGlyph: aCharacter depth: depth subpixelPosition: sub font: aFreeTypeFont
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