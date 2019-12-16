"
A FreeType font
"
Class {
	#name : #FreeTypeFont,
	#superclass : #AbstractFont,
	#instVars : [
		'face',
		'pointSize',
		'simulatedEmphasis',
		'pixelSize',
		'widthAndKernedWidthCache',
		'cachedHeight',
		'cachedAscent',
		'cachedDescent',
		'subPixelPositioned',
		'symbolFont',
		'mutex'
	],
	#pools : [
		'FT2Constants',
		'FreeTypeCacheConstants'
	],
	#category : #'FreeType-Fonts'
}

{ #category : #'instance creation' }
FreeTypeFont class >> forLogicalFont: aLogicalFont fileInfo: aFreeTypeFileInfoAbstract [
	| pointSize index |
	pointSize := aLogicalFont pointSize.
	index := aFreeTypeFileInfoAbstract index.  
	^aFreeTypeFileInfoAbstract isEmbedded
		ifTrue:[
			self 
				fromBytes: aFreeTypeFileInfoAbstract fileContents 
				pointSize: pointSize 
				index: index]
		ifFalse:[
			self 
				fromFile: aFreeTypeFileInfoAbstract absolutePath
				pointSize: pointSize 
				index: index]
]

{ #category : #'instance creation' }
FreeTypeFont class >> fromBytes: aByteArray pointSize: anInteger [
	^self fromBytes: aByteArray pointSize: anInteger index: 0
]

{ #category : #'instance creation' }
FreeTypeFont class >> fromBytes: aByteArray pointSize: anInteger  index: i [
	^self new
		setFace: (FreeTypeFace fromBytes: aByteArray index: i) pointSize: anInteger; 
		yourself
]

{ #category : #'instance creation' }
FreeTypeFont class >> fromFile: aFileName pointSize: anInteger [ 
	^ self fromFile: aFileName pointSize: anInteger index: 0
]

{ #category : #'instance creation' }
FreeTypeFont class >> fromFile: aFileName pointSize: anInteger index: i [
	^self new
		setFace: (FreeTypeFace fromFile: aFileName index: i) pointSize: anInteger; 
		yourself
]

{ #category : #measuring }
FreeTypeFont >> ascent [
	| asc desc h |
	cachedAscent ifNotNil:[^cachedAscent].
	asc := self basicAscent.
	desc := self descent.
	h := self height.
	asc + desc < h ifFalse:[^cachedAscent := asc].
	"height is greater than asc+desc, adjust ascent to include the difference"
	^cachedAscent := h - desc 
]

{ #category : #measuring }
FreeTypeFont >> basicAscent [

	^(self face ascender * self pixelSize // self face unitsPerEm).

]

{ #category : #'glyph lookup' }
FreeTypeFont >> characterFormAt: aCharacter [
	FreeTypeSettings current 
		forceNonSubPixelDuring:[
			^self 
				glyphOf: aCharacter 
				destDepth: 32 
				colorValue: (Color black pixelValueForDepth: 32)
				subpixelPosition: 0]
]

{ #category : #accessing }
FreeTypeFont >> clearCachedMetrics [
	widthAndKernedWidthCache := cachedHeight := cachedAscent := cachedDescent := subPixelPositioned := nil
]

{ #category : #accessing }
FreeTypeFont >> defaultSimulatedItalicSlant [
	^0.22
]

{ #category : #accessing }
FreeTypeFont >> depth [

	^ 32.
]

{ #category : #measuring }
FreeTypeFont >> descent [
	^cachedDescent ifNil:[ 
		cachedDescent := ((self face descender * self pixelSize // self face unitsPerEm) negated) ]
]

{ #category : #measuring }
FreeTypeFont >> descentKern [
	"should have default in AbstractFont"
	^0
]

{ #category : #displaying }
FreeTypeFont >> displayLineGlyphOn: aDisplayContext from: startPoint to: endPoint [
	|  oldCombinationRule oldHalftoneForm originalColorMap clr depth foreColorVal foreColorAlpha glyph width height
	startPointX startPointY endPointX endPointY foreColor |
	oldCombinationRule := aDisplayContext combinationRule .
	oldHalftoneForm := aDisplayContext halftoneForm .
	originalColorMap := aDisplayContext colorMap.
	clr := (foreColor := aDisplayContext lastFontForegroundColor ifNil:[Color black asNontranslucentColor]) 
		pixelValueForDepth: 32.
	depth := aDisplayContext destForm depth.
	foreColorVal := clr bitAnd: 16rFFFFFF.
	foreColorAlpha := (clr bitAnd: 16rFF000000) >> 24.
	depth <= 8
		ifTrue:[
			aDisplayContext colorMap: (aDisplayContext cachedFontColormapFrom:32 to: depth)]
		ifFalse:[
			aDisplayContext colorMap: nil].
	startPointX := startPoint x truncated.
	startPointY := startPoint y.
	endPointX := endPoint x ceiling.
	endPointY := endPoint y.
	width := endPointX - startPointX.
	height := endPointY - startPointY.
	glyph := (Form extent: width@height depth: 32) fillWhite. "we could cache a big white glyph somewhere to save having to create this. Clipping will make only a part of it display"
	aDisplayContext sourceForm: glyph.
	aDisplayContext destOrigin: startPointX@startPointY.
	aDisplayContext width: width.
	aDisplayContext height: height.
	aDisplayContext 
		sourceOrigin: 0@0;
		halftoneForm: nil.
	(FreeTypeSettings current useSubPixelAntiAliasing and: [depth >= 8])
		ifTrue:[
			aDisplayContext 
				combinationRule: 41.
			aDisplayContext 
				copyBitsColor: foreColorVal 
				alpha: foreColorAlpha 
				gammaTable: FreeTypeSettings current gammaTable
				ungammaTable: FreeTypeSettings current gammaInverseTable]
		ifFalse:[
			glyph fillWithColor: foreColor.
			aDisplayContext combinationRule: (depth <= 8 ifTrue: [Form paint] ifFalse: [34]).
			aDisplayContext copyBits].		
	aDisplayContext 
		colorMap: originalColorMap;
		combinationRule: oldCombinationRule;
		halftoneForm: oldHalftoneForm.
	
	
]

{ #category : #displaying }
FreeTypeFont >> displayStrikeoutOn: aDisplayContext from: baselineStartPoint to: baselineEndPoint [
	| top bottom strikeoutThickness s e |
	
	"the strikeout size/position for TrueType fonts should really come from the TT_OS2 table.
	This needs to be read by the plugin when the face is created.
	For now, we use the underlineThickness, and 1/4 of the ascender from the baseline"
	strikeoutThickness := (self face underlineThickness * self pixelSize / self face unitsPerEm). 			
	top := ((self face ascender / 4) * self pixelSize / self face unitsPerEm) negated - (strikeoutThickness/2).
	top := top rounded.
	bottom := top + strikeoutThickness ceiling.
	s := baselineStartPoint + (0@top).
	e := baselineEndPoint + (0@bottom).
	self displayLineGlyphOn: aDisplayContext from: s to: e
	
]

{ #category : #displaying }
FreeTypeFont >> displayString: aString on: aDisplayContext from: startIndex to: stopIndex at: aPoint kern: kernDelta [
	
	^self displayString: aString
		on: aDisplayContext
		from: startIndex
		to: stopIndex
		at: aPoint
		kern: kernDelta
		baselineY: aPoint y  + self ascent
]

{ #category : #displaying }
FreeTypeFont >> displayString: aString on: aBitBlt from: startIndex to: stopIndex at: aPoint kern: kernDelta baselineY: baselineY [
	| glyph  depth foreColorVal foreColorAlpha originalColorMap clr subPixelPosition widthAndKernedWidth char nextChar floatDestX  destX destY offset gammaTable gammaInverseTable useRule41 |
	
	useRule41 := FreeTypeSettings current useSubPixelAntiAliasing and: [aBitBlt destForm depth >= 8].
	depth := aBitBlt destForm depth.
	originalColorMap := aBitBlt colorMap.
	clr := (aBitBlt lastFontForegroundColor ifNil:[Color black asNontranslucentColor]) 
		pixelValueForDepth: 32.
	useRule41 
		ifTrue:[
			foreColorVal := clr bitAnd: 16rFFFFFF.
			foreColorAlpha := (clr bitAnd: 16rFF000000) >> 24.
			gammaTable := FreeTypeSettings current gammaTable.
			gammaInverseTable := FreeTypeSettings current gammaInverseTable.]
		ifFalse:[
			foreColorVal := clr].
	depth <= 8
		ifTrue:[
			aBitBlt colorMap: (aBitBlt cachedFontColormapFrom:32 to: depth)]
		ifFalse:[
			aBitBlt colorMap: nil].
	destX := aPoint x.
	destY := baselineY.
	floatDestX := aPoint x.
	widthAndKernedWidth := Array new: 2.
 	startIndex to: stopIndex do: [:i | 
		subPixelPosition := ((floatDestX \\ 1) roundTo: "1/64" 0.015625) * 64.
		subPixelPosition = 64 
			ifTrue:[
				subPixelPosition := 0.
				destX := destX + 1].
		char := aString at: i.
		glyph := self 
			glyphOf: char 
			destDepth: depth 
			colorValue: foreColorVal 
			subpixelPosition: subPixelPosition.
		aBitBlt sourceForm: glyph.
		offset := glyph offset.
		aBitBlt destX: destX + offset x.
		aBitBlt destY: destY + offset y.
		aBitBlt width: glyph width.
		aBitBlt height: glyph height.
		useRule41
			ifTrue:[
				aBitBlt 
					copyBitsColor: foreColorVal 
					alpha: foreColorAlpha 
					gammaTable: gammaTable
					ungammaTable: gammaInverseTable]
			ifFalse:[
				aBitBlt copyBits].
		nextChar := (i + 1 <= stopIndex) 
				ifTrue:[aString at: i + 1]
				ifFalse:[nil].
		self 
			widthAndKernedWidthOfLeft: char 
			right: nextChar
			into: widthAndKernedWidth.
		floatDestX := floatDestX + (widthAndKernedWidth at: 2) + kernDelta.
		destX := floatDestX ].
	aBitBlt colorMap: originalColorMap.
	^ destX @ destY 

]

{ #category : #displaying }
FreeTypeFont >> displayUnderlineOn: aDisplayContext from: baselineStartPoint to: baselineEndPoint [
	| underlineTop underlineBottom underlineThickness s e |

	underlineThickness := (self face underlineThickness * self pixelSize / self face unitsPerEm). 
	underlineTop := (self face underlinePosition * self pixelSize / self face unitsPerEm) negated - (underlineThickness/2).
	underlineTop := underlineTop rounded + 1.  "needs the +1 , possibly because glyph origins are moved down by 1 so that their baselines line up with strike fonts"
	underlineBottom := underlineTop + underlineThickness ceiling.
	s := baselineStartPoint + (0@underlineTop).
	e := baselineEndPoint + (0@(underlineBottom)).
	self displayLineGlyphOn: aDisplayContext from: s to: e
]

{ #category : #accessing }
FreeTypeFont >> face [
	"Validate, and answer, the receiver's face"
	
	^face validate
]

{ #category : #accessing }
FreeTypeFont >> face: aFace [
	face := aFace
]

{ #category : #accessing }
FreeTypeFont >> familyName [ 

	^face familyName 
]

{ #category : #measuring }
FreeTypeFont >> getLinearWidthOf: aCharacter [ 
	| em glyph la charCode |

	aCharacter < $  ifTrue: [^self getLinearWidthOf: $ ].
	charCode := aCharacter asUnicode asInteger.
	(self face charmaps includes:'unic')
		ifTrue:[
			(self isSymbolFont and: [ charCode between: 16r20 and: 16rFF ])
				ifTrue:[charCode := charCode + 16rF000]]
		ifFalse:[
			(self face charmaps includes:'armn')
				ifTrue:[ "select apple roman char map, and map character from unicode to mac encoding"
					self face setCharMap:'armn'.
					charCode := aCharacter unicodeToMacRoman asUnicode asInteger. "check this!"]].
	em := self pixelSize.
	face validate.
	face setPixelWidth: em height: em.
	[face loadCharacter: charCode flags: (LoadNoBitmap bitOr: (LoadIgnoreTransform bitOr: "FreeTypeSettings current hintingFlags" 2 "no hinting"))]
		on: FT2Error , PrimitiveFailed do:[:e |
			face loadGlyph: 0 flags: (LoadNoBitmap bitOr: (LoadIgnoreTransform bitOr: FreeTypeSettings current hintingFlags "no hinting")) ].
	glyph := face glyph.
	la := glyph linearHorizontalAdvance.
	la isZero ifTrue:[
		"FreeType 2.2.1 sometimes screws up when getting metrics,
		Maybe the bug is in the plugin?
		For example Calibri pixel size 13 gives linearAdvance x of zero !
		We try again at double the size, and half the result"
		em := self pixelSize * 2.
		face validate.
		face setPixelWidth: em height: em.
		face loadCharacter: charCode flags:(LoadNoBitmap bitOr: (LoadIgnoreTransform bitOr: "FreeTypeSettings current hintingFlags" 2 "no hinting")). "load glyph metrics"
		glyph := face glyph.
		la := glyph linearHorizontalAdvance / 2.0].
	^la

]

{ #category : #measuring }
FreeTypeFont >> getWidthOf: aCharacter [ 
	"Glyphs are either 1 or 8 bit deep. For 32 bpp we use 8 bits, otherwise 1"
	| em glyph hintingFlags flags charCode |

	aCharacter < $  ifTrue: [^self getWidthOf: $ ].
	charCode := aCharacter asUnicode asInteger.
	(self face charmaps includes:'unic')
		ifTrue:[
			(self isSymbolFont and:[ charCode between: 16r20 and: 16rFF ])
				ifTrue:[charCode := charCode + 16rF000]]
		ifFalse:[
			(self face charmaps includes:'armn')
				ifTrue:[ "select apple roman char map, and map character from unicode to mac encoding"
					self face setCharMap:'armn'.
					charCode := aCharacter unicodeToMacRoman asUnicode asInteger. "check this!"]].
	em := self pixelSize.
	face validate.
	face isValid ifFalse:[^0].
	face setPixelWidth: em height: em.
	hintingFlags := FreeTypeSettings current hintingFlags.
	flags :=  LoadNoBitmap bitOr:( LoadIgnoreTransform bitOr: hintingFlags). 
	[face loadCharacter: charCode flags: flags.
	] on:FT2Error, PrimitiveFailed do:[:e | "character not in map?"^0]. 
	glyph := face glyph.
	"When not hinting FreeType sets the advance to the truncated linearAdvance.
	The characters appear squashed together. Rounding is probably better, so we 
	answer the rounded linear advance here"
	^self subPixelPositioned
		ifTrue:[ glyph roundedPixelLinearAdvance x]
		ifFalse:[ glyph advance x].

]

{ #category : #'glyph lookup' }
FreeTypeFont >> glyphOf: aCharacter colorValue: aColorValue mono: monoBoolean subpixelPosition: sub [

	^FreeTypeCache current
		atFont: self
		charCode: aCharacter asUnicode asInteger
		type: ((1+sub) << 32) + aColorValue
		ifAbsentPut: [
			FreeTypeGlyphRenderer current
				glyphOf: aCharacter 
				colorValue: aColorValue 
				mono: monoBoolean 
				subpixelPosition: sub 
				font: self]

]

{ #category : #'glyph lookup' }
FreeTypeFont >> glyphOf: aCharacter destDepth: destDepth colorValue: aColorValue subpixelPosition: sub [
	"sub can be between 0 and 63 and denotes the sub-pixel position of the glyph"
	| validSub |
	validSub := self isSubPixelPositioned
		ifTrue: [((sub asInteger max: 0) min: 63) "bitAnd: 2r111000"]
		ifFalse:[0].
	^(destDepth >=8 and:[FreeTypeSettings current subPixelAntiAliasing])
		ifTrue:[
			self 
				subGlyphOf: aCharacter
				colorValue: aColorValue
				mono: FreeTypeSettings current monoHinting
				subpixelPosition: validSub]
		ifFalse:[
			(destDepth >= 8 and:[FreeTypeSettings current useSubPixelAntiAliasing])
				ifTrue:[
					self 
						mode41GlyphOf: aCharacter
						colorValue: aColorValue
						mono: FreeTypeSettings current monoHinting
						subpixelPosition: validSub]
				ifFalse:[
					self 
						glyphOf: aCharacter
						colorValue: aColorValue
						mono: FreeTypeSettings current monoHinting
						subpixelPosition: validSub]]
]

{ #category : #preview }
FreeTypeFont >> gtInspectorFontIn: composite [
	<gtInspectorPresentationOrder: 0>
	composite morph
		title: 'Font';
		display: [ :x | | pane |
			pane := ScrollPane new
						alwaysShowScrollBars: true;
						yourself.
			pane scroller addMorph:
					((String streamContents: [ :s | 22 to: 255 do: [ :idx | s nextPut: idx asCharacter ] ]) asMorph
						font: x;
						yourself).
			pane ]
]

{ #category : #testing }
FreeTypeFont >> hasDistinctGlyphsForAll: asciiString [
	"Answer true if the receiver has glyphs for all the characters
	in asciiString and no single glyph is shared by more than one character, false otherwise.
	The default behaviour is to answer true, but subclasses may reimplement"
	| setOfIndices |
	self face isValid ifFalse:[^false].
	setOfIndices := Set new.
	asciiString asSet do:[:c | | i |
		(i := self face ffiGetCharIndex: c asInteger) = 0
			ifTrue:[^false]
			ifFalse:[
				(setOfIndices includes: i)
					ifTrue:[^false]
					ifFalse:[setOfIndices add: i]]].
	^true
]

{ #category : #testing }
FreeTypeFont >> hasGlyphsForAll: asciiString [
	"Answer true if the receiver has glyphs for all the characters
	in asciiString, false otherwise.
	The default behaviour is to answer true, but subclasses may reimplement"
	
	self face isValid ifFalse:[^false].
	asciiString do:[:c |
		(self face ffiGetCharIndex: c asInteger) = 0
			ifTrue:[^false]].
	^true
]

{ #category : #comparing }
FreeTypeFont >> hash [
	^pointSize hash
]

{ #category : #measuring }
FreeTypeFont >> height [

	^cachedHeight ifNil:[
		cachedHeight := (self face height * self pixelSize / self face unitsPerEm) ceiling ]
]

{ #category : #measuring }
FreeTypeFont >> hintedKerningLeft: leftChar right: rightChar [
	^(self linearKerningLeft: leftChar right: rightChar) rounded
]

{ #category : #measuring }
FreeTypeFont >> hintedWidthOf: aCharacter [
	"retrieve advance width for character. try to use cached glyph if possible"
	| charCode answer |
	
	charCode := aCharacter asUnicode asInteger.
	answer := FreeTypeCache current 
		atFont: self 
		charCode: charCode 
		type: FreeTypeCacheWidth
		ifAbsentPut: [self getWidthOf: aCharacter].
	^answer

]

{ #category : #initialization }
FreeTypeFont >> initialize: aFont [

	self face: aFont face.
]

{ #category : #displaying }
FreeTypeFont >> installOn: aBitBlt foregroundColor: foreColor backgroundColor: backColor [

	| |
	"fcolor := foreColor pixelValueForDepth: 32."
	aBitBlt installFreeTypeFont: self foregroundColor: foreColor backgroundColor: backColor.

]

{ #category : #testing }
FreeTypeFont >> isBold [
	^(simulatedEmphasis == nil and:[self face isBold])
		or:[self isSimulatedBold]
]

{ #category : #testing }
FreeTypeFont >> isFixedWidth [
	^self face isFixedWidth 
]

{ #category : #testing }
FreeTypeFont >> isItalic [
	^(simulatedEmphasis == nil and:[self face isItalic])
		or:[self isSimulatedItalic]
]

{ #category : #testing }
FreeTypeFont >> isRegular [
	^(simulatedEmphasis == nil and:[self face isRegular])
		or: [self isSimulatedRegular]
]

{ #category : #testing }
FreeTypeFont >> isSimulated [
	^simulatedEmphasis notNil
]

{ #category : #testing }
FreeTypeFont >> isSimulatedBold [
	^self simulatedEmphasis anyMask: 1
]

{ #category : #testing }
FreeTypeFont >> isSimulatedItalic [
	^self simulatedEmphasis anyMask: 2
]

{ #category : #testing }
FreeTypeFont >> isSimulatedRegular [
	^simulatedEmphasis = 0
]

{ #category : #testing }
FreeTypeFont >> isSubPixelPositioned [
	"Answer true if the receiver is currently using subpixel positioned
	glyphs, false otherwise. This affects how padded space sizes are calculated
	when composing text. 
	Currently, only FreeTypeFonts are subPixelPositioned, and only when not
	Hinted"
	
	^self subPixelPositioned
]

{ #category : #testing }
FreeTypeFont >> isSymbolFont [
	| charmaps |
	symbolFont ifNotNil:[^symbolFont].
	self face isValid ifFalse:[^false].
	charmaps := self face charmaps.
	(charmaps includes: 'symb') ifTrue:[^symbolFont := true]."MS Symbol font"
	^symbolFont := false
]

{ #category : #testing }
FreeTypeFont >> isTTCFont [
	"not really - look for senders of this"
	^true
]

{ #category : #measuring }
FreeTypeFont >> kerningLeft: leftChar right: rightChar [
	^self isSubPixelPositioned
		ifTrue: [self linearKerningLeft: leftChar right: rightChar]
		ifFalse:[self hintedKerningLeft: leftChar right: rightChar]
]

{ #category : #measuring }
FreeTypeFont >> lineGrid [

	^self height
]

{ #category : #measuring }
FreeTypeFont >> linearKerningLeft: leftChar right: rightChar [
	| f  l r |

	f := self face.
	f hasKerning ifFalse:[^0].
	l := leftChar asUnicode.
	r := rightChar asUnicode.
	(self face charmaps includes:'unic')
		ifTrue:[
			self isSymbolFont 
				ifTrue:[
					(l asInteger between: 16r20 and: 16rFF)
						ifTrue:[l := (Character value: l asInteger + 16rF000) asUnicode].
					(r asInteger between: 16r20 and: 16rFF)
						ifTrue:[r := (Character value: r asInteger + 16rF000) asUnicode]]]
		ifFalse:[
			(self face charmaps includes:'armn')
				ifTrue:[ "select apple roman char map, and map characters from unicode to mac encoding"
					self face setCharMap:'armn'.
					(l asInteger between: 16r20 and: 16rFF)
						ifTrue:[l := (Character value: l asInteger) unicodeToMacRoman].
					(r asInteger between: 16r20 and: 16rFF)
						ifTrue:[r := (Character value: r asInteger) unicodeToMacRoman]]].
	^(f kerningLeft: l right: r) x asFloat *  self pixelSize / f unitsPerEm
]

{ #category : #measuring }
FreeTypeFont >> linearWidthOf: aCharacter [
	"retrieve linear advance width for character. try to use cached glyph if possible.
	This is the scaled, unrounded advance width."
	| charCode answer |
	
	charCode := aCharacter asUnicode asInteger.
	answer := FreeTypeCache current 
		atFont: self 
		charCode: charCode 
		type: FreeTypeCacheLinearWidth
		ifAbsentPut: [self getLinearWidthOf: aCharacter].
	^answer

]

{ #category : #accessing }
FreeTypeFont >> maxAscii [
	"should have default in AbstractFont"
	^SmallInteger maxVal
]

{ #category : #accessing }
FreeTypeFont >> minAscii [
	"should have default in AbstractFont"
	^0
]

{ #category : #'glyph lookup' }
FreeTypeFont >> mode41GlyphOf: aCharacter colorValue: aColorValue mono: monoBoolean subpixelPosition: sub [

	| |
	^FreeTypeCache current
		atFont: self
		charCode: aCharacter asUnicode asInteger
		type: (FreeTypeCacheGlyph + sub)
		ifAbsentPut: [
			FreeTypeGlyphRenderer current
				mode41GlyphOf: aCharacter 
				colorValue: aColorValue 
				mono: monoBoolean 
				subpixelPosition: sub 
				font: self]


]

{ #category : #accessing }
FreeTypeFont >> mutex [

	^ mutex ifNil: [ mutex := Semaphore forMutualExclusion ]
]

{ #category : #measuring }
FreeTypeFont >> pixelSize [
	^pixelSize ifNil:[pixelSize := super pixelSize rounded]
]

{ #category : #notifications }
FreeTypeFont >> pixelsPerInchChanged [
	"the TextStyle pixels per inch setting has changed"
	
	pixelSize := nil.
	widthAndKernedWidthCache := nil.
	FreeTypeCache current removeAllForFont: self.
]

{ #category : #measuring }
FreeTypeFont >> pointSize [
	^pointSize
]

{ #category : #measuring }
FreeTypeFont >> pointSize: aSize [
	pointSize := aSize
]

{ #category : #accessing }
FreeTypeFont >> postscriptName [
	^self face postscriptName
]

{ #category : #printing }
FreeTypeFont >> printOn: aStream [

	aStream
		nextPutAll: self class name;
		nextPut: $(;
		print: face familyName;
		space;
		print: face styleName;
		space;
		print: pointSize;
		nextPut: $)
]

{ #category : #initialization }
FreeTypeFont >> releaseCachedState [
	face releaseCachedState.
	FreeTypeCache current removeAllForFont: self.
]

{ #category : #accessing }
FreeTypeFont >> setFace: aFreetypeFace pointSize: anInteger [
	face := aFreetypeFace.
	pointSize := anInteger.
]

{ #category : #accessing }
FreeTypeFont >> simulatedBoldStrength [
	"Answer the amount by which glyphs need to be emboldened/lightened
	according to the receiver's simulated emphasis and the face's real emphasis"
	| bold faceBold |
	
	self isSimulated ifFalse:[^0].
	bold := self isSimulatedBold.
	faceBold := face isBold.
	(bold and: [faceBold not])
		ifTrue:[^self pixelSize/24].
	^0
]

{ #category : #accessing }
FreeTypeFont >> simulatedEmphasis [
	"Answer the simulatedEmphasis.
	This is 
		0 - normal (no simulatedEmphasis, or simulated regular).
		1 - bold 
		2 - italic
		3 - bold & italic"
	^simulatedEmphasis ifNil:[0]
]

{ #category : #accessing }
FreeTypeFont >> simulatedEmphasis: anIntegerOrNil [
	"Set the simulatedEmphasis.
	This is 
		nil - no simulated emphasis
		0 - normal (simulated regular).
		1 - bold 
		2 - italic
		3 - bold & italic"
	simulatedEmphasis := anIntegerOrNil
]

{ #category : #accessing }
FreeTypeFont >> simulatedItalicSlant [
	"Answer the slant that needs to be added to italicize/un-italicize
	glyphs according to the receiver's simulated emphasis and the face's 
	real emphasis"
	| italic faceItalic |
	
	self isSimulated ifFalse:[^0].
	italic := self isSimulatedItalic.
	faceItalic := face isItalic.
	(italic and: [faceItalic not])
		ifTrue:[^self defaultSimulatedItalicSlant].
	^0
]

{ #category : #'glyph lookup' }
FreeTypeFont >> subGlyphOf: aCharacter colorValue: aColorValue mono: monoBoolean subpixelPosition: sub [

	^FreeTypeCache current
		atFont: self
		charCode: aCharacter asUnicode asInteger
		type: FreeTypeCacheGlyphLCD + sub
		ifAbsentPut: [ 
			FreeTypeGlyphRenderer current
				subGlyphOf: aCharacter 
				colorValue: aColorValue 
				mono: monoBoolean 
				subpixelPosition: sub 
				font: self]


]

{ #category : #testing }
FreeTypeFont >> subPixelPositioned [
	"Answer true if the receiver is currently using subpixel positioned
	glyphs, false otherwise. This affects how padded space sizes are calculated
	when composing text."
	| settings |
	^subPixelPositioned 
		ifNil:[
			settings := FreeTypeSettings current.
			subPixelPositioned := settings hinting not or:[settings lightHinting]]
]

{ #category : #validation }
FreeTypeFont >> validate [ 
	self face validate
]

{ #category : #accessing }
FreeTypeFont >> veryDeepCopyWith: deepCopier [
]

{ #category : #measuring }
FreeTypeFont >> widthAndKernedWidthCache [
	^widthAndKernedWidthCache ifNil:[widthAndKernedWidthCache := Dictionary new]
]

{ #category : #measuring }
FreeTypeFont >> widthAndKernedWidthOfLeft: leftCharacter right: rightCharacterOrNil into: aTwoElementArray [
	"Set the first element of aTwoElementArray to the width of leftCharacter and 
	the second element to the width of left character when kerned with
	rightCharacterOrNil. Answer the receiver
	
	We use a widthAndKernedWidthCache to store these values for speed"	
	| privateArray |
 
	privateArray := (self widthAndKernedWidthCache at: leftCharacter ifAbsentPut:[Dictionary new])
		at: (rightCharacterOrNil ifNil:[0 asCharacter])
		ifAbsentPut:[ 
			super 
				widthAndKernedWidthOfLeft: leftCharacter 
				right: rightCharacterOrNil 
				into: (Array new: 2)].
	"We can't answer privateArray, we MUST copy its elements into aTwoElementArray"
	aTwoElementArray 
		at: 1 put: (privateArray at: 1);
		at: 2 put: (privateArray at: 2).
	^aTwoElementArray
]

{ #category : #measuring }
FreeTypeFont >> widthOf: aCharacter [
	"retrieve advance width for character. try to use cached glyph if possible"
	^self isSubPixelPositioned
		ifTrue:[self linearWidthOf: aCharacter]
		ifFalse: [self hintedWidthOf: aCharacter]

]

{ #category : #measuring }
FreeTypeFont >> widthOfString: aString from: startIndex to: stopIndex [
	"Measure the length of the given string between start and stop index.
	Currently this allows for the right side bearing of the last char, but does not allow for the left side bearing of the first char. We really need a new method - boundingBoxOfString that allows for both. Senders of this will also need to know the LSB of the first char, and position their text accordingly"
	| char nextChar resultX glyph a subPixelPosition |

	a := Array new: 2.
	"FreeTypeSettings current hinting ifFalse:[
		^self linearWidthOfString: aString from: startIndex to: stopIndex]."
	resultX := 0.
	startIndex to: stopIndex do:[:i | 
		char := aString at: i.
		nextChar := (i + 1 <= stopIndex)
			ifTrue:[ aString at: i + 1]
			ifFalse:[nil].
		self widthAndKernedWidthOfLeft: char right:  nextChar into: a. 
		resultX := resultX + (a at:2).
		i = stopIndex 
			ifTrue:[
				subPixelPosition := (((resultX \\ 1) roundTo: "1/64" 0.015625) * 64) asInteger.
				subPixelPosition = 64 
					ifTrue:[
						subPixelPosition := 0.
						resultX := resultX + 1 ].
				subPixelPosition := (subPixelPosition max: 0) min: 63.
				glyph := self glyphOf: char colorValue: 0 mono: FreeTypeSettings current monoHinting subpixelPosition: subPixelPosition.
				glyph ifNotNil:[
					"currently the glyph is too wide. This is to allow for some extra space to ensure 
					the glyph is not clipped when it is produced. Either make the width accurate,
					or hold the RSB value separately, or hold an accurate width separately"
					resultX := resultX "+ 2" + glyph offset x "negated" + (glyph width - (a at: 2)  "glyph linearAdvance x floor")]]].
	^resultX ceiling 
]
