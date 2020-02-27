"
I am something acts like a font out of collection of fonts. First font in me is used as representative font to answer many messages.

Examples:
- display a font: StrikeFontSet allInstances first fontDisplay 
- calculate pixels width of a string for a font: StrikeFontSet allInstances first widthOfString: 'Pharo'
"
Class {
	#name : #StrikeFontSet,
	#superclass : #AbstractFont,
	#instVars : [
		'fontArray',
		'emphasis',
		'derivativeFonts',
		'name'
	],
	#pools : [
		'TextConstants'
	],
	#category : #'Graphics-Fonts'
}

{ #category : #'instance creation' }
StrikeFontSet class >> familyName: aName size: aSize [
	"Answer a font (or the default font if the name is unknown) in the specified size."

	| collection |
	collection :=  self allInstances select: [:inst | (inst name beginsWith: aName) and: [inst emphasis = 0]].
	collection isEmpty ifTrue: [
		(aName = 'DefaultMultiStyle') ifTrue: [
			collection := (TextSharedInformation at: #DefaultMultiStyle) fontArray.
		] ifFalse: [
			^ TextStyle defaultFont
		]
	].
	collection := collection asSortedCollection: [:a :b | a pointSize <= b pointSize].
	collection do: [:s | (s pointSize >= aSize) ifTrue: [^ s]].
	^ TextStyle defaultFont.

]

{ #category : #'instance creation' }
StrikeFontSet class >> familyName: aName size: aSize emphasized: emphasisCode [
	"Create the font with this emphasis"

	^ (self familyName: aName size: aSize) emphasized: emphasisCode

]

{ #category : #utilities }
StrikeFontSet class >> findMaximumLessThan: f in: array [

	array size to: 1 by: -1 do: [:i |
		f height >= (array at: i) height ifTrue: [^ array at: i].
	].
	^ array first.

]

{ #category : #'file in/out' }
StrikeFontSet class >> installNewFontAtIndex: newIndex fromOld: oldIndex [

	
	self allInstances do: [:set | | newArray fontArray |
		fontArray := set fontArray.
		newIndex + 1 > fontArray size ifTrue: [
			newArray := Array new: newIndex + 1.
			newArray replaceFrom: 1 to: fontArray size with: fontArray startingAt: 1.
			newArray at: newIndex + 1 put: (fontArray at: oldIndex + 1).
			set initializeWithFontArray: newArray.
		] ifFalse: [
			fontArray at: newIndex + 1 put: (fontArray at: oldIndex + 1).
		].
	].

"
StrikeFontSet installNewFontAtIndex: UnicodeSimplifiedChinese leadingChar fromOld: UnicodeJapanese leadingChar
StrikeFontSet installNewFontAtIndex: UnicodeKorean leadingChar fromOld: UnicodeJapanese leadingChar
"

]

{ #category : #'instance creation' }
StrikeFontSet class >> newFontArray: anArray [
 
	^super new initializeWithFontArray: anArray

]

{ #category : #'file in/out' }
StrikeFontSet class >> removeFontsForEncoding: leadingChar encodingName: encodingSymbol [

	| insts |
	leadingChar = 0 ifTrue: [^ self error: 'you cannot delete the intrinsic fonts'].
	insts := self allInstances.
	insts do: [:inst | | index fonts newFonts |
		fonts := inst fontArray.
		fonts size >= (leadingChar + 1) ifTrue: [
			leadingChar + 1 = fonts size ifTrue: [
				newFonts := fonts copyFrom: 1 to: fonts size - 1.
				index := newFonts indexOf: nil.
				index > 0 ifTrue: [newFonts := newFonts copyFrom: 1 to: index - 1].
				inst initializeWithFontArray: newFonts.
			] ifFalse: [
				fonts at: leadingChar + 1 put: nil.
			].
		].
	].

	TextSharedInformation removeKey: encodingSymbol asSymbol ifAbsent: [].

]

{ #category : #private }
StrikeFontSet >> addNewFont: aFont at: encodingIndex [

	| newArray |
	encodingIndex > fontArray size ifTrue: [
		newArray := Array new: encodingIndex.
		newArray replaceFrom: 1 to: fontArray size with: fontArray startingAt: 1.
	] ifFalse: [
		newArray := fontArray.
	].

	newArray at: encodingIndex put: aFont.

	self initializeWithFontArray: newArray.

]

{ #category : #accessing }
StrikeFontSet >> ascent [

	^ (fontArray  at: 1) ascent.

]

{ #category : #accessing }
StrikeFontSet >> ascentKern [

	^ (fontArray  at: 1) ascentKern.

]

{ #category : #accessing }
StrikeFontSet >> ascentOf: aCharacter [
	^(self fontOf: aCharacter) ascent
]

{ #category : #accessing }
StrikeFontSet >> baseKern [

	^ (fontArray  at: 1) baseKern.

]

{ #category : #accessing }
StrikeFontSet >> bonk: glyphForm with: bonkForm at: j [
	"Bonking means to run through the glyphs clearing out black pixels
	between characters to prevent them from straying into an adjacent
	character as a result of, eg, bolding or italicizing"
	"Uses the bonkForm to erase at every character boundary in glyphs."

	| bb offset font x |
	font := (fontArray at: j).
	offset := bonkForm offset x.
	bb := BitBlt toForm: glyphForm.
	bb sourceForm: bonkForm; sourceRect: bonkForm boundingBox;
		combinationRule: Form erase; destY: 0.
	x := font xTable.
	(x isMemberOf: SparseLargeTable) ifTrue: [
		x base to: x size-1 do: [:i | bb destX: (x at: i) + offset; copyBits].
	] ifFalse: [
		1 to: x size-1 do: [:i | bb destX: (x at: i) + offset; copyBits].
	].

]

{ #category : #'character shapes' }
StrikeFontSet >> characterFormAt: character [ 

	| encoding ascii xTable leftX rightX |
	encoding := character leadingChar + 1.
	ascii := character charCode.
	(ascii < (fontArray at: encoding) minAscii or: [ascii > (fontArray at: encoding) maxAscii])
		ifTrue: [ascii := (fontArray at: encoding) maxAscii].
	xTable := (fontArray at: encoding) xTable.
	leftX := xTable at: ascii + 1.
	rightX := xTable at: ascii + 2.
	^ (fontArray at: encoding) glyphs copy: (leftX @ 0 corner: rightX @ self height).

]

{ #category : #'character shapes' }
StrikeFontSet >> characterFormAt: character put: characterForm [ 

	| ascii leftX rightX widthDif newGlyphs encoding xTable glyphs |
	encoding := character leadingChar + 1.
	ascii := character charCode.
	ascii < (fontArray at: encoding) minAscii ifTrue: [
		^ self error: 'Cant store characters below min ascii'
	].
	ascii > (fontArray at: encoding) maxAscii ifTrue: [
		^ self error: 'No change made'
	].
	xTable := (fontArray at: encoding) xTable.
	leftX := xTable at: ascii + 1.
	rightX := xTable at: ascii + 2.
	glyphs := (fontArray at: encoding) glyphs.
	widthDif := characterForm width - (rightX - leftX).
	widthDif ~= 0 ifTrue: [
		newGlyphs := Form extent: glyphs width + widthDif @ glyphs height.
		newGlyphs copy: (0 @ 0 corner: leftX @ glyphs height) from: 0 @ 0
			in: glyphs rule: Form over.
		newGlyphs
				copy: (rightX + widthDif @ 0 corner: newGlyphs width @ glyphs height)
				from: rightX @ 0 in: glyphs rule: Form over.
		glyphs := newGlyphs.
		"adjust further entries on xTable"
		xTable := xTable copy.
		ascii + 2 to: xTable size do: [:i |
			xTable at: i put: (xTable at: i) + widthDif]].
	glyphs copy: (leftX @ 0 extent: characterForm extent) from: 0 @ 0 in: characterForm rule: Form over.

]

{ #category : #accessing }
StrikeFontSet >> characterToGlyphMap [
	"used in 
	primDisplayString: aString from: startIndex to: stopIndex
			map: font characterToGlyphMap xTable: font xTable
			kern: kernDelta.
	
	Since 'font xTable' using a first font xtable, we could use the same glyph mapping for it'
	
	This should allow a primitive to not fail, because of characterToGlyphMap == nil
	"
	^ (fontArray at: 1) characterToGlyphMap
]

{ #category : #displaying }
StrikeFontSet >> characters: anInterval in: sourceString displayAt: aPoint clippedBy: clippingRectangle rule: ruleInteger fillColor: aForm kernDelta: kernDelta on: aBitBlt [
	"Simple, slow, primitive method for displaying a line of characters.
	No wrap-around is provided."

	^anInterval inject: aPoint into: 
		[:destPoint :i |
		| f xTable leftX noFont sourceRect encoding ascii rightX |
		encoding := (sourceString at: i) leadingChar + 1.
		noFont := false.
		f := [fontArray at: encoding]
			on: Exception do: [:ex | nil].
		f ifNil: [noFont := true. f := fontArray at: 1].
		ascii := (noFont ifTrue: [$?] ifFalse: [sourceString at: i]) charCode.
		(ascii < f minAscii
			or: [ascii > f maxAscii])
			ifTrue: [ascii := f maxAscii].
		xTable := f xTable.
		leftX := xTable at: ascii + 1.
		rightX := xTable at: ascii + 2.
		sourceRect := leftX @ 0 extent: (rightX - leftX) @ self height.
		aBitBlt copyFrom: sourceRect in: f glyphs to: destPoint.
		destPoint x + (rightX - leftX + kernDelta) @ destPoint y.
	].

]

{ #category : #copying }
StrikeFontSet >> copy [

	| s a |
	s := self class new.
	s name: self name.
	s emphasis: self emphasis.
	s reset.
	a := Array new: fontArray size.
	1 to: a size do: [:i |
		a at: i put: (fontArray at: i) copy.
	].
	s fontArray: a.
	^ s.

]

{ #category : #accessing }
StrikeFontSet >> derivativeFonts [
	^derivativeFonts copyWithout: nil
]

{ #category : #accessing }
StrikeFontSet >> descent [

	^ (fontArray  at: 1) descent.

]

{ #category : #accessing }
StrikeFontSet >> descentKern [

	^ (fontArray at: 1) descentKern.

]

{ #category : #accessing }
StrikeFontSet >> descentOf: aCharacter [
	^(self fontOf: aCharacter) descent
]

{ #category : #displaying }
StrikeFontSet >> displayLine: aString at: aPoint [ 
	"Display the characters in aString, starting at position aPoint."

	self characters: (1 to: aString size)
		in: aString
		displayAt: aPoint
		clippedBy: Display boundingBox
		rule: Form over
		fillColor: nil
		kernDelta: 0
		on: (BitBlt toForm: Display).

]

{ #category : #displaying }
StrikeFontSet >> displayMultiString: aString on: aBitBlt from: startIndex to: stopIndex at: aPoint kern: kernDelta baselineY: baselineY [

	| destPoint leftX rightX glyphInfo g destY |
	destPoint := aPoint.
	glyphInfo := Array new: 5.
	startIndex to: stopIndex do: [:charIndex |
		self glyphInfoOf: (aString at: charIndex) into: glyphInfo.
		g := glyphInfo at:1.
		leftX := glyphInfo at:2.
		rightX := glyphInfo at:3.
		((glyphInfo at:5) ~= aBitBlt lastFont) ifTrue: [
			(glyphInfo at:5) installOn: aBitBlt.
		].
		aBitBlt sourceForm: g.
		destY := baselineY - (glyphInfo at:4).
		aBitBlt destX: (destPoint x) destY: destY width: (rightX - leftX) height: (self height).
		aBitBlt sourceOrigin: leftX @ 0.
		aBitBlt copyBits.
		destPoint := destPoint x + (rightX - leftX + kernDelta) @ destPoint y.
	].
	^ destPoint.
]

{ #category : #displaying }
StrikeFontSet >> displayString: aString on: aBitBlt from: startIndex to: stopIndex at: aPoint kern: kernDelta [ 

	^ self displayString: aString on: aBitBlt from: startIndex to: stopIndex at: aPoint kern: kernDelta baselineY: aPoint y + self ascent.

]

{ #category : #displaying }
StrikeFontSet >> displayString: aString on: aBitBlt from: startIndex to: stopIndex at: aPoint kern: kernDelta baselineY: baselineY [
	"Draw the given string from startIndex to stopIndex 
	at aPoint on the (already prepared) BitBlt."
	
	"Look for an excuse to use the fast primitive"
 	(aString hasWideCharacterFrom: startIndex to: stopIndex) ifTrue:[
		^ self displayMultiString: aString 
				on: aBitBlt 
				from: startIndex 
				to: stopIndex 
				at: aPoint 
				kern: kernDelta 
				baselineY: baselineY].

	^ aBitBlt displayString: aString 
			from: startIndex 
			to: stopIndex 
			at: aPoint 
			strikeFont: self
			kern: kernDelta
]

{ #category : #displaying }
StrikeFontSet >> displayString: aString on: aBitBlt from: startIndex to: stopIndex at: aPoint kern: kernDelta from: fromFont baselineY: baselineY [

	| destPoint leftX rightX glyphInfo g tag char destY rIndex| 
	destPoint := aPoint.
	rIndex := startIndex.
	tag := (aString at: rIndex) leadingChar.
	glyphInfo := Array new: 5.
	[rIndex <= stopIndex] whileTrue: [
		char := aString at: rIndex.
		((fromFont hasGlyphOf: char) or: [char leadingChar ~= tag]) ifTrue: [^ Array with: rIndex with: destPoint].
		self glyphInfoOf: char into: glyphInfo.
		g := glyphInfo at: 1.
		leftX := glyphInfo at: 2.
		rightX := glyphInfo at: 3.
		(glyphInfo fifth ~= aBitBlt lastFont) ifTrue: [
			glyphInfo fifth installOn: aBitBlt.
		].
		aBitBlt sourceForm: g.
		destY := baselineY - (glyphInfo at: 4). 
		aBitBlt destX: destPoint x.
		aBitBlt destY: destY.
		aBitBlt sourceOrigin: leftX @ 0.
		aBitBlt width: rightX - leftX.
		aBitBlt height: self height.
		aBitBlt copyBits.
		destPoint := destPoint + (rightX - leftX + kernDelta @ 0).
		rIndex := rIndex + 1.
	].
	^ Array with: rIndex with: destPoint.

]

{ #category : #accessing }
StrikeFontSet >> emphasis [
	"Answer the integer code for synthetic bold, italic, underline, and 
	strike-out."

	^ emphasis.

]

{ #category : #accessing }
StrikeFontSet >> emphasis: code [ 
	"Set the integer code for synthetic bold, itallic, underline, and strike-out, 
	where bold=1, italic=2, underlined=4, and struck out=8."

	emphasis := code.

]

{ #category : #accessing }
StrikeFontSet >> emphasized: code [ 

	"Answer a copy of the receiver with emphasis set to include code."
	| derivative addedEmphasis base safeCode |
	code = 0 ifTrue: [^ self].
	derivativeFonts isEmptyOrNil ifTrue: [^ self].
	derivative := derivativeFonts at: (safeCode := code min: derivativeFonts size).
	derivative ifNotNil: [^ derivative].  "Already have this style"

	"Dont have it -- derive from another with one with less emphasis"
	addedEmphasis := 1 bitShift: safeCode highBit - 1.
	base := self emphasized: safeCode - addedEmphasis.  "Order is Bold, Ital, Under, Narrow"
	addedEmphasis = 1 ifTrue:   "Compute synthetic bold version of the font"
		[derivative := (base copy name: base name , 'B') makeBoldGlyphs].
	addedEmphasis = 2 ifTrue:   "Compute synthetic italic version of the font"
		[ derivative := (base copy name: base name , 'I') makeItalicGlyphs].
	addedEmphasis = 4 ifTrue:   "Compute underlined version of the font"
		[derivative := (base copy name: base name , 'U') makeUnderlinedGlyphs].
	addedEmphasis = 8 ifTrue:   "Compute narrow version of the font"
		[derivative := (base copy name: base name , 'N') makeCondensedGlyphs].
	addedEmphasis = 16 ifTrue:   "Compute struck-out version of the font"
		[derivative := (base copy name: base name , 'X') makeStruckOutGlyphs].
	derivative emphasis: safeCode.
	derivativeFonts at: safeCode put: derivative.
	^ derivative

]

{ #category : #accessing }
StrikeFontSet >> familyName [

	^ (fontArray at: 1) familyName.

]

{ #category : #accessing }
StrikeFontSet >> familySizeFace [

	^ Array
		with: (fontArray  at: 1) name
		with: self height
		with: (fontArray  at: 1) emphasis

]

{ #category : #accessing }
StrikeFontSet >> fontArray [

	^ fontArray

]

{ #category : #accessing }
StrikeFontSet >> fontArray: anArray [

	fontArray := anArray.

]

{ #category : #accessing }
StrikeFontSet >> fontNameWithPointSize [

	^ (fontArray at: 1) fontNameWithPointSize.

]

{ #category : #accessing }
StrikeFontSet >> fontOf: aCharacter [
	"Answer the actual font to use for aCharacter"
	^self fontOf: aCharacter ifAbsent:[fontArray at: 1]
]

{ #category : #accessing }
StrikeFontSet >> fontOf: aCharacter ifAbsent: aBlock [
	"Answer the actual font to use for aCharacter"
	| encoding font |
	encoding := aCharacter leadingChar + 1.
	encoding <= fontArray size 
		ifTrue:[font := fontArray at: encoding].
	font ifNil:[^aBlock value].
	^font

]

{ #category : #private }
StrikeFontSet >> glyphInfoOf: aCharacter into: glyphInfoArray [
	| index f code leftX |
	index := aCharacter leadingChar + 1.
	fontArray size < index
		ifTrue: [ ^ self questionGlyphInfoInto: glyphInfoArray ].
	(f := fontArray at: index) ifNil: [ ^ self questionGlyphInfoInto: glyphInfoArray ].
	code := aCharacter charCode.
	(code between: f minAscii and: f maxAscii)
		ifFalse: [ ^ self questionGlyphInfoInto: glyphInfoArray ].
	leftX := f xTable at: code + 1.
	leftX < 0
		ifTrue: [ ^ self questionGlyphInfoInto: glyphInfoArray ].
	glyphInfoArray
		at: 1 put: f glyphs;
		at: 2 put: leftX;
		at: 3 put: (f xTable at: code + 2);
		at: 4 put: (f ascentOf: aCharacter);
		at: 5 put: self.
	^ glyphInfoArray
]

{ #category : #accessing }
StrikeFontSet >> glyphs [

	^ (fontArray  at: 1) glyphs

]

{ #category : #accessing }
StrikeFontSet >> glyphsEncoding: anInteger [

	^ (fontArray at: (anInteger+1)) glyphs.

]

{ #category : #accessing }
StrikeFontSet >> height [

	^ (fontArray  at: 1) height.

]

{ #category : #accessing }
StrikeFontSet >> heightOf: aCharacter [
	^(self fontOf: aCharacter) height
]

{ #category : #'private - initialization' }
StrikeFontSet >> initializeWithFontArray: anArray [ 
	"Initialize with given font array, the ascent of primary font is modified 
	if another font has higher size"
	| primaryFont maxHeight newFont |
	fontArray := anArray.
	primaryFont := anArray at: 1.
	emphasis := 0.
	name := primaryFont name.
	maxHeight := anArray
				inject: 0
				into: [:theHeight :font | (font notNil
							and: [theHeight < font height])
						ifTrue: [font height]
						ifFalse: [theHeight]].
	primaryFont height < maxHeight
		ifTrue: [newFont := primaryFont copy
						fixAscent: primaryFont ascent + (maxHeight - primaryFont height)
						andDescent: primaryFont descent
						head: 0.
			fontArray at: 1 put: newFont].
	self reset
]

{ #category : #displaying }
StrikeFontSet >> installOn: aDisplayContext [

	^ aDisplayContext installStrikeFont: self.

]

{ #category : #displaying }
StrikeFontSet >> installOn: aDisplayContext foregroundColor: foregroundColor backgroundColor: backgroundColor [ 

	^ aDisplayContext
		installStrikeFont: self
		foregroundColor: foregroundColor
		backgroundColor: backgroundColor.

]

{ #category : #testing }
StrikeFontSet >> isFontSet [

	^ true.

]

{ #category : #testing }
StrikeFontSet >> isSynthetic [

	^ fontArray first isSynthetic
]

{ #category : #accessing }
StrikeFontSet >> latin1 [
	"Answer primary font"
	^ fontArray at: 1
]

{ #category : #accessing }
StrikeFontSet >> lineGrid [

	| f |
	f := fontArray at: 1.
	^ f ascent + f descent.

]

{ #category : #emphasis }
StrikeFontSet >> makeBoldGlyphs [
	"Make a bold set of glyphs with same widths by ORing 1 bit to the right
		(requires at least 1 pixel of intercharacter space)"

	| g bonkForm font |
	1 to: fontArray size do: [:i |
		font := fontArray at: i.
		font ifNotNil: [
			g := font glyphs deepCopy.
			bonkForm := (Form extent: 1@16) fillBlack offset: -1@0.
			self bonk: g with: bonkForm at: i.
			g copyBits: g boundingBox from: g at: (1@0)
				clippingBox: g boundingBox rule: Form under fillColor: nil.
			(fontArray at: i) setGlyphs: g.
		].
	].

]

{ #category : #emphasis }
StrikeFontSet >> makeItalicGlyphs [
	"Make an italic set of glyphs with same widths by skewing left and right
		(may require more intercharacter space)"

	| g bonkForm bc font |
	1 to: fontArray size do: [:j |
		font := (fontArray at: j).
		font ifNotNil: [
			g := font glyphs deepCopy.
			"BonkForm will have bits where slanted characters overlap their neighbors."
			bonkForm := Form extent: (self height//4+2) @ self height.
			bc := font descent//4 + 1.  "Bonker x-coord corresponding to char boundary."
			bonkForm fill: (0 @ 0 corner: (bc+1) @ font ascent) fillColor: Color black.
			4 to: font ascent-1 by: 4 do:
				[:y | 		"Slide ascenders right..."
				g copy: (1@0 extent: g width @ (font ascent - y))
					from: 0@0 in: g rule: Form over.
				bonkForm copy: (1@0 extent: bonkForm width @ (font ascent - y))
					from: 0@0 in: bonkForm rule: Form over].
			bonkForm fill: (0 @ 0 corner: (bc+1) @ font ascent) fillColor: Color white.
			bonkForm fill: (bc @ font ascent corner: bonkForm extent) fillColor: Color black.
			font ascent to: font height-1 by: 4 do:
				[:y | 		"Slide descenders left..."
				g copy: (0@y extent: g width @ g height)
					from: 1@y in: g rule: Form over.
				bonkForm copy: (0@0 extent: bonkForm width @ bonkForm height)
					from: 1@0 in: bonkForm rule: Form over].
			bonkForm fill: (bc @ font ascent corner: bonkForm extent) fillColor: Color white.
			"Now use bonkForm to erase at every character boundary in glyphs."
			bonkForm offset: (0-bc) @ 0.
			font bonk: g with: bonkForm.
			font setGlyphs: g
		].
	].

]

{ #category : #emphasis }
StrikeFontSet >> makeStruckOutGlyphs [
	"Make a struck-out set of glyphs with same widths"

	| g font |
	1 to: fontArray size do: [:i |
		font := (fontArray at: i).
		font ifNotNil: [
			g := font glyphs deepCopy.
			g fillBlack: (0 @ (font ascent - (font ascent//3)) extent: g width @ 1).
			font setGlyphs: g
		].
	].

]

{ #category : #emphasis }
StrikeFontSet >> makeUnderlinedGlyphs [
	"Make an underlined set of glyphs with same widths"

	| g font |
	1 to: fontArray size do: [:i |
		font := (fontArray at: i).
		font ifNotNil: [
			g := font glyphs deepCopy.
			g fillBlack: (0 @ (font ascent+1) extent: g width @ 1).
			font setGlyphs: g
		].
	].

]

{ #category : #accessing }
StrikeFontSet >> maxAsciiFor: encoding [

	| f |
	f := (fontArray at: encoding+1).
	f ifNotNil: [^ f maxAscii].
	^ 0.

]

{ #category : #accessing }
StrikeFontSet >> maxEncoding [

	^ fontArray size.

]

{ #category : #accessing }
StrikeFontSet >> maxWidth [

	^ (fontArray at: 1) maxWidth.

]

{ #category : #testing }
StrikeFontSet >> name [

	^ name

]

{ #category : #accessing }
StrikeFontSet >> name: aString [

	name := aString

]

{ #category : #accessing }
StrikeFontSet >> pointSize [

	^ (fontArray  at: 1) pointSize.

]

{ #category : #copying }
StrikeFontSet >> postCopy [
	super postCopy.
	self reset.
	fontArray := fontArray copy
]

{ #category : #printing }
StrikeFontSet >> printOn: aStream [

	super printOn: aStream.
	aStream nextPutAll: '(' , self name.
	aStream space.
	self height printOn: aStream.
	aStream nextPut: $).

]

{ #category : #private }
StrikeFontSet >> questionGlyphInfoInto: glyphInfoArray [

	| f ascii |
	f := fontArray at: 1.
	ascii := $? asciiValue.
	glyphInfoArray at: 1 put: f glyphs;
		at: 2 put: (f xTable at: ascii + 1);
		at: 3 put: (f xTable at: ascii + 2);
		at: 4 put: (self ascentOf: $?);
		at: 5 put: self.
	^ glyphInfoArray.

]

{ #category : #initialization }
StrikeFontSet >> reset [
	"Reset the cache of derivative emphasized fonts"

	derivativeFonts := Array new: 32.

]

{ #category : #accessing }
StrikeFontSet >> subscript [

	^ (fontArray  at: 1) subscript

]

{ #category : #accessing }
StrikeFontSet >> superscript [

	^ (fontArray  at: 1) superscript

]

{ #category : #accessing }
StrikeFontSet >> textStyle [

	^ TextStyle actualTextStyles detect: [:aStyle | (aStyle fontArray collect: [:s | s name]) includes: self name]
		ifNone: [].

]

{ #category : #accessing }
StrikeFontSet >> widthOf: aCharacter [ 
	"Answer the width of the argument as a character in the receiver."
	^(self fontOf: aCharacter) widthOf: aCharacter
]

{ #category : #measuring }
StrikeFontSet >> widthOfString: aString [

	aString ifNil:[^0].
	"Optimizing"
	(aString isByteString) ifTrue: [
		^ (self fontArray  at: 1) widthOfString: aString from: 1 to: aString size].
	^ self widthOfString: aString from: 1 to: aString size.
"
	TextStyle default defaultFont widthOfString: 'zort' 21
"

]

{ #category : #accessing }
StrikeFontSet >> xTable [
	"Answer an Array of the left x-coordinate of characters in glyphs."

	^ (fontArray  at: 1) xTable.

]

{ #category : #private }
StrikeFontSet >> xTableEncoding: anInteger [
	"Answer an Array of the left x-coordinate of characters in glyphs."

	^(fontArray at: anInteger + 1) xTable.

]
