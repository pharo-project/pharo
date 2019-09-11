"
I represent a compact encoding of a set of Forms corresponding to characters in the ASCII character set. All the forms are placed side by side in a large form whose height is the font height, and whose width is the sum of all the character widths. The xTable variable gives the left-x coordinates of the subforms corresponding to the glyphs. Characters are mapped to glyphs by using the characterToGyphMap.

Subclasses can have non-trivial mapping rules as well as different representations for glyphs sizes (e.g., not using an xTable). If so, these classes should return nil when queried for xTable and/or the characterToGlyphMap. This will cause the CharacterScanner primitive to fail and query the font for the width of a character (so that a more programatical approach can be implemented).

For display, fonts need to implement two messages:
	#installOn: aDisplayContext foregroundColor: foregroundColor backgroundColor: backgroundColor
This method installs the receiver (a font) on the given DisplayContext (which may be an instance of BitBlt or Canvas (or any of it's subclasses). The font should take the appropriate action to initialize the display context so that further display operations can be optimized.
	#displayString: aString on: aDisplayContext from: startIndex to: stopIndex at: aPoint kern: kernDelta
This method is called for each subsequent run of characters in aString which is to be displayed with the (previously installed) settings.

"
Class {
	#name : #StrikeFont,
	#superclass : #AbstractFont,
	#instVars : [
		'characterToGlyphMap',
		'xTable',
		'glyphs',
		'name',
		'type',
		'minAscii',
		'maxAscii',
		'maxWidth',
		'ascent',
		'descent',
		'raster',
		'subscript',
		'superscript',
		'emphasis',
		'derivativeFonts',
		'pointSize',
		'fallbackFont'
	],
	#pools : [
		'TextConstants'
	],
	#category : #'Graphics-Fonts'
}

{ #category : #accessing }
StrikeFont class >> actualFamilyNames [
	"Answer a sorted list of actual family names, without the Default aliases"

	^(self familyNames copyWithoutAll: TextStyle defaultFamilyNames) asOrderedCollection
]

{ #category : #cleanup }
StrikeFont class >> cleanUp: aggressive [
	"Flush synthesized strike fonts"

	self allInstancesDo: [:sf| sf reset].
]

{ #category : #accessing }
StrikeFont class >> default [
	^TextStyle default defaultFont
]

{ #category : #accessing }
StrikeFont class >> defaultFallbackFontKeys [
	^ Array with: self defaultFontKey 
]

{ #category : #accessing }
StrikeFont class >> defaultFontKey [
	^ self dejaVuKey 

]

{ #category : #accessing }
StrikeFont class >> defaultFontSize [
	^ 9

]

{ #category : #accessing }
StrikeFont class >> dejaVuKey [
	^ 'Bitmap DejaVu Sans'

]

{ #category : #accessing }
StrikeFont class >> familyName: aName pointSize: aSize [
	"Answer a font (or the default font if the name is unknown) in the specified size."

	^ ((TextStyle named: aName asSymbol) ifNil: [TextStyle default]) fontOfPointSize: aSize
]

{ #category : #'instance creation' }
StrikeFont class >> familyName: aName pointSize: aSize emphasized: emphasisCode [
	"Create the font with this emphasis"

	^ (self familyName: aName pointSize: aSize) emphasized: emphasisCode
]

{ #category : #accessing }
StrikeFont class >> familyName: aName size: aSize [
	"Answer a font (or the default font if the name is unknown) in the specified size."
	| style |
	style := TextStyle named: aName asSymbol.
	style ifNil: [^(FontSubstitutionDuringLoading forFamilyName: aName pixelSize: aSize)
			signal: 'missing font' ].
	^style fontOfSize: aSize
]

{ #category : #'instance creation' }
StrikeFont class >> familyName: aName size: aSize emphasized: emphasisCode [
	"Create the font with this emphasis"

	^ (self familyName: aName size: aSize) emphasized: emphasisCode
]

{ #category : #accessing }
StrikeFont class >> familyNames [
	^ (TextSharedInformation select: [:each | each isKindOf: TextStyle]) keys asSortedCollection
]

{ #category : #'font creation' }
StrikeFont class >> fromHostFont: fontName size: fontSize flags: fontFlags weight: fontWeight [ 
	"
		^StrikeFont fromHostFont: (StrikeFont hostFontFromUser)
					size: 12 flags: 0 weight: 4.
	"
	| fontHandle glyphs xTable xStart maxWidth w glyphForm ascent descent fontHeight |
	fontHandle := self 
		primitiveCreateFont: fontName
		size: fontSize
		flags: fontFlags
		weight: fontWeight.
	ascent := self primitiveFontAscent: fontHandle.
	descent := self primitiveFontDescent: fontHandle.
	fontHeight := ascent + descent.
	xTable := Array new: 258.
	xStart := maxWidth := 0.
	0 
		to: 255
		do: 
			[ :i | 
			xTable 
				at: i + 1
				put: xStart.
			w := self 
				primitiveFont: fontHandle
				widthOfChar: i.
			w > maxWidth ifTrue: [ maxWidth := w ].
			xStart := xStart + w ].
	xTable 
		at: 256
		put: xStart.
	xTable 
		at: 257
		put: xStart.
	xTable 
		at: 258
		put: xStart.
	glyphs := Form 
		extent: xTable last @ fontHeight
		depth: 1.
	glyphForm := Form 
		extent: maxWidth @ fontHeight
		depth: 1.
	0 
		to: 255
		do: 
			[ :i | 
			glyphForm fillWhite.
			self 
				primitiveFont: fontHandle
				glyphOfChar: i
				into: glyphForm.
			xStart := xTable at: i + 1.
			glyphForm 
				displayOn: glyphs
				at: xStart @ 0.
			glyphForm 
				displayOn: Display
				at: xStart @ 0 ].
	self primitiveDestroyFont: fontHandle.
	^ Array 
		with: glyphs
		with: xTable
]

{ #category : #'font creation' }
StrikeFont class >> fromStrike: fileName [ 
	"Read a font from disk in the old ST-80 'strike' format.
	Note: this is an old format; use strike2 format instead"

	^self new newFromStrike: fileName
]

{ #category : #'font creation' }
StrikeFont class >> hostFontFromUser [
	"StrikeFont hostFontFromUser"
	| fontNames index labels |
	fontNames := self listFontNames asSortedCollection.
	labels := (String new: 100) writeStream.
	fontNames 
		do: [ :fn | labels nextPutAll: fn ]
		separatedBy: [ labels cr ].
	index := UIManager default 
		chooseFrom: labels contents substrings
		title: 'Choose your font'.
	index = 0 ifTrue: [ ^ nil ].
	^ fontNames at: index
]

{ #category : #'initialize-release' }
StrikeFont class >> initialize [
	SessionManager default registerGuiClassNamed: self name
]

{ #category : #removing }
StrikeFont class >> limitTo16Bits [
	"Limit glyph depth to 16 bits (it is usually 16 or 32).
	
	StrikeFont limitTo16Bits
	"
	StrikeFont allInstances do: [ :f | f
		setGlyphsDepthAtMost: 16 ].
]

{ #category : #'font creation' }
StrikeFont class >> listFont: index [
	<primitive:'primitiveListFont' module:'FontPlugin'>
	^nil
]

{ #category : #'font creation' }
StrikeFont class >> listFontNames [
	"StrikeFont listFontNames"
	"List all the OS font names"
	| font fontNames index |
	fontNames := Array new writeStream.
	index := 0.
	
	[ font := self listFont: index.
	font == nil ] whileFalse: 
		[ fontNames nextPut: font.
		index := index + 1 ].
	^ fontNames contents
]

{ #category : #'font creation' }
StrikeFont class >> localeChanged [
	self setupDefaultFallbackFont
]

{ #category : #'character shapes' }
StrikeFont class >> makeControlCharsVisible [
	"
	Make normally not visible characters, visible
	StrikeFont makeControlCharsVisible
	"
	self allInstances do: [ :font | font makeControlCharsVisible ]
]

{ #category : #'character shapes' }
StrikeFont class >> makeLfInvisible [
	"
	Make line feed characters invisible
	StrikeFont makeLfInvisible
	"
	self allInstances do: [ :font | font makeLfInvisible ]
]

{ #category : #'character shapes' }
StrikeFont class >> makeLfVisible [
	"
	Make line feed characters visible
	StrikeFont makeLfVisible
	"
	self allInstances do: [ :font | font makeLfVisible ]
]

{ #category : #'character shapes' }
StrikeFont class >> makeTabInvisible [
	"
	Make tab characters invisible
	StrikeFont makeTabInvisible
	"
	self allInstances do: [ :font | font makeTabInvisible ]
]

{ #category : #'character shapes' }
StrikeFont class >> makeTabVisible [
	"
	Make tab characters visible
	StrikeFont makeTabVisible
	"
	self allInstances do: [ :font | font makeTabVisible ]
]

{ #category : #'instance creation' }
StrikeFont class >> passwordFontSize: aSize [ 
	^ FixedFaceFont new passwordFont fontSize: aSize
]

{ #category : #'font creation' }
StrikeFont class >> primitiveCreateFont: fontName size: fontSize flags: fontFlags weight: fontWeight [
	<primitive:'primitiveCreateFont' module:'FontPlugin'>
	^self primitiveFailed
]

{ #category : #'font creation' }
StrikeFont class >> primitiveDestroyFont: fontHandle [
	<primitive:'primitiveDestroyFont' module:'FontPlugin'>
	^self primitiveFailed
]

{ #category : #'font creation' }
StrikeFont class >> primitiveFont: fontHandle glyphOfChar: charIndex into: glyphForm [
	<primitive:'primitiveFontGlyphOfChar' module:'FontPlugin'>
	^self primitiveFailed
]

{ #category : #'font creation' }
StrikeFont class >> primitiveFont: fontHandle widthOfChar: charIndex [
	<primitive:'primitiveFontWidthOfChar' module:'FontPlugin'>
	^self primitiveFailed
]

{ #category : #'font creation' }
StrikeFont class >> primitiveFontAscent: fontHandle [
	<primitive:'primitiveFontAscent' module:'FontPlugin'>
	^self primitiveFailed
]

{ #category : #'font creation' }
StrikeFont class >> primitiveFontDescent: fontHandle [
	<primitive:'primitiveFontDescent' module:'FontPlugin'>
	^self primitiveFailed
]

{ #category : #'font creation' }
StrikeFont class >> primitiveFontEncoding: fontHandle [
	<primitive:'primitiveFontEncoding' module:'FontPlugin'>
	^self primitiveFailed
]

{ #category : #examples }
StrikeFont class >> readStrikeFont2Family: familyName [ 
	"StrikeFont readStrikeFont2Family: 'Lucida'"
	^self readStrikeFont2Family: familyName fromDirectory: FileSystem workingDirectory
]

{ #category : #examples }
StrikeFont class >> readStrikeFont2Family: familyName fromDirectory: aDirectory [ 
	"StrikeFont readStrikeFont2Family: 'Lucida' fromDirectory: FileDirectory default"
	"This utility reads all available .sf2 StrikeFont files for a given family from  
	the current directory. It returns an Array, sorted by size, suitable for handing 
	to TextStyle newFontArray: ."
	"For this utility to work as is, the .sf2 files must be named 'familyNN.sf2'."
	| files strikeFonts fontArray |
	files := aDirectory filesMatching: familyName , '##.sf2'.
	strikeFonts := files collect: [ :file | StrikeFont new readFromStrike2: file basename ].
	strikeFonts do: [ :font | font reset ].
	strikeFonts := strikeFonts asSortedCollection: [ :a :b | a height < b height ].
	fontArray := strikeFonts asArray.
	^ fontArray

	"TextConstants at: #Lucida put: (TextStyle fontArray: (StrikeFont 
	readStrikeFont2Family: 'Lucida'))."
]

{ #category : #removing }
StrikeFont class >> saveSpace [
	"Removes glyphs over 128, leaving only lower ascii.
	Also limit glyph depth to 4 bits (it is usually 16 or 32).
	This effectively turns off subpixel rendering, as glyphs will only have 16 shades of gray.
	
	StrikeFont saveSpace
	"
	StrikeFont allInstances do: [ :f | f
		stripHighGlyphs;
		setGlyphsDepthAtMost: 4 ].
]

{ #category : #accessing }
StrikeFont class >> setupDefaultFallbackFont [
	"
	StrikeFont setupDefaultFallbackFont
	"
	(self defaultFallbackFontKeys
		collect: [:e | TextStyle named: e])
		do: [:style | style fontArray
				do: [:e | 
					e reset.
					(e respondsTo: #fontArray)
						ifTrue: [e fontArray 
								do: [:f | f setupDefaultFallbackFont]]]].

]

{ #category : #'system startup' }
StrikeFont class >> shutDown [
	"StrikeFont shutDown"
	"Deallocate synthetically derived copies of base fonts to save space"
	self allSubInstancesDo: [ :sf | sf reset ].
	StrikeFontSet allSubInstancesDo: [ :sf | sf reset ].

]

{ #category : #'character shapes' }
StrikeFont class >> useUnderscoreIfOver1bpp [
	"Sets underscore and caret glyphs for chars 95 and 94. 
	Only for enhanced StrikeFonts, i.e. those with glyphs of more than 1bpp.
	ASCII standard glyphs"
	"
	StrikeFont useUnderscoreIfOver1bpp
	"
	self allInstances do: [ :font | font useUnderscoreIfOver1bpp ]
]

{ #category : #'character shapes' }
StrikeFont >> alter: char formBlock: formBlock [
	self characterFormAt: char 
		put: (formBlock value: (self characterFormAt: char))
]

{ #category : #accessing }
StrikeFont >> ascent [
	"Answer the receiver's maximum extent of characters above the baseline."

	^ascent
]

{ #category : #accessing }
StrikeFont >> ascentKern [
	"Return the kern delta for ascenders."
	(emphasis noMask: 2) ifTrue: [^ 0].
	^ (self ascent-5+4)//4 max: 0  "See makeItalicGlyphs"


]

{ #category : #accessing }
StrikeFont >> ascentOf: aCharacter [

	(self hasGlyphOf: aCharacter) ifFalse: [
		fallbackFont ifNotNil: [
			^ fallbackFont ascentOf: aCharacter.
		].
	].
	^ self ascent.

]

{ #category : #accessing }
StrikeFont >> baseKern [
	"Return the base kern value to be used for all characters."
	
	| italic |

	italic := emphasis allMask: 2.	
	"Are we a bitmapped font with alpha, whose italic property is not synthetic?"
	(self isSyntheticItalic not and: [ glyphs depth > 1 ]) ifTrue: [
		^(italic or: [ pointSize < 9 ])
			ifTrue: [ 1 ]
			ifFalse: [ 0] ].
	
	italic ifFalse: [^ 0].
	^ ((self height-1-self ascent+4)//4 max: 0)  "See makeItalicGlyphs"
		+ (((self ascent-5+4)//4 max: 0))
]

{ #category : #emphasis }
StrikeFont >> bonk: glyphForm with: bonkForm [ 
	"Bonking means to run through the glyphs clearing out black pixels
	between characters to prevent them from straying into an adjacent
	character as a result of, eg, bolding or italicizing"
	"Uses the bonkForm to erase at every character boundary in glyphs."
	| bb offset x |
	offset := bonkForm offset x.
	bb := BitBlt toForm: glyphForm.
	bb
		sourceForm: bonkForm;
		sourceRect: bonkForm boundingBox;
		combinationRule: Form erase;
		destY: 0.
	x := self xTable.
	(x isMemberOf: SparseLargeTable) 
		ifTrue: 
			[ x base 
				to: x size - 1
				do: 
					[ :i | 
					bb
						destX: (x at: i) + offset;
						copyBits ] ]
		ifFalse: 
			[ 1 
				to: x size - 1
				do: 
					[ :i | 
					bb
						destX: (x at: i) + offset;
						copyBits ] ]
]

{ #category : #building }
StrikeFont >> buildFromForm: allGlyphs data: data name: aString [ 
	| x |
	pointSize := data first.
	ascent := data second.
	descent := data third.
	minAscii := 32.
	maxAscii := 255.
	name := aString.
	type := 0.	"ignored for now"
	superscript := (ascent - descent) // 3.
	subscript := (descent - ascent) // 3.
	emphasis := 0.
	xTable := (Array new: 258) atAllPut: 0.
	maxWidth := 0.
	glyphs := allGlyphs.
	x := 0.
	minAscii 
		to: maxAscii + 1
		do: 
			[ :i | 
			x := data at: i - minAscii + 4.
			xTable 
				at: i + 1
				put: x ].
	xTable 
		at: 258
		put: x.
	self reset.
	derivativeFonts := Array new: 32
]

{ #category : #'file in/out' }
StrikeFont >> buildfontNamed: nm fromForms: forms startingAtAscii: startAscii ascent: a descent: d maxWid: m [ 
	"This builds a StrikeFont instance from existing forms."
	| lastAscii width ascii charForm missingForm tempGlyphs |
	name := nm.
	ascent := 11.
	descent := 3.
	maxWidth := 16.
	pointSize := 8.
	name := (name copyWithout: Character space) , (pointSize < 10 
			ifTrue: [ '0' , pointSize printString ]
			ifFalse: [ pointSize printString ]).
	minAscii := 258.
	maxAscii := 0.
	superscript := (ascent - descent) // 3.
	subscript := (descent - ascent) // 3.
	emphasis := 0.
	type := 0.	"ignored for now"
	tempGlyphs := Form extent: (maxWidth * 257) @ self height.
	xTable := (Array new: 258) atAllPut: 0.
	xTable 
		at: 1
		put: 0.

	"Read character forms and blt into tempGlyphs"
	lastAscii := -1.
	1 
		to: forms size
		do: 
			[ :i | 
			charForm := forms at: i.
			width := charForm width.
			ascii := startAscii - 1 + i.
			self 
				displayChar: ascii
				form: charForm.
			ascii = 256 
				ifTrue: [ missingForm := charForm deepCopy ]
				ifFalse: 
					[ minAscii := minAscii min: ascii.
					maxAscii := maxAscii max: ascii.
					lastAscii + 1 
						to: ascii - 1
						do: 
							[ :as | 
							xTable 
								at: as + 2
								put: (xTable at: as + 1) ].
					tempGlyphs 
						copy: ((xTable at: ascii + 1) @ 0 extent: charForm extent)
						from: 0 @ 0
						in: charForm
						rule: Form over.
					xTable 
						at: ascii + 2
						put: (xTable at: ascii + 1) + width.
					lastAscii := ascii ] ].
	lastAscii + 1 
		to: maxAscii + 1
		do: 
			[ :as | 
			xTable 
				at: as + 2
				put: (xTable at: as + 1) ].
	missingForm == nil ifFalse: 
		[ tempGlyphs 
			copy: missingForm boundingBox
			from: missingForm
			to: (xTable at: maxAscii + 2) @ 0
			rule: Form over.
		xTable 
			at: maxAscii + 3
			put: (xTable at: maxAscii + 2) + missingForm width ].
	glyphs := Form extent: (xTable at: maxAscii + 3) @ self height.
	glyphs 
		copy: glyphs boundingBox
		from: 0 @ 0
		in: tempGlyphs
		rule: Form over.
	xTable := xTable 
		copyFrom: 1
		to: maxAscii + 3.
	characterToGlyphMap := nil
]

{ #category : #'character shapes' }
StrikeFont >> characterForm: char pixelValueAt: pt put: val [ 
	| f |
	f := self characterFormAt: char.
	f 
		pixelAt: pt
		put: val.
	self 
		characterFormAt: char
		put: val
]

{ #category : #'character shapes' }
StrikeFont >> characterFormAt: character [ 
	"Answer a Form copied out of the glyphs for the argument, character."
	| ascii leftX rightX |
	ascii := character charCode.
	(ascii 
		between: minAscii
		and: maxAscii) ifFalse: [ ascii := maxAscii + 1 ].
	leftX := xTable at: ascii + 1.
	rightX := xTable at: ascii + 2.
	leftX < 0 ifTrue: [ ^ glyphs copy: (0 @ 0 corner: 0 @ self height) ].
	^ glyphs copy: (leftX @ 0 corner: rightX @ self height)
]

{ #category : #'character shapes' }
StrikeFont >> characterFormAt: character put: characterForm [ 
	"Copy characterForm over the glyph for the argument, character."
	| ascii leftX rightX widthDif newGlyphs |
	ascii := character asciiValue.
	ascii < minAscii ifTrue: [ ^ self error: 'Cant store characters below min ascii' ].
	ascii > maxAscii ifTrue: 
		[ (self confirm: 'This font does not accomodate ascii values higher than ' , maxAscii printString , '.
Do you wish to extend it permanently to handle values up to ' , ascii printString) 
			ifTrue: [ self extendMaxAsciiTo: ascii ]
			ifFalse: [ ^ self error: 'No change made' ] ].
	leftX := xTable at: ascii + 1.
	rightX := xTable at: ascii + 2.
	widthDif := characterForm width - (rightX - leftX).
	widthDif ~= 0 ifTrue: 
		[ "Make new glyphs with more or less space for this char"
		newGlyphs := Form 
			extent: (glyphs width + widthDif) @ glyphs height
			depth: glyphs depth.
		newGlyphs 
			copy: (0 @ 0 corner: leftX @ glyphs height)
			from: 0 @ 0
			in: glyphs
			rule: Form over.
		newGlyphs 
			copy: ((rightX + widthDif) @ 0 corner: newGlyphs width @ glyphs height)
			from: rightX @ 0
			in: glyphs
			rule: Form over.
		glyphs := newGlyphs.
		"adjust further entries on xTable"
		xTable := xTable copy.
		ascii + 2 
			to: xTable size
			do: 
				[ :i | 
				xTable 
					at: i
					put: (xTable at: i) + widthDif ] ].
	glyphs 
		copy: (leftX @ 0 extent: characterForm extent)
		from: 0 @ 0
		in: characterForm
		rule: Form over
	"
| f |  f := TextStyle defaultFont.
f characterFormAt: $  put: (Form extent: (f widthOf: $ )+10@f height)
"
]

{ #category : #'character shapes' }
StrikeFont >> characterFormAtMulti: character [ 
	"Answer a Form copied out of the glyphs for the argument, character."
	| ascii leftX rightX |
	ascii := character charCode.
	(ascii 
		between: minAscii
		and: maxAscii) ifFalse: [ ascii := maxAscii + 1 ].
	leftX := xTable at: ascii + 1.
	rightX := xTable at: ascii + 2.
	^ glyphs copy: (leftX @ 0 corner: rightX @ self height)
]

{ #category : #accessing }
StrikeFont >> characterToGlyphMap [
	^ characterToGlyphMap ifNil: [ characterToGlyphMap := self createCharacterToGlyphMap ]
]

{ #category : #accessing }
StrikeFont >> characterToGlyphMap: anArray [ 
	characterToGlyphMap := anArray
]

{ #category : #displaying }
StrikeFont >> characters: anInterval in: sourceString displayAt: aPoint clippedBy: clippingRectangle rule: ruleInteger fillColor: aForm kernDelta: kernDelta on: aBitBlt [ 
	"Simple, slow, primitive method for displaying a line of characters.
	No wrap-around is provided."
	| destPoint |
	destPoint := aPoint.
	anInterval do: 
		[ :i | | rightX sourceRect leftX ascii | 
		self flag: #yoDisplay.
		"if the char is not supported, fall back to the specified fontset."
		ascii := (sourceString at: i) charCode.
		(ascii < minAscii or: [ ascii > maxAscii ]) ifTrue: [ ascii := maxAscii ].
		leftX := xTable at: ascii + 1.
		rightX := xTable at: ascii + 2.
		sourceRect := leftX @ 0 extent: (rightX - leftX) @ self height.
		aBitBlt 
			copyFrom: sourceRect
			in: glyphs
			to: destPoint.
		destPoint := destPoint + ((rightX - leftX + kernDelta) @ 0)
		"destPoint printString displayAt: 0@(i*20)" ].
	^ destPoint
]

{ #category : #testing }
StrikeFont >> checkCharacter: character [ 
	"Answer a Character that is within the ascii range of the receiver--either 
	character or the last character in the receiver."
	| ascii |
	ascii := character asciiValue.
	(ascii < minAscii or: [ ascii > maxAscii ]) 
		ifTrue: [ ^ maxAscii asCharacter ]
		ifFalse: [ ^ character ]
]

{ #category : #private }
StrikeFont >> createCharacterToGlyphMap [
        "Private. Create the character to glyph mapping for a font that didn't have any before. This is basically equivalent to what the former setStopCondition did, only based on indexes."

        maxAscii < 256 ifTrue: [^ (1 to: 256) collect: [:i | i - 1]].
        ^ nil.

]

{ #category : #copying }
StrikeFont >> deepCopy [
 " there is a circular reference from the derivative fonts back to the receiver. It is therefore not possible to make a deep copy. We make a sahllow copy. The method postCopy can be used to modify the shallow copy. " 
  ^self copy
]

{ #category : #emphasis }
StrikeFont >> derivativeFont: aStrikeFont at: index [

	| newDeriv |
	(aStrikeFont isNil and: [ index = 0 ]) 
		ifTrue: [derivativeFonts := nil. ^ self].
	derivativeFonts ifNil: [derivativeFonts := Array new: 32].
	derivativeFonts size < 32 ifTrue: [
		newDeriv := Array new: 32.
		newDeriv replaceFrom: 1 to: derivativeFonts size with: derivativeFonts.
		derivativeFonts := newDeriv.
	].
	derivativeFonts at: index put: aStrikeFont.
]

{ #category : #accessing }
StrikeFont >> derivativeFonts [
	^derivativeFonts copyWithout: nil
]

{ #category : #accessing }
StrikeFont >> descent [
	"Answer the receiver's maximum extent of characters below the baseline."

	^pointSize < 9
		ifTrue: [descent-1]
		ifFalse: [descent]
]

{ #category : #accessing }
StrikeFont >> descentKern [
	"Return the kern delta for descenders."
	(emphasis noMask: 2) ifTrue: [^ 0].
	^ (self height-1-self ascent+4)//4 max: 0  "See makeItalicGlyphs"


]

{ #category : #accessing }
StrikeFont >> descentOf: aCharacter [

	(self hasGlyphOf: aCharacter) ifFalse: [
		fallbackFont ifNotNil: [
			^ fallbackFont descentOf: aCharacter.
		].
	].
	^ self descent.

]

{ #category : #'file in/out' }
StrikeFont >> displayChar: ascii form: charForm [ 
	"Convenience utility used during conversion of BitFont files"
	| m bigForm |
	Display fillBlack: (0 @ 0 extent: 20 @ 14).
	ascii printString displayAt: 0 @ 2.
	charForm width > 0 ifTrue: 
		[ m := 5.
		bigForm := charForm 
			magnify: charForm boundingBox
			by: m @ m.
		Display 
			border: ((bigForm boundingBox expandBy: m) translateBy: 50 @ 2)
			width: m.
		bigForm displayAt: 50 @ 2.
		Display fillBlack: (50 @ 2 + ((m * charForm width) @ 0) extent: 1 @ (m * self height)) ]
]

{ #category : #displaying }
StrikeFont >> displayLine: aString at: aPoint [ 
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
StrikeFont >> displayMultiString: aString on: aBitBlt from: startIndex to: stopIndex at: aPoint kern: kernDelta baselineY: baselineY [ 
	| nextWide destX leftX rightX glyphInfo char destY gfont charIndex |
	destX := aPoint x.
	charIndex := startIndex.
	glyphInfo := Array new: 5.
	[ charIndex <= stopIndex ] whileTrue: 
		[ nextWide := aString indexOfWideCharacterFrom: charIndex to: stopIndex.
		nextWide = 0 ifTrue: [nextWide := stopIndex +1].
		nextWide > charIndex ifTrue: [ destX := (aBitBlt displayString: aString 
			from: charIndex 
			to: nextWide -1
			at: destX @ aPoint y
			strikeFont: self
			kern: kernDelta) x.
			charIndex := nextWide]. 
		nextWide > stopIndex ifFalse: [
		char := aString at: charIndex.
		(self hasGlyphOf: char)
			ifTrue: 
				[ self 
					glyphInfoOf: char
					into: glyphInfo.
				leftX := glyphInfo at: 2.
				rightX := glyphInfo at: 3.
				destY := glyphInfo at: 4.
				gfont := glyphInfo at: 5.
				(gfont == aBitBlt lastFont) ifFalse: [gfont installOn: aBitBlt].
				aBitBlt displayGlyph: (glyphInfo at: 1) at: (destX @ (baselineY - destY)) left: leftX right: rightX font: self.
				destX := destX + (rightX - leftX + kernDelta).
				 ] 
			ifFalse: 
				[fallbackFont displayString: aString on: aBitBlt from: charIndex to: charIndex at: destX@destY kern: kernDelta baselineY: baselineY.
				destX := destX + (fallbackFont widthOf: char) + kernDelta.
				charIndex := charIndex + 1]]].
	^ Array 
		with: charIndex
		with: aPoint + (destX @ 0)
]

{ #category : #displaying }
StrikeFont >> displayString: aString on: aBitBlt from: startIndex to: stopIndex at: aPoint kern: kernDelta [
	"Draw the given string from startIndex to stopIndex 
	at aPoint on the (already prepared) BitBlt."
	
		"Somewhat of a hack:
	The scanner seem to only pass in runs of either all ASCII or all non-ASCII characters. If all characters are ascii, then it's safe to use basic rendering-method."
	(aString hasWideCharacterFrom: startIndex to: stopIndex) ifTrue:  [^ self displayMultiString: aString on: aBitBlt from: startIndex to: stopIndex at: aPoint kern: kernDelta baselineY: aPoint y + self ascent.].

	^ aBitBlt displayString: aString 
			from: startIndex 
			to: stopIndex 
			at: aPoint 
			strikeFont: self
			kern: kernDelta.
]

{ #category : #displaying }
StrikeFont >> displayString: aString on: aBitBlt from: startIndex to: stopIndex at: aPoint kern: kernDelta baselineY: baselineY [
	"Draw the given string from startIndex to stopIndex 
	at aPoint on the (already prepared) BitBlt."
	
	"Somewhat of a hack:
	The scanner seem to only pass in runs of either all ASCII or all non-ASCII characters. If all characters are ascii, then it's safe to use basic rendering-method."
	(aString hasWideCharacterFrom: startIndex to: stopIndex) ifTrue: [^self displayMultiString: aString 
						on: aBitBlt 
						from: startIndex 
						to: stopIndex 
						at: aPoint 
						kern: kernDelta 
						baselineY: baselineY.].

	^ aBitBlt displayString: aString 
			from: startIndex 
			to: stopIndex
			at: aPoint 
			strikeFont: self
			kern: kernDelta.
]

{ #category : #emphasis }
StrikeFont >> emphasis [
	"Answer the integer code for synthetic bold, italic, underline, and 
	strike-out."

	^emphasis
]

{ #category : #emphasis }
StrikeFont >> emphasis: code [ 
	"Set the integer code for synthetic bold, itallic, underline, and strike-out, 
	where bold=1, italic=2, underlined=4, and struck out=8."
	emphasis := code
]

{ #category : #emphasis }
StrikeFont >> emphasized: code [ 
	"Answer a copy of the receiver with emphasis set to include code."
	| derivative addedEmphasis base safeCode |
	code = 0 ifTrue: [ ^ self ].
	(derivativeFonts == nil or: [ derivativeFonts size = 0 ]) ifTrue: [ ^ self ].
	derivative := derivativeFonts at: (safeCode := code min: derivativeFonts size).
	derivative == nil ifFalse: [ ^ derivative ].	"Already have this style"

	"Dont have it -- derive from another with one with less emphasis"
	addedEmphasis := 1 bitShift: safeCode highBit - 1.
	base := self emphasized: safeCode - addedEmphasis.	"Order is Bold, Ital, Under, Narrow"
	addedEmphasis = 1 ifTrue: 
		[ "Compute synthetic bold version of the font"
		derivative := (base copy ensureCleanBold name: base name , 'B') makeBoldGlyphs ].
	addedEmphasis = 2 ifTrue: 
		[ "Compute synthetic italic version of the font"
		derivative := (base copy name: base name , 'I') makeItalicGlyphs ].
	addedEmphasis = 4 ifTrue: 
		[ "Compute underlined version of the font"
		derivative := (base copy name: base name , 'U') makeUnderlinedGlyphs ].
	addedEmphasis = 8 ifTrue: 
		[ "Compute narrow version of the font"
		derivative := (base copy name: base name , 'N') makeCondensedGlyphs ].
	addedEmphasis = 16 ifTrue: 
		[ "Compute struck-out version of the font"
		derivative := (base copy name: base name , 'X') makeStruckOutGlyphs ].
	derivative emphasis: safeCode.
	derivativeFonts 
		at: safeCode
		put: derivative.
	^ derivative
]

{ #category : #'character shapes' }
StrikeFont >> ensureCleanBold [
	"This ensures that all character glyphs have at least one pixel of white space on the right
	so as not to cause artifacts in neighboring characters in bold or italic."
	| wider glyph |
	emphasis = 0 ifFalse: [ ^ self ].
	minAscii 
		to: maxAscii
		do: 
			[ :i | 
			glyph := self characterFormAt: (Character value: i).
			(glyph copy: (glyph boundingBox topRight - (1 @ 0) corner: glyph boundingBox bottomRight)) isAllWhite ifFalse: 
				[ wider := Form 
					extent: (glyph width + 1) @ glyph height
					depth: glyph depth.
				glyph depth > 1 ifTrue: [ wider fillWhite ].
				glyph displayOn: wider.
				self 
					characterFormAt: (Character value: i)
					put: wider ] ]
	"
StrikeFont allInstancesDo: [:f | f ensureCleanBold].
(StrikeFont familyName: 'NewYork' size: 21) ensureCleanBold.
StrikeFont shutDown.  'Flush synthetic fonts'.
"
]

{ #category : #'character shapes' }
StrikeFont >> extendMaxAsciiTo: newMax [ 
	"Extend the range of this font so that it can display glyphs up to newMax."
	newMax + 3 <= xTable size ifTrue: [ ^ self ].	"No need to extend."
	xTable size = (maxAscii + 3) ifFalse: [ ^ self error: 'This font is not well-formed.' ].

	"Insert a bunch of zero-width characters..."
	xTable := (xTable 
		copyFrom: 1
		to: maxAscii + 2) , ((maxAscii + 1 to: newMax) collect: [ :i | xTable at: maxAscii + 2 ]) , {  (xTable at: maxAscii + 3)  }.
	maxAscii := newMax.
	self fillZeroWidthSlots.
	characterToGlyphMap := nil
]

{ #category : #accessing }
StrikeFont >> fallbackFont [
	^ fallbackFont ifNil: [ fallbackFont := FixedFaceFont new errorFont fontSize: self height ]
]

{ #category : #accessing }
StrikeFont >> fallbackFont: aFontSetOrNil [ 
	fallbackFont := aFontSetOrNil
]

{ #category : #accessing }
StrikeFont >> familyName [
	| nameString firstDigit |
	nameString := self name.
	firstDigit := nameString findFirst: [ :m | m isDigit ].
	^ firstDigit > 0
		ifTrue: [ (nameString copyFrom: 1 to: firstDigit - 1) trimRight ]
		ifFalse: [ nameString ]
]

{ #category : #accessing }
StrikeFont >> familySizeFace [
	"Answer an array with familyName, a String, pointSize, an Integer, and
	faceCode, an Integer."

	^Array with: name
		with: self height
		with: emphasis

	"(1 to: 12) collect: [:x | (TextStyle default fontAt: x) familySizeFace]"
]

{ #category : #'character shapes' }
StrikeFont >> fillZeroWidthSlots [
	"Note: this is slow because it copies the font once for every replacement."
	| nullGlyph |
	nullGlyph := (Form extent: 1 @ glyphs height) fillGray.
	"Now fill the empty slots with narrow box characters."
	minAscii 
		to: maxAscii
		do: 
			[ :i | 
			(self widthOf: (Character value: i)) = 0 ifTrue: 
				[ self 
					characterFormAt: (Character value: i)
					put: nullGlyph ] ]
]

{ #category : #'multibyte character methods' }
StrikeFont >> fixAscent: a andDescent: d head: h [ 
	"(a + d) = (ascent + descent) ifTrue: ["
	| bb newGlyphs |
	ascent := a.
	descent := d.
	newGlyphs := Form extent: glyphs width @ (h + glyphs height).
	bb := BitBlt toForm: newGlyphs.
	bb 
		copy: (0 @ h extent: glyphs extent)
		from: 0 @ 0
		in: glyphs
		fillColor: nil
		rule: Form over.
	glyphs := newGlyphs
	"]."
]

{ #category : #'character shapes' }
StrikeFont >> fixOneWideChars [
	"This fixes all 1-wide characters to be 2 wide with blank on the right
	so as not to cause artifacts in neighboring characters in bold or italic."
	| twoWide |
	minAscii 
		to: maxAscii
		do: 
			[ :i | 
			(self widthOf: (Character value: i)) = 1 ifTrue: 
				[ twoWide := Form extent: 2 @ glyphs height.
				(self characterFormAt: (Character value: i)) 
					displayOn: twoWide
					at: 0 @ 0.
				self 
					characterFormAt: (Character value: i)
					put: twoWide ] ]
	"
StrikeFont allInstancesDo: [:f | f fixOneWideChars].
StrikeFont shutDown.  'Flush synthetic fonts'.
"
]

{ #category : #'multibyte character methods' }
StrikeFont >> fixXTable [
	| newXTable val |
	xTable size >= 258 ifTrue: [ ^ self ].
	newXTable := Array new: 258.
	1 
		to: xTable size
		do: 
			[ :i | 
			newXTable 
				at: i
				put: (xTable at: i) ].
	val := xTable at: xTable size.
	xTable size + 1 
		to: 258
		do: 
			[ :i | 
			newXTable 
				at: i
				put: val ].
	minAscii := 0.
	maxAscii := 255.
	xTable := newXTable
]

{ #category : #accessing }
StrikeFont >> fontNameWithPointSize [
	^self name withoutTrailingDigits, ' ', self pointSize printString
]

{ #category : #accessing }
StrikeFont >> glyphInfoOf: aCharacter into: glyphInfoArray [
	"Answer the width of the argument as a character in the receiver."

	| code |
	(self hasGlyphOf: aCharacter) ifFalse: [
		fallbackFont ifNotNil: [
			^ fallbackFont glyphInfoOf: aCharacter into: glyphInfoArray.
		].
		code := 0.
	] ifTrue: [
		code := characterToGlyphMap 
				ifNotNil: [:map | map at: aCharacter charCode +1] 
				ifNil: [aCharacter charCode].
	].
	glyphInfoArray at: 1 put: glyphs;
		at: 2 put: (xTable at: code + 1);
		at: 3 put: (xTable at: code + 2);
		at: 4 put: (self ascentOf: aCharacter);
		at: 5 put: self.
	^ glyphInfoArray.

]

{ #category : #accessing }
StrikeFont >> glyphOf: aCharacter [ 
	"Answer the width of the argument as a character in the receiver."
	| code |
	(self hasGlyphOf: aCharacter) ifFalse: 
		[ fallbackFont ifNotNil: [ ^ fallbackFont glyphOf: aCharacter ].
		^ (Form extent: 1 @ self height) fillColor: Color white ].
	code := aCharacter charCode.
	^ glyphs copy: ((xTable at: code + 1) @ 0 corner: (xTable at: code + 2) @ self height)
]

{ #category : #accessing }
StrikeFont >> glyphs [
	"Answer a Form containing the bits representing the characters of the 
	receiver."

	^glyphs
]

{ #category : #'multibyte character methods' }
StrikeFont >> hasGlyphOf: aCharacter [
	| code |
	code := aCharacter charCode.
	(code between: self minAscii and: self maxAscii)
		ifFalse: [ ^ false ].
	characterToGlyphMap ifNotNil: [ :map | code := map at: code + 1 ].
	(xTable at: code + 1) < 0
		ifTrue: [ ^ false ].
	^ true
]

{ #category : #accessing }
StrikeFont >> height [
	"Answer the height of the receiver, total of maximum extents of 
	characters above and below the baseline."

	^self ascent + self descent
]

{ #category : #accessing }
StrikeFont >> heightOf: aCharacter [

	(self hasGlyphOf: aCharacter) ifFalse: [
		fallbackFont ifNotNil: [
			^ fallbackFont heightOf: aCharacter.
		].
	].
	^ self height.

]

{ #category : #displaying }
StrikeFont >> installOn: aDisplayContext [

	^aDisplayContext installStrikeFont: self.

]

{ #category : #displaying }
StrikeFont >> installOn: aDisplayContext foregroundColor: foregroundColor backgroundColor: backgroundColor [
	^aDisplayContext 
		installStrikeFont: self
		foregroundColor: foregroundColor 
		backgroundColor: backgroundColor
]

{ #category : #emphasis }
StrikeFont >> isSynthetic [
	^type > 0
]

{ #category : #emphasis }
StrikeFont >> isSynthetic: aBoolean [ 
	type := aBoolean 
		ifTrue: [ 3 ]
		ifFalse: [ 0 ]
]

{ #category : #emphasis }
StrikeFont >> isSyntheticItalic [
	^type = 5
]

{ #category : #emphasis }
StrikeFont >> isSyntheticItalic: aBoolean [ 
	aBoolean 
		ifTrue: [type :=  5]
		
]

{ #category : #accessing }
StrikeFont >> lineGrid [
	^ ascent + descent
]

{ #category : #'make arrows' }
StrikeFont >> makeAssignArrow [
	"Replace the underline character with an arrow for this font"
	| arrowForm arrowCanvas arrowY arrowLeft arrowRight arrowHeadLength |
	arrowForm := (self characterFormAt: $_) copy.
	arrowCanvas := arrowForm getCanvas.
	arrowCanvas fillColor: Color white.
	arrowY := arrowForm height // 2.
	arrowLeft := 0.
	arrowRight := arrowForm width - 2.
	arrowHeadLength := (arrowRight - arrowLeft) * 2 // 5.
	"Draw the lines"
	arrowCanvas 
		line: arrowLeft @ arrowY
		to: arrowRight @ arrowY
		color: Color black.
	arrowCanvas 
		line: arrowLeft @ arrowY
		to: (arrowLeft + arrowHeadLength) @ (arrowY - arrowHeadLength)
		color: Color black.
	arrowCanvas 
		line: arrowLeft @ arrowY
		to: (arrowLeft + arrowHeadLength) @ (arrowY + arrowHeadLength)
		color: Color black.

	"Replace the glyph"
	self 
		characterFormAt: $_
		put: arrowForm
]

{ #category : #emphasis }
StrikeFont >> makeBoldGlyphs [
	"Make a bold set of glyphs with same widths by ORing 1 bit to the right
		(requires at least 1 pixel of intercharacter space)"
	| g bonkForm |
	g := glyphs deepCopy.
	bonkForm := (Form extent: 1 @ 16) fillBlack offset: -1 @ 0.
	self 
		bonk: g
		with: bonkForm.
	glyphs depth = 1 
		ifTrue: 
			[ g 
				copyBits: g boundingBox
				from: g
				at: 1 @ 0
				clippingBox: g boundingBox
				rule: Form under
				fillColor: nil ]
		ifFalse: 
			[ 0 
				to: g width - 2
				do: 
					[ :x | 
					0 
						to: g height - 1
						do: 
							[ :y | 
							(glyphs colorAt: x @ y) = Color white ifFalse: 
								[ g 
									colorAt: (x + 1) @ y
									put: ((glyphs colorAt: (x + 1) @ y) = Color white 
											ifTrue: [ glyphs colorAt: x @ y ]
											ifFalse: [ Color black ]) ] ] ] ].
	glyphs := g.
	self isSynthetic: true.
	fallbackFont ifNotNil: [ fallbackFont := fallbackFont emphasized: 1 ]
]

{ #category : #'character shapes' }
StrikeFont >> makeCarriageReturnsWhite [
	"Some larger fonts have a gray carriage return (from the zero wide fixup) make it white so it doesn't show"
	| crForm |
	crForm := self characterFormAt: 13 asCharacter.
	crForm fillWhite.
	self 
		characterFormAt: 13 asCharacter
		put: crForm
]

{ #category : #emphasis }
StrikeFont >> makeCondensedGlyphs [
	"Make a condensed set of glyphs with same widths.
	NOTE: this has been superceded by kerning -- should not get called"
	| g newXTable x x1 w |
	g := glyphs deepCopy.
	newXTable := Array new: xTable size.
	newXTable 
		at: 1
		put: (x := xTable at: 1).
	1 
		to: xTable size - 1
		do: 
			[ :i | 
			x1 := xTable at: i.
			w := (xTable at: i + 1) - x1.
			w > 1 ifTrue: [ w := w - 1 ].	"Shrink every character wider than 1"
			g 
				copy: (x @ 0 extent: w @ g height)
				from: x1 @ 0
				in: glyphs
				rule: Form over.
			newXTable 
				at: i + 1
				put: (x := x + w) ].
	xTable := newXTable.
	glyphs := g.
	self isSynthetic: true.
	fallbackFont ifNotNil: [ fallbackFont emphasized: 8 ]

	"
(TextStyle default fontAt: 1) copy makeCondensedGlyphs
	displayLine: 'The quick brown fox jumps over the lazy dog'
	at: Sensor cursorPoint
"
]

{ #category : #'character shapes' }
StrikeFont >> makeControlCharsVisible [
	| glyph |
	self characterToGlyphMap.
	glyph := self characterFormAt: Character space.
	glyph 
		border: glyph boundingBox
		width: 1
		fillColor: Color blue.
	self 
		characterFormAt: (Character value: 133)
		put: glyph.

	"Keep tab(9), lf(10), cr(13) and space(32) transparent or whatever the user chose"
	#(
		0
		1
		2
		3
		4
		5
		6
		7
		8
		11
		12
		14
		15
		16
		17
		18
		19
		20
		21
		22
		23
		24
		25
		26
		27
		28
		29
		30
		31
	) do: 
		[ :ascii | 
		characterToGlyphMap 
			at: ascii + 1
			put: 133 ]
]

{ #category : #emphasis }
StrikeFont >> makeItalicGlyphs [
	"Make an italic set of glyphs with same widths by skewing left and right.
	In the process, characters would overlap, so we widen them all first.
	"
	| extraWidth newGlyphs newXTable x newX w extraOnLeft |
	extraOnLeft := (self height - 1 - self ascent + 4) // 4 max: 0.
	extraWidth := ((self ascent - 5 + 4) // 4 max: 0) + extraOnLeft.
	newGlyphs := Form 
		extent: (glyphs width + ((maxAscii + 1 - minAscii) * extraWidth)) @ glyphs height
		depth: glyphs depth.
	newGlyphs fillWhite.
	newXTable := xTable copy.

	"Copy glyphs into newGlyphs with room on left and right for overlap."
	minAscii 
		to: maxAscii + 1
		do: 
			[ :ascii | 
			x := xTable at: ascii + 1.
			w := (xTable at: ascii + 2) - x.
			newX := newXTable at: ascii + 1.
			newGlyphs 
				copy: ((newX + extraOnLeft) @ 0 extent: w @ glyphs height)
				from: x @ 0
				in: glyphs
				rule: Form over.
			newXTable 
				at: ascii + 2
				put: newX + w + extraWidth ].
	glyphs := newGlyphs.
	xTable := newXTable.
	"Slide the bitmaps left and right for synthetic italic effect."
	4 
		to: self ascent - 1
		by: 4
		do: 
			[ :y | 
			"Slide ascenders right..."
			glyphs 
				copy: (1 @ 0 extent: glyphs width @ (self ascent - y))
				from: 0 @ 0
				in: glyphs
				rule: Form over ].
	self ascent 
		to: self height - 1
		by: 4
		do: 
			[ :y | 
			"Slide descenders left..."
			glyphs 
				copy: (0 @ y extent: glyphs width @ glyphs height)
				from: 1 @ y
				in: glyphs
				rule: Form over ].
	self isSyntheticItalic: true.
	fallbackFont ifNotNil: [ fallbackFont := fallbackFont emphasized: 2 ]
]

{ #category : #'character shapes' }
StrikeFont >> makeLfInvisible [
	self characterToGlyphMap.
	characterToGlyphMap at: 11 put: (11 < minAscii ifFalse: [11] ifTrue: [maxAscii+1])
]

{ #category : #'character shapes' }
StrikeFont >> makeLfVisible [
	| glyph |
	self characterToGlyphMap.
	glyph := self characterFormAt: (Character value: 163).
	glyph 
		border: glyph boundingBox
		width: 1
		fillColor: Color blue.
	"	glyph := glyph reverse."
	self 
		characterFormAt: (Character value: 132)
		put: glyph.
	characterToGlyphMap 
		at: 11
		put: 132
]

{ #category : #'make arrows' }
StrikeFont >> makeReturnArrow [
	"Replace the caret character with an arrow"
	| arrowForm arrowCanvas arrowHeadLength arrowX arrowTop arrowBottom |
	arrowForm := (self characterFormAt: $^) copy.
	arrowCanvas := arrowForm getCanvas.
	arrowCanvas fillColor: Color white.
	arrowHeadLength := (arrowForm width - 2) // 2.
	arrowX := arrowHeadLength max: arrowForm width // 2.
	arrowTop := arrowForm height // 4.
	arrowBottom := arrowTop + (arrowForm width * 4 // 5).
	arrowBottom := (arrowBottom min: arrowForm height) max: arrowForm height * 2 // 3.

	"Draw the lines"
	arrowCanvas 
		line: arrowX @ arrowTop
		to: arrowX @ arrowBottom
		color: Color black.
	arrowCanvas 
		line: arrowX @ arrowTop
		to: (arrowX - arrowHeadLength) @ (arrowTop + arrowHeadLength)
		color: Color black.
	arrowCanvas 
		line: arrowX @ arrowTop
		to: (arrowX + arrowHeadLength) @ (arrowTop + arrowHeadLength)
		color: Color black.

	"Replace the glyph"
	self 
		characterFormAt: $^
		put: arrowForm
]

{ #category : #emphasis }
StrikeFont >> makeStruckOutGlyphs [
	"Make a struck-out set of glyphs with same widths"
	| g |
	g := glyphs deepCopy.
	g fillBlack: (0 @ (self ascent - (self ascent // 3)) extent: g width @ 1).
	glyphs := g.
	self isSynthetic: true.
	fallbackFont ifNotNil: [ fallbackFont := fallbackFont emphasized: 16 ]
]

{ #category : #'character shapes' }
StrikeFont >> makeTabInvisible [
	self characterToGlyphMap.
	characterToGlyphMap at: 10 put: (10 < minAscii ifFalse: [10] ifTrue:[maxAscii+1])
]

{ #category : #'character shapes' }
StrikeFont >> makeTabVisible [
	self characterToGlyphMap.
	characterToGlyphMap at: 10 put: 172
]

{ #category : #emphasis }
StrikeFont >> makeUnderlinedGlyphs [
	"Make an underlined set of glyphs with same widths"
	| g |
	g := glyphs deepCopy.
	g fillBlack: (0 @ (self ascent + 1) extent: g width @ 1).
	glyphs := g.
	self isSynthetic: true.
	fallbackFont ifNotNil: [ fallbackFont := fallbackFont emphasized: 4 ]
]

{ #category : #accessing }
StrikeFont >> maxAscii [
	"Answer the integer that is the last Ascii character value of the receiver."

	^maxAscii
]

{ #category : #accessing }
StrikeFont >> maxWidth [
	"Answer the integer that is the width of the receiver's widest character."

	^maxWidth
]

{ #category : #accessing }
StrikeFont >> minAscii [
	"Answer the integer that is the first Ascii character value of the receiver."

	^minAscii
]

{ #category : #accessing }
StrikeFont >> name [
	"Answer the receiver's name."

	^name ifNil: ['(unnamed)']
]

{ #category : #accessing }
StrikeFont >> name: aString [ 
	"Set the receiver's name."
	name := aString
]

{ #category : #'file in/out' }
StrikeFont >> newFromStrike: fileName [ 
	"Build an instance from the strike font file name. The '.strike' extension
	is optional."
	| strike startName raster16  |
	name := fileName copyUpTo: $..	"assumes extension (if any) is '.strike'"
	strike := File openForReadFileNamed: name , '.strike.'.

	"strip off direcory name if any"
	startName := name size.
	
	[ startName > 0 and: [ (name at: startName) ~= $> & ((name at: startName) ~= $]) ] ] whileTrue: [ startName := startName - 1 ].
	name := name 
		copyFrom: startName + 1
		to: name size.
	type := strike nextWord.	"type is ignored now -- simplest
												assumed.  Kept here to make
												writing and consistency more
												straightforward."
	minAscii := strike nextWord.
	maxAscii := strike nextWord.
	maxWidth := strike nextWord.
	strike nextWord. "strikeLength"
	ascent := strike nextWord.
	descent := strike nextWord.
	strike nextWord. "xOffset"
	raster16 := strike nextWord.
	superscript := (ascent - descent) // 3.
	subscript := (descent - ascent) // 3.
	emphasis := 0.
	glyphs := Form 
		extent: (raster16 * 16) @ self height
		offset: 0 @ 0.
	glyphs bits fromByteStream: strike.
	xTable := (Array new: maxAscii + 3) atAllPut: 0.
	(minAscii + 1 to: maxAscii + 3) do: 
		[ :index | 
		xTable 
			at: index
			put: strike nextWord ].

	"Set up space character"
	((xTable at: Space asciiValue + 2) = 0 or: [ (xTable at: Space asciiValue + 2) = (xTable at: Space asciiValue + 1) ]) ifTrue: 
		[ Space asciiValue + 2 
			to: xTable size
			do: 
				[ :index | 
				xTable 
					at: index
					put: (xTable at: index) + DefaultSpace ] ].
	strike close.
	characterToGlyphMap := nil
]

{ #category : #accessing }
StrikeFont >> pointSize [
	^ pointSize
]

{ #category : #accessing }
StrikeFont >> pointSize: anInteger [ 
	pointSize := anInteger
]

{ #category : #copying }
StrikeFont >> postCopy [
 " the receiver is a just created shallow copy. This method gives it the final touch. " 
 
    glyphs := glyphs copy.
    xTable := xTable copy.
    characterToGlyphMap := characterToGlyphMap copy.
 
    self reset.  " takes care of the derivative fonts "
]

{ #category : #printing }
StrikeFont >> printOn: aStream [
	super printOn: aStream.
	aStream
		nextPut: $(;
		nextPutAll: self name;
		space;
		print: self height;
		nextPut: $)
]

{ #category : #accessing }
StrikeFont >> raster [
	"Answer an integer that specifies the layout of the glyphs' form."

	^raster
]

{ #category : #'file in/out' }
StrikeFont >> readBFHeaderFrom: f [ 
	name := self 
		restOfLine: 'Font name = '
		from: f.
	ascent := (self 
		restOfLine: 'Ascent = '
		from: f) asNumber.
	descent := (self 
		restOfLine: 'Descent = '
		from: f) asNumber.
	maxWidth := (self 
		restOfLine: 'Maximum width = '
		from: f) asNumber.
	pointSize := (self 
		restOfLine: 'Font size = '
		from: f) asNumber.
	name := (name copyWithout: Character space) , (pointSize < 10 
			ifTrue: [ '0' , pointSize printString ]
			ifFalse: [ pointSize printString ]).
	minAscii := 258.
	maxAscii := 0.
	superscript := (ascent - descent) // 3.
	subscript := (descent - ascent) // 3.
	emphasis := 0.
	type := 0	"ignored for now"
]

{ #category : #'multibyte character methods' }
StrikeFont >> readCharacter: aBits from: aStream [ 
	| pos |
	pos := 0.
	12 timesRepeat: 
		[ 1 
			to: 2
			do: 
				[ :w | 
				aBits 
					byteAt: pos + w
					put: aStream next ].
		pos := pos + 4 ]
]

{ #category : #'file in/out' }
StrikeFont >> readFromBitFont: fileName [ 
	"This builds a StrikeFont instance by reading the data file format
	produced by BitFont, a widely available font conversion utility
	written by Peter DiCamillo at Brown University"
	"StrikeFont new readFromBitFont: 'Palatino10.BF' "
	| f lastAscii charLine width ascii charForm line missingForm tempGlyphs iRect p rectLine left tokens right |
	f := File openForReadFileNamed: fileName.
	self readBFHeaderFrom: f.

	"NOTE: if font has been scaled (and in any case),
	the REAL bitmap dimensions come after the header."
	self 
		restOfLine: 'Extent information for entire font'
		from: f.
	"Parse the following line (including mispelling!)"
	"Image rectange: left = -2, right = 8, bottom = -2, top = 7"
	tokens := f nextLine findTokens: ' '.
	iRect := Rectangle 
		left: (tokens at: 5) asNumber
		right: (tokens at: 8) asNumber
		top: (tokens at: 14) asNumber
		bottom: (tokens at: 11) asNumber.
	ascent := iRect top.
	descent := iRect bottom negated.
	tempGlyphs := Form extent: (maxWidth * 257) @ self height.
	xTable := (Array new: 258) atAllPut: 0.
	xTable 
		at: 1
		put: 0.

	"Read character forms and blt into tempGlyphs"
	lastAscii := -1.
	
	[ charLine := self 
		restOfLine: 'Character: '
		from: f.
	charLine == nil ifFalse: 
		[ p := f position.
		rectLine := f nextLine.
		(rectLine beginsWith: 'Image rectange: left = ') 
			ifTrue: 
				[ tokens := rectLine findTokens: ' '.
				left := (tokens at: 5) asNumber.
				right := (tokens at: 8) asNumber ]
			ifFalse: 
				[ left := right := 0.
				f position: p ].
		width := (self 
			restOfLine: 'Width (final pen position) = '
			from: f) asNumber - left max: right - left + 1.
		(charLine beginsWith: 'Missing character') ifTrue: [ ascii := 256 ].
		('x''*' match: charLine) ifTrue: 
			[ ascii := Number 
				readFrom: (charLine 
					copyFrom: 3
					to: 4) asUppercase
				base: 16 ].
		charForm := Form extent: width @ self height.
		('*[all blank]' match: charLine) ifFalse: 
			[ self 
				restOfLine: '  +'
				from: f.
			1 
				to: self height
				do: 
					[ :y | 
					line := f nextLine.
					4 
						to: (width + 3 min: line size + iRect left - left)
						do: 
							[ :x | 
							(line at: x - iRect left + left) = $* ifTrue: 
								[ charForm 
									pixelValueAt: (x - 4) @ (y - 1)
									put: 1 ] ] ] ] ].
	charLine == nil ] whileFalse: 
		[ self 
			displayChar: ascii
			form: charForm.
		ascii = 256 
			ifTrue: [ missingForm := charForm deepCopy ]
			ifFalse: 
				[ minAscii := minAscii min: ascii.
				maxAscii := maxAscii max: ascii.
				lastAscii + 1 
					to: ascii - 1
					do: 
						[ :a | 
						xTable 
							at: a + 2
							put: (xTable at: a + 1) ].
				tempGlyphs 
					copy: ((xTable at: ascii + 1) @ 0 extent: charForm extent)
					from: 0 @ 0
					in: charForm
					rule: Form over.
				xTable 
					at: ascii + 2
					put: (xTable at: ascii + 1) + width.
				lastAscii := ascii ] ].
	f close.
	lastAscii + 1 
		to: maxAscii + 1
		do: 
			[ :a | 
			xTable 
				at: a + 2
				put: (xTable at: a + 1) ].
	missingForm == nil ifFalse: 
		[ tempGlyphs 
			copy: missingForm boundingBox
			from: missingForm
			to: (xTable at: maxAscii + 2) @ 0
			rule: Form over.
		xTable 
			at: maxAscii + 3
			put: (xTable at: maxAscii + 2) + missingForm width ].
	glyphs := Form extent: (xTable at: maxAscii + 3) @ self height.
	glyphs 
		copy: glyphs boundingBox
		from: 0 @ 0
		in: tempGlyphs
		rule: Form over.
	xTable := xTable 
		copyFrom: 1
		to: maxAscii + 3.
	characterToGlyphMap := nil
]

{ #category : #'file in/out' }
StrikeFont >> readFromStrike2: fileName [ 
	"StrikeFont new readFromStrike2: 'Palatino14.sf2'"
	"Build an instance from the strike font stored in strike2 format.
	fileName is of the form: <family name><pointSize>.sf2"
	| file |
	self assert:['*.sf2' match: fileName]. "likely incompatible"
	name := fileName copyUpTo: $..	"Drop filename extension"
	file := File openForReadFileNamed: fileName.
	[ self readFromStrike2Stream: file ] ensure: [ file close ]
]

{ #category : #'file in/out' }
StrikeFont >> readFromStrike2Stream: file [ 
	"Build an instance from the supplied binary stream on data in strike2 format"
	type := file nextInt32.
	type = 2 ifFalse: 
		[ file close.
		self error: 'not strike2 format' ].
	minAscii := file nextInt32.
	maxAscii := file nextInt32.
	maxWidth := file nextInt32.
	ascent := file nextInt32.
	descent := file nextInt32.
	pointSize := file nextInt32.
	superscript := (ascent - descent) // 3.
	subscript := (descent - ascent) // 3.
	emphasis := file nextInt32.
	xTable := (Array new: maxAscii + 3) atAllPut: 0.
	(minAscii + 1 to: maxAscii + 3) do: 
		[ :index | 
		xTable 
			at: index
			put: file nextInt32 ].
	glyphs := Form new readFrom: file.

	"Set up space character"
	((xTable at: Space asciiValue + 2) = 0 or: [ (xTable at: Space asciiValue + 2) = (xTable at: Space asciiValue + 1) ]) ifTrue: 
		[ Space asciiValue + 2 
			to: xTable size
			do: 
				[ :index | 
				xTable 
					at: index
					put: (xTable at: index) + DefaultSpace ] ].
	characterToGlyphMap := nil
]

{ #category : #emphasis }
StrikeFont >> releaseCachedState [

	self reset.
]

{ #category : #initialization }
StrikeFont >> reset [
	"Reset the cache of derivative emphasized fonts"
	fallbackFont class = FixedFaceFont ifTrue: [ fallbackFont := nil ].
	derivativeFonts notNil ifTrue: 
		[ derivativeFonts withIndexDo: 
			[ :f :i | 
			(f notNil and: [ f isSynthetic ]) ifTrue: 
				[ derivativeFonts 
					at: i
					put: nil ] ] ]
	"
	derivativeFonts := Array new: 32.
	#('B' 'I' 'BI') doWithIndex:
		[:tag :index | 
		(style := TextStyle named: self familyName) ifNotNil:
			[(font := style fontArray
				detect: [:each | each name = (self name , tag)]
				ifNone: [nil]) ifNotNil: [derivativeFonts at: index put: font]]]
	"
]

{ #category : #'file in/out' }
StrikeFont >> restOfLine: leadString from: file [ 
	"Utility method to assist reading of BitFont data files"
	| line |
	
	[ line := file nextLine.
	line size < leadString size or: [ leadString ~= (line 
			copyFrom: 1
			to: leadString size) ] ] whileTrue: [ file atEnd ifTrue: [ ^ nil ] ].
	^ line 
		copyFrom: leadString size + 1
		to: line size
]

{ #category : #accessing }
StrikeFont >> setGlyphs: newGlyphs [ 
	"Replace the glyphs form.  Used to make a synthetic bold or italic font quickly."
	glyphs := newGlyphs
]

{ #category : #building }
StrikeFont >> setGlyphsDepthAtMost: aNumber [ 
	glyphs depth > aNumber ifTrue: [ glyphs := glyphs asFormOfDepth: aNumber ]
]

{ #category : #'multibyte character methods' }
StrikeFont >> setupDefaultFallbackFont [
	| fonts f |
	fonts := TextStyle default fontArray.
	f := fonts first.
	1 
		to: fonts size
		do: [ :i | self height > (fonts at: i) height ifTrue: [ f := fonts at: i ] ].
	self fallbackFont: f.
	self reset
]

{ #category : #building }
StrikeFont >> stripHighGlyphs [
	"Remove glyphs for characters above 128"
	| i |
	maxAscii := 127.
	xTable := xTable 
		copyFrom: 1
		to: maxAscii + 3.
	i := xTable at: maxAscii + 1.
	xTable 
		at: maxAscii + 2
		put: i.
	xTable 
		at: maxAscii + 3
		put: i.
	glyphs := glyphs copy: (0 @ 0 extent: i @ glyphs height).
	maxWidth := 0.
	2 
		to: xTable size
		do: [ :ii | maxWidth := maxWidth max: (xTable at: ii) - (xTable at: ii - 1) - 1 ].
	characterToGlyphMap := nil.
	self reset
]

{ #category : #accessing }
StrikeFont >> subscript [
	"Answer an integer that is the further vertical offset relative to the 
	baseline for positioning characters as subscripts."

	^subscript
]

{ #category : #accessing }
StrikeFont >> superscript [
	"Answer an integer that is the further vertical offset relative to the 
	baseline for positioning characters as superscripts."

	^superscript
]

{ #category : #accessing }
StrikeFont >> textStyle [
	^ TextStyle actualTextStyles detect:
		[:aStyle | aStyle fontArray includes: self] ifNone: [nil]
]

{ #category : #'character shapes' }
StrikeFont >> useLeftArrow [
	self characterToGlyphMap.
	characterToGlyphMap at: 96 put: 95.
	characterToGlyphMap at: 95 put: 94
]

{ #category : #'character shapes' }
StrikeFont >> useUnderscore [
	self characterToGlyphMap.
	characterToGlyphMap at: 96 put: 129.
	characterToGlyphMap at: 95 put: 128
]

{ #category : #'character shapes' }
StrikeFont >> useUnderscoreIfOver1bpp [

	glyphs depth = 1 ifTrue: [
		characterToGlyphMap ifNotNil: [	
			characterToGlyphMap at: 96 put: 95.
			characterToGlyphMap at: 95 put: 94 ].
		^self ].
	
	self characterToGlyphMap.
	characterToGlyphMap at: 96 put: 129.
	characterToGlyphMap at: 95 put: 128
]

{ #category : #copying }
StrikeFont >> veryDeepCopyWith: deepCopier [
	"Return self.  I am shared.  Do not record me."
]

{ #category : #'character shapes' }
StrikeFont >> widen: char by: delta [ 
	
	^ self 
		alter: char
		formBlock: 
			[ :charForm | | newForm | 
			"Make a new form, wider or narrower..."
			newForm := Form extent: charForm extent + (delta @ 0).
			charForm displayOn: newForm.	"Copy this image into it"
			newForm	"and substitute it in the font" ]
]

{ #category : #accessing }
StrikeFont >> widthOf: aCharacter [ 
	"Answer the width of the argument as a character in the receiver."
	| code |
	code := aCharacter charCode.
	self characterToGlyphMap size > code ifTrue: [
		code := characterToGlyphMap at: code + 1 ].
	((code < minAscii or: [maxAscii < code]) 
		or: [(xTable at: code + 1) < 0])
			ifTrue: [^ self fallbackFont widthOf: aCharacter].
	^ (xTable at: code + 2) - (xTable at: code + 1)
]

{ #category : #accessing }
StrikeFont >> xTable [
	"Answer an Array of the left x-coordinate of characters in glyphs."

	^xTable
]

{ #category : #accessing }
StrikeFont >> xTable: anObject [ 
	xTable := anObject
]
