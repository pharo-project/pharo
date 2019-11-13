"
A textStyle comprises the formatting information for composing and displaying a unit (usually a paragraph) of text.  Typically one makes a copy of a master textStyle (such as TextStyle default), and then that copy may get altered in the process of editing.  Bad things can happen if you do not copy first.

Each of my instances consists of...
	fontArray		An array of StrikeFonts
	fontFamilySize	unused
	lineGrid			An integer; default line spacing for paragraphs
	baseline			An integer; default baseline (dist from line top to bottom of an 'a')
	alignment		An integer; text alignment, see TextStyle alignment:
	firstIndent		An integer; indent of first line in pixels
	restIndent		An integer; indent of remaining lines in pixels
	rightIndent		An integer; indent of right margin rel to section
	tabsArray		An array of integers giving tab offsets in pixels
	marginTabsArray	An array of margin tabs
	leading			An integer giving default vertical line separation

For a concrete example, look at TextStyle default copy inspect
"
Class {
	#name : #TextStyle,
	#superclass : #Object,
	#instVars : [
		'fontArray',
		'lineGrid',
		'baseline',
		'alignment',
		'firstIndent',
		'restIndent',
		'rightIndent',
		'tabsArray',
		'marginTabsArray',
		'leading',
		'defaultFontIndex'
	],
	#pools : [
		'TextConstants'
	],
	#classInstVars : [
		'notifier'
	],
	#category : #'Text-Core-Base'
}

{ #category : #'textconstants access' }
TextStyle class >> actualTextStyles [
	"TextStyle actualTextStyles"
	"Answer dictionary whose keys are the names of styles in the system and whose values are the actual styles"
	| aDict |
	aDict := TextSharedInformation select: [ :thang | thang isKindOf: self ].
	self defaultFamilyNames do: [ :sym | aDict removeKey: sym ].
	^ aDict
]

{ #category : #accessing }
TextStyle class >> addDependent: anObject [

	self notifier addDependent: anObject
]

{ #category : #accessing }
TextStyle class >> changed: anAspect [

	self notifier changed: anAspect
]

{ #category : #utilities }
TextStyle class >> decodeStyleName: styleName [ 
	"Given a string styleName, return a collection with: 
	 
	* [1] the probable Pharo emphasis code, which is a bit combination of: 
	1	bold 
	2	italic 
	4	underlined 
	8	narrow 
	16	strikeout 
	 
	* [2] the base style name without the modifiers (can be empty)
	* [3] the modifiers in the order they were found 
	* [4] the codes for those modifiers, in the same order
	"
	| decoder keys modifiers modifierCodes baseName styleCode matchedKey |

	decoder := self styleDecoder.

	modifiers := OrderedCollection new.
	modifierCodes := OrderedCollection new.
	keys := decoder keys asArray
				sort: [:a :b | a size > b size].
	styleCode := 0.
	baseName := styleName asString.
	[matchedKey := keys
				detect: [:k | baseName endsWith: k]
				ifNone: [].
	matchedKey notNil]
		whileTrue: [| last code | 
			last := baseName size - matchedKey size.
			last > 0
				ifTrue: [('- ' includes: (baseName at: last))
						ifTrue: [last := last - 1]].
			baseName := baseName copyFrom: 1 to: last.
			code := decoder at: matchedKey.
			styleCode := styleCode + code.
			modifiers addFirst: matchedKey.
			modifierCodes addFirst: code.
	].
	^ {styleCode. baseName. modifiers. modifierCodes }
]

{ #category : #constants }
TextStyle class >> default [
  "Answer the system default text style."

  ^TextSharedInformation at: #DefaultTextStyle ifAbsent: nil
]

{ #category : #'textconstants access' }
TextStyle class >> defaultFamilyNames [
	| DefaultTextStyle DefaultFixedTextStyle DefaultMultiStyle |

	DefaultTextStyle := TextSharedInformation at: #DefaultTextStyle.
	DefaultFixedTextStyle := TextSharedInformation at: #DefaultFixedTextStyle.
	DefaultMultiStyle := TextSharedInformation at: #DefaultMultiStyle.
	
	^#(DefaultTextStyle DefaultFixedTextStyle DefaultMultiStyle)
]

{ #category : #constants }
TextStyle class >> defaultFont [
	"Answer the default system font"

	^ self default defaultFont
]

{ #category : #'instance creation' }
TextStyle class >> fontArray: anArray [ 
	"Answer an instance of me with fonts those in the argument, anArray."

	^self new newFontArray: anArray
]

{ #category : #'textconstants access' }
TextStyle class >> fontArrayForStyle: aName [
	"Answer the fonts in the style named aName,
	or an empty Array if no such named style."

	"TextStyle fontArrayForStyle: #Atlanta"
	"TextStyle fontPointSizesFor: 'NewYork'"

	^ ((self named: aName) ifNil: [ ^#() ]) fontArray

]

{ #category : #'textconstants access' }
TextStyle class >> fontSizesFor: aName [
	"Answer the pixel sizes for all the fonts in the given text style"

	"TextStyle fontSizesFor: 'Arial'"
	"TextStyle fontSizesFor: 'NewYork'"

	^ (self fontArrayForStyle: aName) collect: [:f | f height ]

]

{ #category : #initialization }
TextStyle class >> initialize [
	self initializeStyleDecoder.
]

{ #category : #initialization }
TextStyle class >> initializeStyleDecoder [
	TextSharedInformation at: #StyleDecoder put: nil.
	self styleDecoder.
]

{ #category : #'textconstants access' }
TextStyle class >> knownTextStyles [
	"Answer the names of the known text styles, sorted in alphabetical order"

	"TextStyle knownTextStyles"
	^ (TextSharedInformation select: [:thang | thang isKindOf: TextStyle]) keys sort


]

{ #category : #'textconstants access' }
TextStyle class >> knownTextStylesWithoutDefault [
	"Answer the names of the known text styles, sorted in alphabetical order without default"

	"TextStyle knownTextStylesWithoutDefault"
	| result |
	result := self knownTextStyles asOrderedCollection.
	^ result copyWithoutAll: self defaultFamilyNames


]

{ #category : #constants }
TextStyle class >> named: familyName [ 
	"Answer the TextStyle with the given name, or nil."
	"TextStyle named: 'NewYork'"
	| textStyle |
	textStyle := TextSharedInformation 
		at: familyName
		ifAbsent: [ ^ nil ].
	(textStyle isKindOf: self) ifFalse: [ ^ nil ].
	^ textStyle
]

{ #category : #'instance creation' }
TextStyle class >> new [
	^ super new leading: 2
]

{ #category : #accessing }
TextStyle class >> notifier [

	^ notifier ifNil: [ notifier := Model new ]
]

{ #category : #utilities }
TextStyle class >> pixelsPerInch [
	"Answer the nominal resolution of the screen."

	^TextSharedInformation at: #pixelsPerInch ifAbsentPut: [ 96.0 ].
]

{ #category : #utilities }
TextStyle class >> pixelsToPoints: pixels [
	^pixels * 72.0 / self pixelsPerInch
]

{ #category : #'textconstants access' }
TextStyle class >> pointSizesFor: aName [
	"Answer all the point sizes for the given text style name"

	"TextStyle pointSizesFor: 'NewYork'"
	^ (self fontArrayForStyle: aName) collect: [:f | f pointSize]

]

{ #category : #utilities }
TextStyle class >> pointsToPixels: points [
	^points * self pixelsPerInch / 72.0
]

{ #category : #accessing }
TextStyle class >> removeDependent: anObject [

	self notifier removeDependent: anObject
]

{ #category : #constants }
TextStyle class >> setDefault: aTextStyle [
	"Answer the system default text style."

	TextSharedInformation at: #DefaultTextStyle put: aTextStyle.
]

{ #category : #initialization }
TextStyle class >> styleDecoder [
	TextSharedInformation at: #StyleDecoder ifPresent: [ :dict | dict ifNotNil: [ ^dict ]].
	^TextSharedInformation at: #StyleDecoder put: (
		Dictionary new at: 'Regular' put: 0;
				 at: 'Roman' put: 0;
				 at: 'Medium' put: 0;
				 at: 'Light' put: 0;
				 at: 'Normal' put: 0;
				 at: 'Plain' put: 0;
				 at: 'Book' put: 0;
				 at: 'Demi' put: 0;
				 at: 'Demibold' put: 0;
				 at: 'Semibold' put: 0;
				 at: 'SemiBold' put: 0;
				 at: 'ExtraBold' put: 1;
				 at: 'SuperBold' put: 1;
				 at: 'B' put: 1;
				 at: 'I' put: 2;
				 at: 'U' put: 4;
				 at: 'X' put: 16;
				 at: 'N' put: 8;
				 at: 'Bold' put: 1;
				 at: 'Italic' put: 2;
				 at: 'Oblique' put: 2;
				 at: 'Narrow' put: 8;
				 at: 'Condensed' put: 8;
				 at: 'Underlined' put: 4;
				 yourself )
]

{ #category : #comparing }
TextStyle >> = other [

	self species == other species ifFalse: [^ false].
	1 to: self class instSize do:
		[:i | (self instVarAt: i) == (other instVarAt: i) ifFalse: [^ false]].
	^ true
]

{ #category : #accessing }
TextStyle >> alignment [
	"Answer the code for the current setting of the alignment."

	^alignment
]

{ #category : #accessing }
TextStyle >> alignment: anInteger [ 
	"Set the current setting of the alignment to be anInteger:
	0=left flush, 1=right flush, 2=centered, 3=justified."
	alignment := anInteger \\ (Justified + 1)
]

{ #category : #accessing }
TextStyle >> alignmentSymbol [
	"Answer the symbol for the current setting of the alignment."
	alignment = LeftFlush ifTrue:[^#leftFlush].
	alignment = Centered ifTrue:[^#centered].
	alignment = RightFlush ifTrue:[^#rightFlush].
	alignment = Justified ifTrue:[^#justified].
	^#leftFlush
]

{ #category : #accessing }
TextStyle >> baseline [
	"Answer the distance from the top of the line to the bottom of most of the 
	characters (by convention, bottom of the letter 'A')."

	^baseline
]

{ #category : #accessing }
TextStyle >> baseline: anInteger [ 
	"Set the distance from the top of the line to the bottom of most of the 
	characters."
	baseline := anInteger
]

{ #category : #accessing }
TextStyle >> centered [
	alignment := 2
]

{ #category : #'tabs and margins' }
TextStyle >> clearIndents [
	"Reset all the margin (index) settings to be 0."

	self firstIndent: 0.
	self restIndent: 0.
	self rightIndent: 0
]

{ #category : #'fonts and font indexes' }
TextStyle >> consistOnlyOf: aFont [ 
	fontArray := Array with: aFont.
	defaultFontIndex := 1
]

{ #category : #private }
TextStyle >> consolidate [
	"If this style includes any fonts that are also in the default style,
	then replace them with references to the default ones."
	"
	TextStyle allInstancesDo: [:s | s == TextStyle default ifFalse: [s consolidate]]
"
	| defFonts font |
	defFonts := TextStyle default fontArray.
	1 
		to: fontArray size
		do: 
			[ :i | 
			font := fontArray at: i.
			1 
				to: defFonts size
				do: 
					[ :j | 
					(font familyName asUppercase copyWithout: $ ) = ((defFonts at: j) familyName asUppercase copyWithout: $ ) ifTrue: 
						[ fontArray 
							at: i
							put: (defFonts at: j) ] ] ]
]

{ #category : #accessing }
TextStyle >> defaultFont [
	^ fontArray at: self defaultFontIndex
]

{ #category : #'default font' }
TextStyle >> defaultFontIndex [
	^ defaultFontIndex ifNil: [ defaultFontIndex := 1 ]
]

{ #category : #'default font' }
TextStyle >> defaultFontIndex: anIndex [ 
	defaultFontIndex := anIndex
]

{ #category : #accessing }
TextStyle >> firstIndent [
	"Answer the horizontal indenting of the first line of a paragraph in the 
	style of the receiver."

	^firstIndent
]

{ #category : #accessing }
TextStyle >> firstIndent: anInteger [ 
	"Set the horizontal indenting of the first line of a paragraph in the style 
	of the receiver to be the argument, anInteger."
	firstIndent := anInteger
]

{ #category : #private }
TextStyle >> fontArray [
	"Only for writing out fonts, etc."
	^ fontArray
]

{ #category : #private }
TextStyle >> fontAt: index [ 
	"This is private because no object outside TextStyle should depend on the 
	representation of the font family in fontArray."

	^ fontArray atPin: index
]

{ #category : #private }
TextStyle >> fontAt: index put: font [ 
	"Automatically grow the array."
	index > fontArray size ifTrue: [ fontArray := fontArray , (Array new: index - fontArray size) ].
	fontArray 
		at: index
		put: font
]

{ #category : #'fonts and font indexes' }
TextStyle >> fontIndexOf: aFont [
	^ fontArray indexOf: aFont ifAbsent: [nil]
]

{ #category : #'fonts and font indexes' }
TextStyle >> fontIndexOfPointSize: desiredPointSize [ 
	"Returns an index in fontArray of the font with pointSize <= desiredPointSize"
	"Leading is not inluded in the comparison"
	| bestMatch bestIndex d |
	bestMatch := 9999.
	bestIndex := 1.
	1 
		to: fontArray size
		do: 
			[ :i | 
			d := desiredPointSize - (fontArray at: i) pointSize.
			d = 0 ifTrue: [ ^ i ].
			(d > 0 and: [ d < bestMatch ]) ifTrue: 
				[ bestIndex := i.
				bestMatch := d ] ].
	^ bestIndex
]

{ #category : #'fonts and font indexes' }
TextStyle >> fontIndexOfSize: desiredHeight [ 
	"Returns an index in fontArray of the font with height <= desiredHeight"
	"Leading is not inluded in the comparison"
	| bestMatch bestIndex d |
	bestMatch := 9999.
	bestIndex := 1.
	1 
		to: fontArray size
		do: 
			[ :i | 
			d := desiredHeight - (fontArray at: i) height.
			d = 0 ifTrue: [ ^ i ].
			(d > 0 and: [ d < bestMatch ]) ifTrue: 
				[ bestIndex := i.
				bestMatch := d ] ].
	^ bestIndex
]

{ #category : #accessing }
TextStyle >> fontNamed: fontName [  "TextStyle default fontNamed: 'TimesRoman10'"
	^ fontArray detect: [:x | x familyName sameAs: fontName]
]

{ #category : #accessing }
TextStyle >> fontNames [  "TextStyle default fontNames"
	^ fontArray collect: [:x | x familyName]
]

{ #category : #accessing }
TextStyle >> fontNamesAndSizes [  "TextStyle default fontNames"
	^ fontArray collect: [:x | x familyName, ' ', x height printString]
]

{ #category : #'fonts and font indexes' }
TextStyle >> fontOfPointSize: aPointSize [
	^ fontArray at: (self fontIndexOfPointSize: aPointSize)
]

{ #category : #'fonts and font indexes' }
TextStyle >> fontOfSize: aHeight [
	"See fontIndexOfSize.
	Returns the actual font.  Leading not considered."

	^ fontArray at: (self fontIndexOfSize: aHeight)
]

{ #category : #accessing }
TextStyle >> fonts [
	"Return a collection of fonts contained in this text style"
	^fontArray
]

{ #category : #private }
TextStyle >> gridForFont: fontIndex withLead: leadInteger [ 
	"Force whole style to suit one of its fonts. Assumes only one font referred
	to by runs."
	| font |
	font := self fontAt: fontIndex.
	self lineGrid: font height + leadInteger.
	self baseline: font ascent.
	self leading: leadInteger
]

{ #category : #comparing }
TextStyle >> hash [
	"#hash is re-implemented because #= is re-implemented"
	^fontArray hash

]

{ #category : #accessing }
TextStyle >> justified [
	alignment := 3
]

{ #category : #accessing }
TextStyle >> leading [
	"Leading (from typographers historical use of extra lead (type metal))
	is the extra spacing above and beyond that needed just to accomodate
	the various font heights in the set."
	^ leading
]

{ #category : #accessing }
TextStyle >> leading: yDelta [ 
	leading := yDelta
]

{ #category : #accessing }
TextStyle >> leftFlush [
	alignment := 0
]

{ #category : #'tabs and margins' }
TextStyle >> leftMarginTabAt: marginIndex [ 
	"Set the 'nesting' level of left margin indents of the paragraph in the 
	style of the receiver to be the argument, marginIndex."

	(marginIndex > 0 and: [marginIndex < marginTabsArray size])
		ifTrue: [^(marginTabsArray at: marginIndex) at: 1]
		ifFalse: [^0]	
	"The marginTabsArray is an Array of tuples.  The Array is indexed according 
	to the marginIndex, the 'nesting' level of the requestor."

]

{ #category : #accessing }
TextStyle >> lineGrid [
	"Answer the relative space between lines of a paragraph in the style of 
	the receiver."

	^lineGrid
]

{ #category : #accessing }
TextStyle >> lineGrid: anInteger [ 
	"Set the relative space between lines of a paragraph in the style of the 
	receiver to be the argument, anInteger."
	lineGrid := anInteger
]

{ #category : #private }
TextStyle >> marginTabAt: marginIndex side: sideIndex [ 
	"The marginTabsArray is an Array of tuples.  The Array is indexed
	according to the marginIndex, the 'nesting' level of the requestor.
	sideIndex is 1 for left, 2 for right."

	(marginIndex > 0 and: [marginIndex < marginTabsArray size])
		ifTrue: [^(marginTabsArray at: marginIndex) at: sideIndex]
		ifFalse: [^0]
]

{ #category : #private }
TextStyle >> newFontArray: anArray [ 
	"Currently there is no supporting protocol for changing these arrays. If an editor wishes to implement margin setting, then a copy of the default should be stored with these instance variables.  
	, Make size depend on first font."
	fontArray := anArray.
	lineGrid := (fontArray at: 1) height + leading.	"For whole family"
	baseline := (fontArray at: 1) ascent + leading.
	alignment := 0.
	firstIndent := 0.
	restIndent := 0.
	rightIndent := 0.
	tabsArray := DefaultTabsArray.
	marginTabsArray := DefaultMarginTabsArray
	"
TextStyle allInstancesDo: [:ts | ts newFontArray: TextStyle default fontArray].
"
]

{ #category : #'tabs and margins' }
TextStyle >> nextTabXFrom: anX leftMargin: leftMargin rightMargin: rightMargin [ 
	"Tab stops are distances from the left margin. Set the distance into the 
	argument, anX, normalized for the paragraph's left margin."
	| normalizedX tabX |
	normalizedX := anX - leftMargin.
	1 
		to: tabsArray size
		do: 
			[ :i | 
			(tabX := tabsArray at: i) > normalizedX ifTrue: [ ^ leftMargin + tabX min: rightMargin ] ].
	^ rightMargin
]

{ #category : #accessing }
TextStyle >> pointSizes [
	^ fontArray collect:
		[:x | x pointSize]

  "TextStyle default fontNamesWithPointSizes"
]

{ #category : #printing }
TextStyle >> printOn: aStream [

	super printOn: aStream.
	fontArray first isFontSet ifTrue: [
		aStream space; nextPutAll: self defaultFont familySizeFace first; nextPutAll: '(FontSet)'
	] ifFalse: [
		aStream space; nextPutAll: self defaultFont familySizeFace first
	]

]

{ #category : #accessing }
TextStyle >> restIndent [
	"Answer the indent for all but the first line of a paragraph in the style 
	of the receiver."

	^restIndent
]

{ #category : #accessing }
TextStyle >> restIndent: anInteger [ 
	"Set the indent for all but the first line of a paragraph in the style of the 
	receiver to be the argument, anInteger."
	restIndent := anInteger
]

{ #category : #accessing }
TextStyle >> rightFlush [
	alignment := 1
]

{ #category : #accessing }
TextStyle >> rightIndent [
	"Answer the right margin indent for the lines of a paragraph in the style 
	of the receiver."

	^rightIndent
]

{ #category : #accessing }
TextStyle >> rightIndent: anInteger [ 
	"Answer the right margin indent for the lines of a paragraph in the style 
	of the receiver to be the argument, anInteger."
	rightIndent := anInteger
]

{ #category : #'tabs and margins' }
TextStyle >> rightMarginTabAt: marginIndex [ 
	"Set the 'nesting' level of right margin indents of the paragraph in the 
	style of the receiver to be marginIndex."

	(marginIndex > 0 and: [marginIndex < marginTabsArray size])
		ifTrue: [^(marginTabsArray at: marginIndex) at: 2]
		ifFalse: [^0]
	"The marginTabsArray is an Array of tuples.  The Array is indexed according 
	to the marginIndex, the 'nesting' level of the requestor."

]

{ #category : #'tabs and margins' }
TextStyle >> tabWidth [
	"Answer the width of a tab."

	^DefaultTab
]

{ #category : #copying }
TextStyle >> veryDeepCopyWith: deepCopier [ 
	"All inst vars are meant to be shared"

	self == #veryDeepCopyWith:.	"to satisfy checkVariables"
	^deepCopier references at: self ifAbsentPut: [self shallowCopy]	"remember"
]
