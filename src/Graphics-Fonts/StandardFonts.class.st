"
I am a facade and a global reference to actual fonts to be used system wide in a number of predefined categories.

I also provide methods to interactively choose these fonts, to reset all fonts to some default value, to set larger demo fonts, to construct a font menu and to manipulate the current settings as an external specification.

The font categories that I know of are: ButtonFont, CodeFont, HaloFont, ListFont, MenuFont and WindowTitleFont

Examples:
- I open a dialog to choose the font for buttons with:
StandardFonts chooseButtonFont.

- To change base size for all fonts (when you want big fonts for a screencast or a presentation):
StandardFonts setDemoFonts.

- Then restore default fonts with:
StandardFonts restoreDefaultFonts.
"
Class {
	#name : #StandardFonts,
	#superclass : #Object,
	#classVars : [
		'BalloonFont',
		'ButtonFont',
		'CodeFont',
		'HaloFont',
		'ListFont',
		'MenuFont',
		'WindowTitleFont'
	],
	#pools : [
		'TextConstants'
	],
	#category : #'Graphics-Fonts'
}

{ #category : #'font accessing' }
StandardFonts class >> balloonFont [
	^ BalloonFont ifNil: [BalloonFont := self defaultFont]
]

{ #category : #'font accessing' }
StandardFonts class >> balloonFont: aFont [
	BalloonFont := aFont
]

{ #category : #styles }
StandardFonts class >> bigPointSizeForStyleNamed: aSymbol [
	| all s idx |
	StrikeFont defaultFontKey.
	all := TextStyle pointSizesFor: StrikeFont defaultFontKey.
	s := self pointSizeForStyleNamed: aSymbol.
	idx := all indexOf: s ifAbsent: [^ s].
	^ idx = all size
			ifTrue: [all at: idx]
			ifFalse: [all at: idx + 1]
]

{ #category : #'font accessing' }
StandardFonts class >> buttonFont [
	^ ButtonFont ifNil: [ButtonFont := self defaultFont]
]

{ #category : #'font accessing' }
StandardFonts class >> buttonFont: aFont [
	 ButtonFont := aFont
]

{ #category : #choosing }
StandardFonts class >> chooseBalloonFont [
	self chooseStandardFont: #balloonFont
]

{ #category : #choosing }
StandardFonts class >> chooseButtonFont [
	self chooseStandardFont: #buttonFont
]

{ #category : #choosing }
StandardFonts class >> chooseCodeFont [
	self chooseStandardFont: #codeFont
]

{ #category : #choosing }
StandardFonts class >> chooseDefaultFont [
	self chooseStandardFont: #defaultFont
]

{ #category : #choosing }
StandardFonts class >> chooseFontWithTitle: aTitle for: aReceiver setSelector: aSetSelector getSelector: aGetSelector [
	FontChooser
		openWithWindowTitle: aTitle
		for: aReceiver
		setSelector: aSetSelector
		getSelector: aGetSelector
]

{ #category : #choosing }
StandardFonts class >> chooseHaloFont [
	self chooseStandardFont: #haloFont
]

{ #category : #choosing }
StandardFonts class >> chooseListFont [
	self chooseStandardFont: #listFont
]

{ #category : #choosing }
StandardFonts class >> chooseMenuFont [
	self chooseStandardFont: #menuFont
]

{ #category : #choosing }
StandardFonts class >> chooseStandardFont: aSelector [ 
	self
		chooseFontWithTitle: (self systemFontLabelOf: aSelector)	, '...'
		for: self
		setSelector: (aSelector , ':') asSymbol
		getSelector: aSelector
]

{ #category : #choosing }
StandardFonts class >> chooseWindowTitleFont [
	self chooseStandardFont: #windowTitleFont
]

{ #category : #'font accessing' }
StandardFonts class >> codeFont [
	^ CodeFont ifNil: [CodeFont := self defaultFont]
]

{ #category : #'font accessing' }
StandardFonts class >> codeFont: aFont [
	 CodeFont := aFont
]

{ #category : #'font accessing' }
StandardFonts class >> defaultFont [
	^ TextStyle defaultFont
]

{ #category : #'font accessing' }
StandardFonts class >> defaultFont: aFont [
	"Establish the default text font and style"

	| aStyle newDefaultStyle |
	aFont ifNil: [^ StandardFonts setFontsFromSpec: {{#defaultFont:. StrikeFont defaultFontKey. StrikeFont defaultFontSize}}].
	aStyle := aFont textStyle ifNil: [^ self].
	newDefaultStyle := aStyle copy.
	newDefaultStyle defaultFontIndex: (aStyle fontIndexOf: aFont).
	TextSharedInformation at: #DefaultTextStyle put: newDefaultStyle.

]

{ #category : #'small font fixing' }
StandardFonts class >> fixSmallDejaVuPunctuationMarks [
	"Just a small hack to make small font punctuation marks readable"
	| fb |
	StandardFonts setFontsToStyleNamed: #small. "To be sure we use the small font"
	fb :=StandardFonts defaultFont emphasized: 1. "copy the bold font"
	"For each punctuation mark, replace the corresponding glyph with the one found in the bold font" 
	#($. $: $; $' $` $, $")
		do: [:ch |
			StandardFonts defaultFont characterFormAt: ch put: (fb characterFormAt: ch) deepCopy].
]

{ #category : #styles }
StandardFonts class >> fontsSpecForStyleNamed: aSymbol [
	| k small regular big |
	k := StrikeFont defaultFontKey.
	small := self smallPointSizeForStyleNamed: aSymbol.
	regular := self pointSizeForStyleNamed: aSymbol.
	big := self bigPointSizeForStyleNamed: aSymbol.
	^ {
			{#defaultFont:. k. regular }. 
			{#codeFont:. k. regular }.
			{#listFont:. k. regular }. 
			{#menuFont:. k. regular }. 
			{#windowTitleFont:. k. big }. 
			{#balloonFont:. k. small }. 
			{#haloFont:. k. small }.
			{#buttonFont:. k. regular } }

]

{ #category : #'font accessing' }
StandardFonts class >> getCurrentFontsSpec [
	"self getCurrentFontsSpec "
	^ self systemFontAspects collect: [:aspect | | font |
		font := self perform: aspect.
		Array with: (aspect, ':') asSymbol with: font familyName with: font pointSize]
]

{ #category : #'font accessing' }
StandardFonts class >> haloFont [
	^ HaloFont ifNil: [HaloFont := StandardFonts defaultFont]
]

{ #category : #'font accessing' }
StandardFonts class >> haloFont: aFont [
	HaloFont := aFont
]

{ #category : #'font accessing' }
StandardFonts class >> listFont [
	^ ListFont ifNil: [ListFont := self defaultFont]
]

{ #category : #'font accessing' }
StandardFonts class >> listFont: aFont [
	 ListFont := aFont
]

{ #category : #'font accessing' }
StandardFonts class >> menuFont [
	^ MenuFont ifNil: [MenuFont := self defaultFont]
]

{ #category : #'font accessing' }
StandardFonts class >> menuFont: aFont [
	aFont ~= MenuFont 
		ifTrue: [ MenuFont := aFont ].
]

{ #category : #styles }
StandardFonts class >> pointSizeForStyleNamed: aSymbol [
	"
	self pointSizeForStyleNamed: #small.
	self pointSizeForStyleNamed: #Medium.
	self pointSizeForStyleNamed: #LARGE.
	self pointSizeForStyleNamed: #verylarge.
	self pointSizeForStyleNamed: #Huge.
	self pointSizeForStyleNamed: #xxxxx.
	"
	^ (#(#(#small 9) #(#medium 12) #(#large 14) #(veryLarge 18) #(huge 24)) 
		detect: [:info | info first asUppercase = aSymbol asUppercase] ifNone: []) 
			ifNotNil: [:info | info second]
	
		

]

{ #category : #'font accessing' }
StandardFonts class >> printStandardSystemFonts [
	"self printStandardSystemFonts"

	| string |
	string := String streamContents: [ :s |

	StandardFonts systemFontAspects do: [:selector |
		| font |
		font := StandardFonts perform: selector.
		s
			nextPutAll: selector; space;
			nextPutAll: font familyName; space;
			nextPutAll: (AbstractFont emphasisStringFor: font emphasis);
			nextPutAll: ' points: ';
			print: font pointSize;
			nextPutAll: ' height: ';
			print: font height;
			newLine
		]].
	UIManager default  
		longMessage: string
		title: 'Current system font settings' translated.
	
]

{ #category : #choosing }
StandardFonts class >> restoreDefaultFonts [
	"Since this is called from menus, we can take the opportunity to prompt for missing font styles."
	"
	self restoreDefaultFonts
	"
	self setFontsToSmall
]

{ #category : #'font accessing' }
StandardFonts class >> setAllStandardFontsTo: aFont [
	self systemFontAspects do: [:selector | StandardFonts perform: (selector, ':') asSymbol with: aFont].
	 

]

{ #category : #menu }
StandardFonts class >> setDefaultFonts: defaultFontsSpec [
	"Since this is called from menus, we can take the opportunity to prompt for missing font styles."

	| fontNames map emphases |
	fontNames := defaultFontsSpec collect: [:array | array second].
	map := IdentityDictionary new.
	emphases := IdentityDictionary new.
	fontNames do: [:originalName | | decoded style |
		decoded := TextStyle decodeStyleName: originalName.
		style := map at: originalName put: (TextStyle named: decoded second).
		emphases at: originalName put: decoded first.
		style ifNil: [map at: originalName put: (TextStyle default) ]].
	defaultFontsSpec do: [:triplet | self
		perform: triplet first
		with: (((map at: triplet second) fontOfPointSize: triplet third) emphasized: (emphases at: triplet second))]
]

{ #category : #choosing }
StandardFonts class >> setDemoFonts [
	"StandardFonts setDemoFonts"
	|size font codeFont titleFont|

	size := UIManager default request: 'Base font size?' initialAnswer: '14'.
	size isEmptyOrNil ifTrue: [^ self].
	size := size asInteger.
	(size isNil or: [size <= 0]) ifTrue: [^ self].
	font := LogicalFont familyName: 'DejaVu Sans' pointSize: size.
	codeFont := LogicalFont familyName: 'DejaVu Sans Mono' pointSize: size.
	titleFont := LogicalFont familyName: 'DejaVu Serif' pointSize: size.

	StandardFonts
		listFont: font;
		menuFont: font;
		codeFont: codeFont;
		buttonFont: font;
		defaultFont: font;
		windowTitleFont: titleFont.
]

{ #category : #'font accessing' }
StandardFonts class >> setFontsFromSpec: fontsSpec [
	"set some fonts by interpreting fontsSpec.
	fontsSpec is a collection.
	Each fontsSpec element is an array with 3 elements: 
	(1) the font accessor (i.e. #codeFont:)
	(2) the font family (i.e. 'Arial')
	(3) the font point size.
	To see an example of fontsSpec, inspect the result of 
	StandardFonts getCurrentFontsSpec.
	"
	| fontNames map emphases |
	fontNames := fontsSpec
				collect: [:array | array second].
	map := IdentityDictionary new.
	emphases := IdentityDictionary new.
	fontNames
		do: [:originalName | 
			| decoded style |
			decoded := TextStyle decodeStyleName: originalName.
			style := map
						at: originalName
						put: (TextStyle named: decoded second).
			emphases at: originalName put: decoded first.
			style
				ifNil: [map at: originalName put: TextStyle default]].
	fontsSpec
		do: [:triplet | self
				perform: triplet first
				with: (((map at: triplet second)
						fontOfPointSize: triplet third)
						emphasized: (emphases at: triplet second))]
]

{ #category : #styles }
StandardFonts class >> setFontsToHuge [
	self setFontsToStyleNamed: #huge
]

{ #category : #styles }
StandardFonts class >> setFontsToLarge [
	self setFontsToStyleNamed: #large
]

{ #category : #styles }
StandardFonts class >> setFontsToMedium [
	self setFontsToStyleNamed: #medium
]

{ #category : #styles }
StandardFonts class >> setFontsToSmall [
	self setFontsToStyleNamed: #small
]

{ #category : #styles }
StandardFonts class >> setFontsToStyleNamed: aSymbol [
	self setFontsFromSpec: (self fontsSpecForStyleNamed: aSymbol)
]

{ #category : #styles }
StandardFonts class >> setFontsToVeryLarge [
	self setFontsToStyleNamed: #verylarge
]

{ #category : #choosing }
StandardFonts class >> setSmallBitmapFonts [

	| sans fixed |
	
	sans := 'Bitmap Source Sans Pro'.
	fixed := 'Bitmap Source Code Pro'.
	
	self defaultFont: (LogicalFont familyName: sans pointSize: 10).
	self codeFont: (LogicalFont familyName: fixed pointSize: 10).
	self listFont: (LogicalFont familyName: sans pointSize: 10).
	self menuFont: (LogicalFont familyName: sans pointSize: 10).
	self buttonFont: (LogicalFont familyName: sans pointSize: 10).
 	self windowTitleFont: (LogicalFont familyName: sans pointSize: 11).
	self balloonFont: (LogicalFont familyName: sans pointSize: 9).
 	self haloFont: (LogicalFont familyName: sans pointSize: 9).
]

{ #category : #styles }
StandardFonts class >> smallPointSizeForStyleNamed: aSymbol [
	| all s idx |
	StrikeFont defaultFontKey.
	all := TextStyle pointSizesFor: StrikeFont defaultFontKey.
	s := self pointSizeForStyleNamed: aSymbol.
	idx := all indexOf: s ifAbsent: [^ s].
	^ idx > 1
			ifTrue: [all at: idx - 1]
			ifFalse: [all at: idx]
]

{ #category : #'font accessing' }
StandardFonts class >> systemFontAspects [
	^ self systemFontAspectsAndLabels collect: [:assoc | assoc key]
]

{ #category : #'font accessing' }
StandardFonts class >> systemFontAspectsAndLabels [
	^ {#defaultFont -> 'Default font' translated. #listFont -> 'List font'  translated. #menuFont -> 'Menu font'  translated. #windowTitleFont -> 'Window title'  translated. #balloonFont -> 'Balloon help font'  translated. #codeFont -> 'Code font'  translated. #buttonFont -> 'Button font'  translated. #haloFont -> 'Halo font'  translated } 
]

{ #category : #'font accessing' }
StandardFonts class >> systemFontLabelOf: aSelector [
	^ self systemFontAspectsAndLabels
		detect: [ :assoc | assoc key = aSelector ]
		ifFound: [ :assoc | assoc value ]
		ifNone: [ '' ]
]

{ #category : #'font accessing' }
StandardFonts class >> systemFontLabels [
	^ self systemFontAspectsAndLabels collect: [:assoc | assoc value]
]

{ #category : #'font accessing' }
StandardFonts class >> windowTitleFont [
	^ WindowTitleFont ifNil: [WindowTitleFont := StandardFonts defaultFont]
]

{ #category : #'font accessing' }
StandardFonts class >> windowTitleFont: aFont [
	WindowTitleFont := aFont
]
