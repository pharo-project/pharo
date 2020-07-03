Extension { #name : #StandardFonts }

{ #category : #'*Fonts-Chooser' }
StandardFonts class >> chooseBalloonFont [
	self chooseStandardFont: #balloonFont
]

{ #category : #'*Fonts-Chooser' }
StandardFonts class >> chooseButtonFont [
	self chooseStandardFont: #buttonFont
]

{ #category : #'*Fonts-Chooser' }
StandardFonts class >> chooseCodeFont [
	self chooseStandardFont: #codeFont
]

{ #category : #'*Fonts-Chooser' }
StandardFonts class >> chooseDefaultFont [
	self chooseStandardFont: #defaultFont
]

{ #category : #'*Fonts-Chooser' }
StandardFonts class >> chooseFontWithTitle: aTitle for: aReceiver setSelector: aSetSelector getSelector: aGetSelector [
	FontChooser
		openWithWindowTitle: aTitle
		for: aReceiver
		setSelector: aSetSelector
		getSelector: aGetSelector
]

{ #category : #'*Fonts-Chooser' }
StandardFonts class >> chooseHaloFont [
	self chooseStandardFont: #haloFont
]

{ #category : #'*Fonts-Chooser' }
StandardFonts class >> chooseListFont [
	self chooseStandardFont: #listFont
]

{ #category : #'*Fonts-Chooser' }
StandardFonts class >> chooseMenuFont [
	self chooseStandardFont: #menuFont
]

{ #category : #'*Fonts-Chooser' }
StandardFonts class >> chooseStandardFont: aSelector [ 
	self
		chooseFontWithTitle: (self systemFontLabelOf: aSelector)	, '...'
		for: self
		setSelector: (aSelector , ':') asSymbol
		getSelector: aSelector
]

{ #category : #'*Fonts-Chooser' }
StandardFonts class >> chooseWindowTitleFont [
	self chooseStandardFont: #windowTitleFont
]

{ #category : #'*Fonts-Chooser' }
StandardFonts class >> restoreDefaultFonts [
	"Since this is called from menus, we can take the opportunity to prompt for missing font styles."
	"
	self restoreDefaultFonts
	"
	self setFontsToSmall
]

{ #category : #'*Fonts-Chooser' }
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

{ #category : #'*Fonts-Chooser' }
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
