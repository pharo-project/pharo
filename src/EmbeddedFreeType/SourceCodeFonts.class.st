"
I'm a font setting for Source Code Pro fonts.
"
Class {
	#name : #SourceCodeFonts,
	#superclass : #Object,
	#category : #'EmbeddedFreeType-Base'
}

{ #category : #accessing }
SourceCodeFonts class >> codeFontName [ 
	^ 'Source Code Pro'
]

{ #category : #accessing }
SourceCodeFonts class >> defaultFontName [
	^ 'Source Sans Pro'
]

{ #category : #accessing }
SourceCodeFonts class >> fontButton: aString size: size [ 
	^ (Smalltalk ui theme 
		newButtonIn: self currentWorld
			for: self
			getState: nil 
			action: #setSourceCodeFonts:	
			arguments: { size }
			getEnabled: nil
			getLabel: nil
			help: 'Change font style to Source Code Pro' translated)
		label: aString;
		yourself
]

{ #category : #accessing }
SourceCodeFonts class >> fontName [
	^ 'Source Code Pro'
]

{ #category : #settings }
SourceCodeFonts class >> fontSourceCodeRow [
	| theme |
	
	theme := Smalltalk ui theme.
	^ theme newRowIn: self currentWorld for: {
		theme buttonLabelForText: 'Predefined styles: ' translated.
		self fontButton: 'Small' size: self sizeSmall.
		self fontButton: 'Medium' size: self sizeMedium.
		self fontButton: 'Large' size: self sizeLarge.
		self fontButton: 'Very Large' size: self sizeVeryLarge.
		self fontButton: 'Huge' size: self sizeHuge	}	
		
		
]

{ #category : #'font registration' }
SourceCodeFonts class >> registerFonts: freeTypeFontManager [
	{ SourceSansProRegular. SourceCodeProRegular } do: [ :each |
		| font |
		font := each new.
		freeTypeFontManager 
			addFromFileContents: font fontContents 
			baseName: font originalFileName ]
]

{ #category : #settings }
SourceCodeFonts class >> setFontsToStyleNamed: aSymbol [

	(Pragma allNamed: #styleName: in: SourceCodeFonts class)
		detect: [ :each | each arguments first = aSymbol ]
		ifFound: [ :pragma | self setSourceCodeFonts: (self perform: pragma method selector) ]
]

{ #category : #accessing }
SourceCodeFonts class >> setSourceCodeFonts: pointSize [
	StandardFonts
		setAllStandardFontsTo: (LogicalFont familyName: self defaultFontName pointSize: pointSize);
		haloFont: (LogicalFont familyName: self defaultFontName pointSize: pointSize - 1);
		balloonFont: (LogicalFont familyName: self defaultFontName pointSize: pointSize - 1);
		windowTitleFont: (LogicalFont familyName: self defaultFontName pointSize: pointSize + 1);
		listFont: (LogicalFont familyName: self defaultFontName pointSize: pointSize);
		menuFont: (LogicalFont familyName: self defaultFontName pointSize: pointSize);
		codeFont: (LogicalFont familyName: self fontName pointSize: pointSize)
]

{ #category : #accessing }
SourceCodeFonts class >> setSourceCodeFontsDefault [
	self setSourceCodeFonts: self sizeSmall
]

{ #category : #settings }
SourceCodeFonts class >> settingsOn: aBuilder [
	<systemsettings>
	(aBuilder setting: #useSourceCode)
		parent: #appearance;
		order: 4;
		target: self;
		iconName: #smallConfigurationIcon;
		label: 'Source Code Fonts';
		description: 'Use Source Code Pro Fonts';
		default: SourceCodeFonts;
		precondition: [ FT2Library current isAvailable not ];
		dialog: [ self fontSourceCodeRow ]
]

{ #category : #accessing }
SourceCodeFonts class >> sizeHuge [

	<styleName: #huge>
	^ 22
]

{ #category : #accessing }
SourceCodeFonts class >> sizeLarge [

	<styleName: #large>
	^ 16
]

{ #category : #accessing }
SourceCodeFonts class >> sizeMedium [ 

	<styleName: #medium>
	^ 13
]

{ #category : #accessing }
SourceCodeFonts class >> sizeSmall [ 

	<styleName: #small>
	^ 10
]

{ #category : #accessing }
SourceCodeFonts class >> sizeSuperTiny [

	<styleName: #superTiny>
	^ 5
]

{ #category : #accessing }
SourceCodeFonts class >> sizeTiny [

	<styleName: #tiny>
	^ 8
]

{ #category : #accessing }
SourceCodeFonts class >> sizeVeryLarge [

	<styleName: #veryLarge>
	^ 18
]

{ #category : #accessing }
SourceCodeFonts class >> useSourceCode [
	
]

{ #category : #accessing }
SourceCodeFonts class >> useSourceCode: aBoolean [
	
]

{ #category : #notes }
SourceCodeFonts >> seeClassSide [
]
