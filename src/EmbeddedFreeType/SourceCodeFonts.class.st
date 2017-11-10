"
I'm a font setting for Source Code Pro fonts.
"
Class {
	#name : #SourceCodeFonts,
	#superclass : #Object,
	#category : #EmbeddedFreeType
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
		newButtonIn: World
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
	^ theme newRowIn: World for: {
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
		precondition: [ FT2Library current notNil ];
		dialog: [ self fontSourceCodeRow ]
]

{ #category : #accessing }
SourceCodeFonts class >> sizeHuge [
	^ 22
]

{ #category : #accessing }
SourceCodeFonts class >> sizeLarge [
	^ 16
]

{ #category : #accessing }
SourceCodeFonts class >> sizeMedium [ 
	^ 13
]

{ #category : #accessing }
SourceCodeFonts class >> sizeSmall [ 
	^ 10
]

{ #category : #accessing }
SourceCodeFonts class >> sizeVeryLarge [
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
