"
Settings for the fonts
"
Class {
	#name : 'GraphicFontSettings',
	#superclass : 'Model',
	#classVars : [
		'FontSize'
	],
	#category : 'Settings-Graphics',
	#package : 'Settings-Graphics'
}

{ #category : 'fonts' }
GraphicFontSettings class >> defaulFontSize [

	^ #medium
]

{ #category : 'fonts' }
GraphicFontSettings class >> displayScaleFactorForStyleNamed: aSymbol [

	^ (#(#(#superTiny 0.6) #(#tiny 0.8) #(#small 1) #(#medium 1.4) #(#large 1.6) #(veryLarge 2) #(huge 2.7))
		detect: [:info | info first asUppercase = aSymbol asUppercase] ifNone: [])
			ifNotNil: [:info | info second]
]

{ #category : 'private' }
GraphicFontSettings class >> font [

	^ StandardFonts defaultFont
]

{ #category : 'private' }
GraphicFontSettings class >> font: aFont [

	^ self resetAllFontsTo: aFont
]

{ #category : 'fonts' }
GraphicFontSettings class >> fontSize [

	^ FontSize
		ifNil: [ FontSize := self defaulFontSize ]
]

{ #category : 'fonts' }
GraphicFontSettings class >> fontSize: aSymbol [

	FontSize := aSymbol.
	self setFontsToStyleNamed: aSymbol.
]

{ #category : 'fonts' }
GraphicFontSettings class >> fontSizeRowSettingsOn: aBuilder [
	<systemsettings>

	(aBuilder pickOne: #fontSize)
		parent: #standardFonts;
		label: 'Predefined Styles';
		description: '';
		target: GraphicFontSettings;		
		selector: #fontSize;
		default: self fontSize;
		order: -1;
		domainValues: {
				('Small' -> #small).
				('Medium' -> #medium).
				('Large' -> #large).
				('Very large' -> #veryLarge).
				('Huge' -> #huge) }
]

{ #category : 'private' }
GraphicFontSettings class >> pickFont: aPresenter [

	FontChooser
		openModalWithTitle: 'Font for all font'
		initialFont: StandardFonts defaultFont
		onAcceptDo: [ :aFont | self resetAllFontsTo: aFont ].

	aPresenter ifNotNil: [ aPresenter window presenter announce: StSettingModelChanged ]
]

{ #category : 'fonts' }
GraphicFontSettings class >> resetAllFontToDefault [

	StandardFonts setAllStandardFontsTo: StandardFonts defaultFont
]

{ #category : 'fonts' }
GraphicFontSettings class >> resetAllFontToDefaultButton [
	^ (self theme
		newButtonIn: self
		for: self
		getState: nil
		action: #resetAllFontToDefault
		arguments: {}
		getEnabled: nil
		getLabel: nil
		help: 'Force all system fonts to be the default one')
		label:
			(self theme
				newRowIn: self
				for:
					{(AlphaImageMorph new
						image: (self iconNamed: #smallRedo)).
					(self theme buttonLabelForText: 'Force all')});
		yourself
]

{ #category : 'private' }
GraphicFontSettings class >> resetAllFontsTo: aFont [

	StandardFonts defaultFont: aFont.
	self resetAllFontToDefault
]

{ #category : 'fonts' }
GraphicFontSettings class >> setFontsToStyleNamed: aSymbol [

	(self class environment includesKey: #SourceCodeFonts)
		ifTrue: [ (self environment at: #SourceCodeFonts) setFontsToStyleNamed: aSymbol ]
		ifFalse: [ StandardFonts setFontsToStyleNamed: aSymbol ].

	WorldMorph displayScaleFactor: (self displayScaleFactorForStyleNamed: aSymbol)
]

{ #category : 'fonts' }
GraphicFontSettings class >> standardFontsSettingsOn: aBuilder [
	<systemsettings>
	(aBuilder group: #standardFonts)
		label: 'Standard fonts';
		target: StandardFonts;
		parent: #appearance;
		order: 4;
		with: [
			(aBuilder button: #pickFont:)
				order: 0;
				description: 'Pick an unique font for all fonts of this category' ;
				target: self;
				getSelector: #font;
				setSelector: #font:;
				label: 'Pick font for all';
				buttonLabel: 'Pick'.
			(aBuilder setting: #defaultFont)
				order: 1;
				description: 'The default text font' ;
				default: (LogicalFont familyName: 'Source Sans Pro' pointSize: 10);
				dialog: (MessageSend receiver: self selector: #resetAllFontToDefaultButton);
				label: 'Default'.
			(aBuilder setting: #codeFont)
				description: 'The standard code font' ;
				default: (LogicalFont familyName: 'Source Code Pro' pointSize: 10);
				label: 'Code'.
			(aBuilder setting: #listFont)
				description: 'The standard list font' ;
				default: (LogicalFont familyName: 'Source Sans Pro' pointSize: 10);
				label: 'List'.
			(aBuilder setting: #menuFont)
				description: 'The standard menu font' ;
				default: (LogicalFont familyName: 'Source Sans Pro' pointSize: 10);
				label: 'Menu' .
			(aBuilder setting: #buttonFont)
				description: 'The standard button font' ;
				default: (LogicalFont familyName: 'Source Sans Pro' pointSize: 10);
				label: 'Button' .
			(aBuilder setting: #windowTitleFont)
				description: 'The standard window title font' ;
				default: (LogicalFont familyName: 'Source Sans Pro' pointSize: 11);
				label: 'Window title' .
			(aBuilder setting: #balloonFont)
				description: 'The standard balloon-help font' ;
				default: (LogicalFont familyName: 'Source Sans Pro' pointSize: 9);
				label: 'Balloon-help' .
			(aBuilder setting: #haloFont)
				default: (LogicalFont familyName: 'Source Sans Pro' pointSize: 9);
				description: 'The morphic halo font' ;
				label: 'Morphic halo' ]
]
