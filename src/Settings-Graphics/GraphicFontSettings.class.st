"
Settings for the fonts
"
Class {
	#name : #GraphicFontSettings,
	#superclass : #Model,
	#category : #Settings-Graphics
}

{ #category : #fonts }
GraphicFontSettings class >> fontChoiceButtonForStyle: aSymbol label: aLabel [
	| label |
	label := StringMorph 
				contents: aLabel asString 
				font: (TextStyle default fontOfPointSize: (StandardFonts pointSizeForStyleNamed: aSymbol)).
	^ (self theme 
		newButtonIn: World
		for: self
		getState: nil 
		action: #setFontsToStyleNamed:
		arguments: { aSymbol }
		getEnabled: nil
		getLabel: nil
		help: ('Change font style to ', aLabel) translated)
			label: label;
			yourself

]

{ #category : #fonts }
GraphicFontSettings class >> fontSizeRow [
	^ self theme newRowIn: World for: {
			self theme buttonLabelForText: 'Predefined styles: ' translated.
			self fontChoiceButtonForStyle: #small label: 'Small' translated.
			self fontChoiceButtonForStyle: #medium label: 'Medium' translated.
			self fontChoiceButtonForStyle: #large label: 'Large' translated.
			self fontChoiceButtonForStyle: #veryLarge label: 'Very large' translated.
			self fontChoiceButtonForStyle: #huge label: 'Huge' translated.
	}
]

{ #category : #fonts }
GraphicFontSettings class >> resetAllFontToDefault [
	StandardFonts setAllStandardFontsTo: StandardFonts defaultFont.
	SettingBrowser refreshAllSettingBrowsers.
	EditorFindReplaceDialogWindow cleanUp.
	 

]

{ #category : #fonts }
GraphicFontSettings class >> resetAllFontToDefaultButton [
	^ (self theme
		newButtonIn: World
		for: self
		getState: nil
		action: #resetAllFontToDefault
		arguments: {}
		getEnabled: nil
		getLabel: nil
		help: 'Force all system fonts to be the default one' translated)
		label:
			(self theme
				newRowIn: World
				for:
					{(AlphaImageMorph new
						image: (self iconNamed: #smallRedoIcon)).
					(self theme buttonLabelForText: 'Force all' translated)});
		yourself
]

{ #category : #fonts }
GraphicFontSettings class >> setFontsToStyleNamed: aSymbol [
	StandardFonts setFontsToStyleNamed: aSymbol.
	SettingBrowser refreshAllSettingBrowsers.
	EditorFindReplaceDialogWindow cleanUp.
]

{ #category : #fonts }
GraphicFontSettings class >> standardFontsSettingsOn: aBuilder [
	<systemsettings>
	(aBuilder group: #standardFonts)
		label: 'Standard fonts';
		target: StandardFonts;
		dialog: [self fontSizeRow];
		parent: #appearance;
		order: 4;
		with: [ 
			(aBuilder setting: #defaultFont)
				order: 1;
				description: 'The default text font' ;
				dialog: (MessageSend receiver: self selector: #resetAllFontToDefaultButton);
				label: 'Default'.
			(aBuilder setting: #codeFont) 
				description: 'The standard code font' ;
				label: 'Code'.
			(aBuilder setting: #listFont)
				description: 'The standard list font' ;
				label: 'List'.
			(aBuilder setting: #menuFont)
				description: 'The standard menu font' ;
				label: 'Menu' .
			(aBuilder setting: #buttonFont)
				description: 'The standard button font' ;
				label: 'Button' .
			(aBuilder setting: #windowTitleFont)
				description: 'The standard window title font' ;
				label: 'Window title' .
			(aBuilder setting: #balloonFont)
				description: 'The standard balloon-help font' ;
				label: 'Balloon-help' .
			(aBuilder setting: #haloFont)
				description: 'The morphic halo font' ;
				label: 'Morphic halo' ]
				
				
]

{ #category : #fonts }
GraphicFontSettings class >> theme [ 
	^ Smalltalk ui theme
]
