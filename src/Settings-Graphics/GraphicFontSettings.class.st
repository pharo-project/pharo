"
Settings for the fonts
"
Class {
	#name : #GraphicFontSettings,
	#superclass : #Model,
	#category : #'Settings-Graphics'
}

{ #category : #fonts }
GraphicFontSettings class >> displayScaleFactorForStyleNamed: aSymbol [

	^ (#(#(#superTiny 0.6) #(#tiny 0.8) #(#small 1) #(#medium 1.3) #(#large 1.6) #(veryLarge 2) #(huge 2.7)) 
		detect: [:info | info first asUppercase = aSymbol asUppercase] ifNone: []) 
			ifNotNil: [:info | info second]

]

{ #category : #fonts }
GraphicFontSettings class >> fontChoiceButtonForStyle: aSymbol label: aLabel [
	| label |
	label := StringMorph 
				contents: aLabel asString 
				font: (TextStyle default fontOfPointSize: (StandardFonts pointSizeForStyleNamed: aSymbol)).
	^ (self theme 
		newButtonIn: self currentWorld
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
	^ self theme newRowIn: self currentWorld for: {
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
		newButtonIn: self currentWorld
		for: self
		getState: nil
		action: #resetAllFontToDefault
		arguments: {}
		getEnabled: nil
		getLabel: nil
		help: 'Force all system fonts to be the default one' translated)
		label:
			(self theme
				newRowIn: self currentWorld
				for:
					{(AlphaImageMorph new
						image: (self iconNamed: #smallRedoIcon)).
					(self theme buttonLabelForText: 'Force all' translated)});
		yourself
]

{ #category : #fonts }
GraphicFontSettings class >> setFontsToStyleNamed: aSymbol [

	(self class environment includesKey: #SourceCodeFonts)
		ifTrue: [ (self environment at: #SourceCodeFonts) setFontsToStyleNamed: aSymbol ]
		ifFalse: [ StandardFonts setFontsToStyleNamed: aSymbol ].
		
	WorldMorph displayScaleFactor: (self displayScaleFactorForStyleNamed: aSymbol).

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
