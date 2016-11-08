fonts
standardFontsSettingsOn: aBuilder
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
				
				