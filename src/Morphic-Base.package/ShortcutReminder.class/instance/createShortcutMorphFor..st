shortcut creation
createShortcutMorphFor: aMenuItem 

	^FadingMorph new
		addMorphBack: (self createLabelMorph: aMenuItem contents);
		addMorphBack: (self createKeyTextMorph: aMenuItem keyText);
		color: backgroundColor;
		yourself
