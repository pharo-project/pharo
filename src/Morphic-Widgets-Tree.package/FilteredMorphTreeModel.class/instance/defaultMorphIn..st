view
defaultMorphIn: aThemable
	| treeMorph patternMorph |
	treeMorph := self defaultTreeMorph buildContents.
	patternMorph := patternModel newTextField
		hResizing: #spaceFill;
		autoAccept: autoAccept;
		withGhostText: ghostString;
		textFont: StandardFonts defaultFont;
		yourself.
	patternMorph ghostTextRuler font: StandardFonts defaultFont.
	patternMorph  announcer when: RubReturnEntered  send: #whenReturnInFilter: to: self.
	^ self theme
		newColumnIn: aThemable
		for: {patternMorph. treeMorph}