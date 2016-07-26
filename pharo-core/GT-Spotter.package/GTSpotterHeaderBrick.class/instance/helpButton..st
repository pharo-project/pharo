accessing
helpButton: aBrick

	| tree item |
	helpButton ifNotNil: [self removeBrick: helpButton].
	helpButton := aBrick.
	
	self themer spotterThemer helpButtonWidgetStyleFor: helpButton.
	self themer spotterThemer helpButtonStyleFor: helpButton.
	
	helpButton
		hintTitle: 'Spotter help';
		action: [ 
			self spotterModel exitDueTo: #actionButton.
			tree := HelpBrowser open instVarNamed: #treeMorph.
			item := tree allItems detect: [:each | 
				each complexContents item key = GTSpotterHelp key].
			tree selection: item ].
	
	self addBrickBack: helpButton