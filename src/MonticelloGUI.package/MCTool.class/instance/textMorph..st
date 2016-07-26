morphic ui
textMorph: aSymbol
	| textMorph |
	textMorph := RubPluggableTextMorph new
		getTextSelector: aSymbol;
		setTextSelector: (aSymbol , ':') asSymbol;
		on: self;
		beWrapped;
		hScrollbarShowNever;
		beForSmalltalkScripting;
		yourself.
	textMorph announcer when: RubTextAcceptRequest send: #accept to: self.
	textMorph hasUnacceptedEdits: false.
	^ textMorph