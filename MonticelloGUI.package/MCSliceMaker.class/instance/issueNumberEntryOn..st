user interface
issueNumberEntryOn: canvas
	| issueNumberEntry |
	issueNumberEntry := canvas
				newTextEntryFor: self info
				get: #issueNumber
				set: #issueNumber:
				class: Integer
				getEnabled: nil
				help: 'Fogbugz issue number' translated.
	issueNumberEntry
		hResizing: #rigid;
		acceptOnFocusChange: true;
		width: (StandardFonts defaultFont widthOfString: 'Issue number0');
		ghostText: 'Issue number'.
	^ issueNumberEntry