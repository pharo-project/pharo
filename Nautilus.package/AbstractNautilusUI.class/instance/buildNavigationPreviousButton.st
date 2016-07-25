build ui buttons
buildNavigationPreviousButton
	^ IconicButton new
		target: self;
		actionSelector: #previous;
		labelGraphic: (self iconNamed: #backIcon);
		color: Color transparent;
		borderWidth: 0;
		helpText: 'Go back in the navigation history';
		yourself