build ui buttons
buildNavigationNextButton
	^ IconicButton new
		target: self;
		actionSelector: #next;
		labelGraphic: (self iconNamed: #forwardIcon);
		color: Color transparent;
		borderWidth: 0;
		helpText: 'Go forward in the navigation history';
		yourself