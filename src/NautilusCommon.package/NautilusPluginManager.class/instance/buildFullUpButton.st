items creation
buildFullUpButton
	^ IconicButton new
		target: self;
		actionSelector: #fullUp;
		labelGraphic: (self iconNamed: #topIcon);
		color: Color transparent;
		extent: 24 @ 24;
		borderWidth: 0