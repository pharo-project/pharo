items creation
buildUpButton
	^ IconicButton new
		target: self;
		actionSelector: #up;
		labelGraphic: (self iconNamed: #upIcon);
		color: Color transparent;
		extent: 24 @ 24;
		borderWidth: 0