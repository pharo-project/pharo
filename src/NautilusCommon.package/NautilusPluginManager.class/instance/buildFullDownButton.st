items creation
buildFullDownButton
	^ IconicButton new
		target: self;
		actionSelector: #fullDown;
		labelGraphic: (self iconNamed: #bottomIcon);
		color: Color transparent;
		extent: 24 @ 24;
		borderWidth: 0