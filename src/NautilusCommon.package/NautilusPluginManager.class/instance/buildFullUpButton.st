items creation
buildFullUpButton

	^ IconicButton new 
		target: self;
		actionSelector: #fullUp;
		labelGraphic: (Smalltalk ui icons iconNamed: #topIcon);
		color: Color transparent;
		extent: 24 @ 24;
		borderWidth: 0