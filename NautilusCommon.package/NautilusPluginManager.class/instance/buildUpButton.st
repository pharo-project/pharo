items creation
buildUpButton

	^ IconicButton new 
		target: self;
		actionSelector: #up;
		labelGraphic: (Smalltalk ui icons iconNamed: #upIcon);
		color: Color transparent;
		extent: 24 @ 24;
		borderWidth: 0