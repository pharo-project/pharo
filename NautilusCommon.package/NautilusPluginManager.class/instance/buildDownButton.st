items creation
buildDownButton

	^ IconicButton new 
		target: self;
		actionSelector: #down;
		labelGraphic: (Smalltalk ui icons iconNamed: #downIcon);
		color: Color transparent;
		extent: 24 @ 24;
		borderWidth: 0