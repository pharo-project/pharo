protocol
asIcon

	^ IconicButton new 
		target: self target;
		actionSelector: self actionSelector;
		arguments: self arguments;
		labelGraphic: self labelGraphic;
		color: self color;
		helpText: self helpText;
		extent: self extent;
		borderWidth: self borderWidth