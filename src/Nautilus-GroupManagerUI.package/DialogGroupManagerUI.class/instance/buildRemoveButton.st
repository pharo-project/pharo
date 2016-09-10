item creation
buildRemoveButton
	^ IconicButton new
		target: self;
		actionSelector: #removeAction;
		labelGraphic: (self iconNamed: #removeIcon);
		color: Color transparent;
		helpText: 'Remove the selected group';
		borderWidth: 0;
		yourself