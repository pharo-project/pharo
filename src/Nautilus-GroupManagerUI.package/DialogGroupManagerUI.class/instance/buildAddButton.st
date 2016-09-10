item creation
buildAddButton
	^ IconicButton new
		target: self;
		actionSelector: #addAction;
		labelGraphic: (self iconNamed: #add);
		color: Color transparent;
		helpText: 'Add a new group';
		borderWidth: 0;
		yourself