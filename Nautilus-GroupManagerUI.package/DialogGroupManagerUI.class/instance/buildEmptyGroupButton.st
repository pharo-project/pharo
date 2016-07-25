item creation
buildEmptyGroupButton
	^ IconicButton new
		target: self;
		actionSelector: #emptySelectedGroup;
		labelGraphic: (Smalltalk ui icons iconNamed: #delete);
		color: Color transparent;
		helpText: 'Empty the selected group';
		borderWidth: 0;
		yourself