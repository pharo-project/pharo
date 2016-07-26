item creation
buildRenameButton
	^ IconicButton new
		target: self;
		actionSelector: #renameAction;
		labelGraphic: (Smalltalk ui icons iconNamed: #edit);
		color: Color transparent;
		helpText: 'Rename the selected group';
		borderWidth: 0;
		yourself