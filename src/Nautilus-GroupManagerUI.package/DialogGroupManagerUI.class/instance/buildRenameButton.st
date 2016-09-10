item creation
buildRenameButton
	^ IconicButton new
		target: self;
		actionSelector: #renameAction;
		labelGraphic: (self iconNamed: #edit);
		color: Color transparent;
		helpText: 'Rename the selected group';
		borderWidth: 0;
		yourself