item creation
buildRemoveSelectedPackageFromSelectedGroupButton
	^ IconicButton new
		target: self;
		actionSelector: #removeSelectedPackageFromSelectedGroup;
		labelGraphic: (self iconNamed: #up);
		color: Color transparent;
		helpText: 'Remove the selected package from its group';
		borderWidth: 0;
		yourself