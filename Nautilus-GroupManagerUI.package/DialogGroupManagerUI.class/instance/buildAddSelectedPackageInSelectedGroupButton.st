item creation
buildAddSelectedPackageInSelectedGroupButton
	^ IconicButton new
		target: self;
		actionSelector: #addSelectedPackageInSelectedGroup;
		labelGraphic: (Smalltalk ui icons iconNamed: #down);
		color: Color transparent;
		helpText: 'Add the selected package into the selected group';
		borderWidth: 0;
		yourself