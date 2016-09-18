initialization
initializeWidgets
	
	packageList := self newList
		multiSelection: true;
		yourself.
	
	buttonAddPackage := self newButton
		help: 'Add these packages to the current analysis';
		label: 'Add packages';
		icon: (self iconNamed: #addIcon);
		yourself.